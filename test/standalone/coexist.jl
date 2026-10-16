# use a standalone LLVM library (see deps/build_standalone.jl) through its C API, next to
# the LLVM that Julia and LLVM.jl use, which should keep working.
#
# usage: julia --project=test test/standalone/coexist.jl LIBRARY [global]

using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, Test, InteractiveUtils
using Base.Libc.Libdl

const P = Ptr{Cvoid}
const lib = ARGS[1]
const mode = get(ARGS, 2, "local") == "global" ? RTLD_LAZY | RTLD_GLOBAL :
                                                  RTLD_LAZY | RTLD_DEEPBIND
const handle = dlopen(lib, mode)
sym(name) = dlsym(handle, name)

# use Julia's LLVM, through LLVM.jl and through Julia's code generator
function check_julia_llvm()
    @dispose ctx=Context() mod=LLVM.Module("test") builder=IRBuilder() begin
        ft = LLVM.FunctionType(LLVM.Int64Type(), [LLVM.Int64Type()])
        f = LLVM.Function(mod, "f", ft)
        position!(builder, LLVM.at_end(BasicBlock(f, "entry")))
        ret!(builder, add!(builder, f.parameters[1], ConstantInt(0)))
        @dispose pb=PassBuilder() begin
            add!(pb, FunctionPassManager()) do fpm
                add!(fpm, InstCombinePass())
            end
            run!(pb, mod)
        end
        @test !occursin("add", string(mod))
    end
    g(x) = x * 2
    @test Base.invokelatest(g, 21) == 42
    @test occursin("shl", sprint(code_llvm, g, Tuple{Int}))
end

throw_error(err) = err == C_NULL ||
    error(unsafe_string(ccall(sym(:LLVMGetErrorMessage), Cstring, (P,), err)))

function buffer_data(buf)
    ptr = ccall(sym(:LLVMGetBufferStart), Ptr{UInt8}, (P,), buf)
    len = ccall(sym(:LLVMGetBufferSize), Csize_t, (P,), buf)
    data = copy(unsafe_wrap(Array, ptr, len))
    ccall(sym(:LLVMDisposeMemoryBuffer), Cvoid, (P,), buf)
    return data
end

# a module pass implemented in Julia, called by the standalone LLVM through LLVMExtra
const functions_seen = Ref(0)
function count_functions(mod::P, ::P)::Cint
    f = ccall(sym(:LLVMGetFirstFunction), P, (P,), mod)
    while f != C_NULL
        functions_seen[] += 1
        f = ccall(sym(:LLVMGetNextFunction), P, (P,), f)
    end
    return 0
end

# the version of LLVM, from the name of the library
const major = parse(Int, match(r"-(\d+)\.", basename(lib))[1])

# the functions that the wrappers define for a version of LLVM
function wrapped_functions(major)
    functions = Symbol[]
    for file in ("libLLVM.jl", "libLLVM_extra.jl")
        path = joinpath(@__DIR__, "..", "..", "lib", file)
        for ex in Meta.parseall(read(path, String)).args
            def = Meta.isexpr(ex, :macrocall) ? ex.args[end] : ex
            Meta.isexpr(def, :function) || continue
            name = def.args[1].args[1]
            available = true
            for st in def.args[2].args
                Meta.isexpr(st, :call) && st.args[1] == :check_available || continue
                since, until = st.args[3], get(st.args, 4, typemax(Int))
                available = since <= major < until
            end
            available && push!(functions, name)
        end
    end
    return unique(functions)
end

@testset "standalone LLVM ($(basename(lib)), $(mode & RTLD_GLOBAL != 0 ? "global" : "local"))" begin

check_julia_llvm()

# LLVM's C++ API is not exported (runtests.jl checks the full list of exports)
@test dlsym(handle, :_ZN4llvm11LLVMContextC1Ev; throw_error=false) === nothing

if major >= 16
    version = Ref{Cuint}()
    ccall(sym(:LLVMGetVersion), Cvoid, (Ptr{Cuint}, P, P), version, C_NULL, C_NULL)
    @test version[] == major
end

# every function that the wrappers define for this version of LLVM is exported, except those
# of LLVM's LTO and remarks libraries, and ones that some versions declare but don't define
undefined = Dict(15 => [:LLVMOrcObjectLayerAddObjectFileWithRT],
                 16 => [:LLVMOrcObjectLayerAddObjectFileWithRT],
                 17 => [:LLVMOrcCreateStaticLibrarySearchGeneratorForPath])
unexported = filter(wrapped_functions(major)) do name
    startswith(String(name), "LLVM") && name != :LLVMRemarkVersion &&
        !(name in get(undefined, major, Symbol[])) &&
        dlsym(handle, name; throw_error=false) === nothing
end
@test isempty(unexported)

@test ccall(sym(:LLVMExtraInitializeNativeTarget), Cint, ()) == 0
@test ccall(sym(:LLVMExtraInitializeNativeAsmPrinter), Cint, ()) == 0

ctx = ccall(sym(:LLVMContextCreate), P, ())
ir = """
    define i32 @f(i32 %x) {
      %y = add i32 %x, 0
      %z = mul i32 %y, 2
      ret i32 %z
    }
    """
buf = ccall(sym(:LLVMCreateMemoryBufferWithMemoryRangeCopy), P, (Cstring, Csize_t, Cstring),
            ir, sizeof(ir), "ir")
mod = Ref{P}()
msg = Ref{Cstring}()
@test ccall(sym(:LLVMParseIRInContext), Cint, (P, P, Ptr{P}, Ptr{Cstring}),
            ctx, buf, mod, msg) == 0

triple = ccall(sym(:LLVMGetDefaultTargetTriple), Cstring, ())
target = Ref{P}()
@test ccall(sym(:LLVMGetTargetFromTriple), Cint, (Cstring, Ptr{P}, Ptr{Cstring}),
            triple, target, msg) == 0
tm = ccall(sym(:LLVMCreateTargetMachine), P, (P, Cstring, Cstring, Cstring, Cint, Cint, Cint),
           target[], triple, "", "", 2, 1, 0)

# optimize, with a pass implemented in Julia
ext = ccall(sym(:LLVMCreatePassBuilderExtensions), P, ())
ccall(sym(:LLVMPassBuilderExtensionsRegisterModulePass), Cvoid, (P, Cstring, P, P),
      ext, "count-functions", @cfunction(count_functions, Cint, (P, P)), C_NULL)
opts = ccall(sym(:LLVMCreatePassBuilderOptions), P, ())
throw_error(ccall(sym(:LLVMRunJuliaPasses), P, (P, Cstring, P, P, P),
                  mod[], "default<O2>,count-functions", tm, opts, ext))
@test functions_seen[] == 1
str = ccall(sym(:LLVMPrintModuleToString), Cstring, (P,), mod[])
@test !occursin("add i32", unsafe_string(str))
ccall(sym(:LLVMDisposeMessage), Cvoid, (Cstring,), str)

# generate native code
out = Ref{P}()
@test ccall(sym(:LLVMTargetMachineEmitToMemoryBuffer), Cint, (P, P, Cint, Ptr{Cstring}, Ptr{P}),
            tm, mod[], 0, msg, out) == 0
@test occursin("f", String(buffer_data(out[])))

ccall(sym(:LLVMDisposePassBuilderOptions), Cvoid, (P,), opts)
ccall(sym(:LLVMDisposePassBuilderExtensions), Cvoid, (P,), ext)
ccall(sym(:LLVMDisposeTargetMachine), Cvoid, (P,), tm)
ccall(sym(:LLVMDisposeModule), Cvoid, (P,), mod[])
ccall(sym(:LLVMContextDispose), Cvoid, (P,), ctx)

check_julia_llvm()

end
