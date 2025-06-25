@testset "interop" begin

using LLVM.Interop
using InteractiveUtils

# the tests below use Julia's JIT, so check whether it uses opaque pointers or not.
# note that the default context used in generated functions may behave differently,
# but that does not matter (as typed IR can be linked with opaque IR).
supports_typed_ptrs = let
    ir = sprint(io->code_llvm(io, unsafe_load, Tuple{Ptr{Int}}))
    if occursin(r"load i64, i64\* .+, align 1", ir)
        true
    elseif occursin(r"load i64, ptr .+, align 1", ir)
        false
    else
        error("could not determine whether Julia uses typed pointers")
    end
end

@testset "base" begin

@generated function foo()
    @dispose ctx=Context() begin
        f, ft = create_function()

        @dispose builder=IRBuilder() begin
            entry = BasicBlock(f, "entry")
            position!(builder, entry)

            ret!(builder)
        end

        call_function(f, Nothing, Tuple{})
    end
end
@test foo() === nothing

@generated function bar()
    @dispose ctx=Context() begin
        T_int = convert(LLVMType, Int)

        f, ft = create_function(T_int)

        @dispose builder=IRBuilder() begin
            entry = BasicBlock(f, "entry")
            position!(builder, entry)

            val = ConstantInt(T_int, 42)

            ret!(builder, val)
        end

        call_function(f, Int, Tuple{})
    end
end
@test bar() == 42

@generated function baz(i)
    @dispose ctx=Context() begin
        T_int = convert(LLVMType, Int)

        f, ft = create_function(T_int, [T_int])

        @dispose builder=IRBuilder() begin
            entry = BasicBlock(f, "entry")
            position!(builder, entry)

            val = add!(builder, parameters(f)[1], ConstantInt(T_int, 42))

            ret!(builder, val)
        end

        call_function(f, Int, Tuple{Int}, :i)
    end
end
@test baz(1) == 43

@eval struct GhostType end
@eval struct NonGhostType1
    x::Int
end
@eval mutable struct NonGhostType2 end

@test isboxed(NonGhostType2)
@test isghosttype(GhostType)
@test !isghosttype(NonGhostType1)
@test !isghosttype(NonGhostType2)

@dispose ctx=Context() begin
    @test isboxed(NonGhostType2)
    @test isghosttype(GhostType)
    @test !isghosttype(NonGhostType1)
    @test !isghosttype(NonGhostType2)
end

end

@testset "asmcall" begin

# only asm

a1() = @asmcall("nop")
@test a1() === nothing

a2() = @asmcall("nop", Nothing)
@test a2() === nothing

a3() = @asmcall("nop", Nothing, Tuple{})
@test a3() === nothing

# asm + constraints

b1() = @asmcall("nop", "")
@test b1() === nothing

b2() = @asmcall("nop", "", Nothing)
@test b2() === nothing

b3() = @asmcall("nop", "", Nothing, Tuple{})
@test b3() === nothing

# asm + constraints + side-effects

c1() = @asmcall("nop", "", false)
@test c1() === nothing

c2() = @asmcall("nop", "", false, Nothing)
@test c2() === nothing

c3() = @asmcall("nop", "", false, Nothing, Tuple{})
@test c3() === nothing

if Sys.ARCH == :x86 || Sys.ARCH == :x86_64

# arguments

d1(a) = @asmcall("bswap \$0", "=r,r", Int32, Tuple{Int32}, a)
@test d1(Int32(1)) == Int32(16777216)

# multiple output registers

e1() = @asmcall("mov \$\$1, \$0; mov \$\$2, \$1;", "=r,=r", Tuple{Int16,Int32})
@test e1() == (Int16(1), Int32(2))

# TODO: alternative test snippets for other platforms

end

@testset "macro hygiene" begin
    mod = @eval module $(gensym())
        module Inner
            using LLVM, LLVM.Interop
        end

        foo() = Inner.@asmcall("nop")
    end

    @test mod.foo() === nothing
end

end

function test_module()
    mod = LLVM.Module("test")
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    @dispose builder=IRBuilder() begin
        entry = BasicBlock(fn, "entry")
        position!(builder, entry)

        ret!(builder)
    end

    return mod
end

@testset "passes" begin
    if VERSION < v"1.11.0-DEV.428"
        @dispose ctx=Context() mod=test_module() pm=ModulePassManager() begin

            demote_float16!(pm)
            julia_licm!(pm)
            alloc_opt!(pm)
            barrier_noop!(pm)
            gc_invariant_verifier!(pm)
            gc_invariant_verifier!(pm, true)
            lower_exc_handlers!(pm)
            combine_mul_add!(pm)
            multi_versioning!(pm)
            propagate_julia_addrsp!(pm)
            lower_ptls!(pm)
            lower_ptls!(pm, true)
            lower_simdloop!(pm)
            remove_ni!(pm)
            late_lower_gc_frame!(pm)
            final_lower_gc!(pm)
            cpu_features!(pm)

        end
        @test "we didn't crash!" != ""
    end

    @dispose ctx=Context() mod=test_module() begin
        # by string
        @test run!("julia", mod) === nothing

        # by object
        @test run!(JuliaPipeline(), mod) === nothing

        # by object with options
        pipeline = JuliaPipeline(opt_level=2)
        @test run!(pipeline, mod) === nothing

        if VERSION >= v"1.12.0-DEV.1029" ||         # JuliaLang/julia#55407
           (v"1.11.0-rc3" <= VERSION < v"1.12-")    # backport
            pipeline = JuliaPipeline(opt_level=3, enable_vector_pipeline=false)
            @test run!(pipeline, mod) === nothing
            # TODO: check that the vector pipeline didn't run

            pipeline = JuliaPipeline(enable_early_simplifications=false,
                                     enable_early_optimizations=false)
            @test contains(string(pipeline), "no_enable_early_simplifications")
            @test contains(string(pipeline), "no_enable_early_optimizations")
        end
    end
end

@testset "intrinsics" begin
    @test assume(true) === nothing
    @test assume() do
        true
    end === nothing
    @test assume(rand()) do x
        0 <= x <= 1
    end isa Float64
end

using Core: LLVMPtr

@testset "pointer operations" begin
    a = LLVMPtr{Int,0}(0)
    b = LLVMPtr{Int,0}(1)
    c = LLVMPtr{UInt,0}(0)
    d = LLVMPtr{UInt,0}(1)
    e = LLVMPtr{Int,1}(0)
    f = LLVMPtr{Int,1}(1)

    @test UInt(a) == 0
    @test UInt(f) == 1

    @test isequal(a,a)
    @test !isequal(a,b)
    @test !isequal(a,c)
    @test !isequal(a,d)
    @test !isequal(a,e)
    @test !isequal(a,f)

    @test a == a
    @test a != b
    @test a == c
    @test a != d
    @test a != e
    @test a != f

    @test a < b
    @test a < d
    @test_throws MethodError a < f

    @test b - a == 1
    @test d - a == 1
    @test_throws MethodError  f - a

    @test a + 1 == b
    @test c + 1 == d
    @test e + 1 == f

    @test b - 1 == a
    @test d - 1 == c
    @test f - 1 == e

    # ensure pointer arithmetic doesn't perform addrspacecasts
    ir = sprint(io->code_llvm(io, +, Tuple{typeof(e), Int}; optimize=false, dump_module=true))
    @test !occursin(r"addrspacecast", ir)
    ir = sprint(io->code_llvm(io, -, Tuple{typeof(e), Int}; optimize=false, dump_module=true))
    @test !occursin(r"addrspacecast", ir)
end

@testset "unsafe_load" begin
    a = Int64[1]
    ptr = reinterpret(Core.LLVMPtr{Int64,0}, pointer(a))
    @test unsafe_load(ptr) == 1
    unsafe_store!(ptr, 2)
    @test unsafe_load(ptr) == 2

    ir = sprint(io->code_llvm(io, unsafe_load, Tuple{typeof(ptr)}))
    if Sys.iswindows() && Sys.WORD_SIZE == 32
        # FIXME: Win32 nightly emits a i64*, even though bitstype_to_llvm uses T_int8
        @test_broken contains(ir, r"@julia_unsafe_load_\d+\(i8\*")
    else
        if supports_typed_ptrs
            @test contains(ir, r"@julia_unsafe_load_\d+\(i8\*")
        else
            @test contains(ir, r"@julia_unsafe_load_\d+\(ptr")
        end
    end
    if supports_typed_ptrs
        @test contains(ir, r"load i64, i64\* %.+?, align 1")
    else
        @test contains(ir, r"load i64, ptr %.+?, align 1")
    end
    ir = sprint(io->code_llvm(io, unsafe_load, Tuple{typeof(ptr), Int, Val{4}}))
    if supports_typed_ptrs
        @test contains(ir, r"load i64, i64\* %.+?, align 4")
    else
        @test contains(ir, r"load i64, ptr %.+?, align 4")
    end
end

@testset "reinterpret with addrspacecast" begin
    ptr = reinterpret(Core.LLVMPtr{Int64, 4}, 0)
    for eltype_dest in (Int64, Int32), AS_dest in (4, 3)
        T_dest = Core.LLVMPtr{eltype_dest, AS_dest}
        ir = sprint(io->code_llvm(io, LLVM.Interop.addrspacecast, Tuple{Type{T_dest}, typeof(ptr)}))
        if supports_typed_ptrs
            if AS_dest == 3
                @test contains(ir, r"addrspacecast i8 addrspace\(4\)\* %.+? to i8 addrspace\(3\)\*")
            else
                @test !contains(ir, r"addrspacecast i8 addrspace\(4\)\* %.+? to i8 addrspace\(3\)\*")
            end
        else
            if AS_dest == 3
                @test contains(ir, r"addrspacecast ptr addrspace\(4\) %.+? to ptr addrspace\(3\)")
            else
                @test !contains(ir, r"addrspacecast ptr addrspace\(4\) %.+? to ptr addrspace\(3\)")
            end
        end
    end
end

@testset "reinterpret(Nothing, nothing)" begin
    ptr = reinterpret(Core.LLVMPtr{Nothing,0}, C_NULL)
    @test unsafe_load(ptr) === nothing
end

@testset "TBAA" begin
    load(ptr) = unsafe_load(ptr)
    store(ptr) = unsafe_store!(ptr, 0)

    for f in (load, store)
        ir = sprint(io->code_llvm(io, f,
                                  Tuple{Core.LLVMPtr{Float32,1}};
                                  dump_module=true, raw=true))
        @test occursin("custom_tbaa_addrspace(1)", ir)

        # no TBAA on generic pointers
        ir = sprint(io->code_llvm(io, f,
                                  Tuple{Core.LLVMPtr{Float32,0}};
                                  dump_module=true, raw=true))
        @test !occursin("custom_tbaa", ir)
    end
end

@testset "ghost values" begin
    @eval struct Singleton end

    ir = sprint(io->code_llvm(io, unsafe_load,
                              Tuple{Core.LLVMPtr{Singleton,0}}))
    @test occursin("ret void", ir)
    @test unsafe_load(reinterpret(Core.LLVMPtr{Singleton,0}, C_NULL)) === Singleton()

    ir = sprint(io->code_llvm(io, unsafe_store!,
                              Tuple{Core.LLVMPtr{Singleton,0},
                              Singleton}))
    @test !occursin("\bstore\b", ir)
end

if supports_typed_ptrs
    @testset "type-preserving ccall" begin
        # NOTE: the auto-upgrader will remangle these intrinsics, but only if they were
        #       specified in the first place (`if Name.startswith("ptr.annotation.")`)
        annotated(ptr::Ptr{T}) where {T} = @typed_ccall("llvm.ptr.annotation.p0i64", llvmcall, Ptr{T}, (Ptr{T}, Ptr{Int8}, Ptr{Int8}, Int32), ptr, C_NULL, C_NULL, 0)

        ir = sprint(io->code_llvm(io, annotated, Tuple{Ptr{Float64}}))
        @test occursin("double* @llvm.ptr.annotation.p0f64(double*", ir)

        annotated(ptr::LLVMPtr{T}) where {T} = @typed_ccall("llvm.ptr.annotation.p0i64", llvmcall, LLVMPtr{T,1}, (LLVMPtr{T,1}, Ptr{Int8}, Ptr{Int8}, Int32), ptr, C_NULL, C_NULL, 0)

        ir = sprint(io->code_llvm(io, annotated, Tuple{LLVMPtr{Float64,1}}))
        @test occursin("double addrspace(1)* @llvm.ptr.annotation.p1f64(double addrspace(1)*", ir)

        # test return nothing
        LLVM.Interop.@typed_ccall("llvm.donothing", llvmcall, Cvoid, ())

        # test return Bool
        expect_bool(val, expected_val) = LLVM.Interop.@typed_ccall("llvm.expect.i1", llvmcall, Bool, (Bool,Bool), val, expected_val)
        @test expect_bool(true, false)

        # test return non-special type
        expect_int(val, expected_val) = LLVM.Interop.@typed_ccall("llvm.expect.i64", llvmcall, Int, (Int,Int), val, expected_val)
        @test expect_int(42, 0) == 42

        # test passing constant values
        let
            a = [42]
            b = [0]
            memcpy(dst, src, len) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (Ptr{Int}, Ptr{Int}, Int, Bool), dst, src, len, Val(false))
            memcpy(b, a, 1)
            @test b == [42]

            const_bool_false = memcpy
            ir = sprint(io->code_llvm(io, memcpy, Tuple{Vector{Int}, Vector{Int}, Int}))
            @test occursin(r"call void @llvm.memcpy.p0i64.p0i64.i64\(i64\* .+, i64\* .+, i64 .+, i1 false\)", ir)

            const_bool_true(dst, src, len) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (Ptr{Int}, Ptr{Int}, Int, Bool), dst, src, len, Val(true))
            ir = sprint(io->code_llvm(io, const_bool_true, Tuple{Vector{Int}, Vector{Int}, Int}))
            @test occursin(r"call void @llvm.memcpy.p0i64.p0i64.i64\(i64\* .+, i64\* .+, i64 .+, i1 true\)", ir)

            const_ptrs(len) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (Ptr{Int}, Ptr{Int}, Int, Bool), Val(Ptr{Int}(0)), Val(Ptr{Int}(1)), len, Val(true))
            ir = sprint(io->code_llvm(io, const_ptrs, Tuple{Int}))
            @test occursin(r"call void @llvm.memcpy.p0i64.p0i64.i64\(i64\* null, i64\* inttoptr \(i64 1 to i64\*\), i64 .+, i1 true\)", ir)

            const_llvmptrs(len) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (LLVMPtr{Int,0}, LLVMPtr{Int,0}, Int, Bool), Val(LLVMPtr{Int,0}(0)), Val(LLVMPtr{Int,0}(1)), len, Val(true))
            ir = sprint(io->code_llvm(io, const_llvmptrs, Tuple{Int}))
            @test occursin(r"call void @llvm.memcpy.p0i64.p0i64.i64\(i64\* null, i64\* inttoptr \(i64 1 to i64\*\), i64 .+, i1 true\)", ir)

            const_int(dst, src) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (Ptr{Int}, Ptr{Int}, Int, Bool), dst, src, Val(999), Val(false))
            ir = sprint(io->code_llvm(io, const_int, Tuple{Vector{Int}, Vector{Int}}))
            @test occursin(r"call void @llvm.memcpy.p0i64.p0i64.i64\(i64\* .+, i64\* .+, i64 999, i1 false\)", ir)
        end
    end
else
    @testset "type-preserving ccall" begin
        # NOTE: the auto-upgrader will remangle these intrinsics, but only if they were
        #       specified in the first place (`if Name.startswith("ptr.annotation.")`)
        annotated(ptr::Ptr{T}) where {T} = @typed_ccall("llvm.ptr.annotation.p0", llvmcall, Ptr{T}, (Ptr{T}, Ptr{Int8}, Ptr{Int8}, Int32), ptr, C_NULL, C_NULL, 0)

        ir = sprint(io->code_llvm(io, annotated, Tuple{Ptr{Float64}}))
        if LLVM.version() >= v"16"
            @test occursin("ptr @llvm.ptr.annotation.p0.p0(ptr", ir)
        else
            @test occursin("ptr @llvm.ptr.annotation.p0(ptr", ir)
        end

        annotated(ptr::LLVMPtr{T}) where {T} = @typed_ccall("llvm.ptr.annotation.p0", llvmcall, LLVMPtr{T,1}, (LLVMPtr{T,1}, Ptr{Int8}, Ptr{Int8}, Int32), ptr, C_NULL, C_NULL, 0)

        ir = sprint(io->code_llvm(io, annotated, Tuple{LLVMPtr{Float64,1}}))
        if LLVM.version() >= v"16"
            @test occursin("ptr addrspace(1) @llvm.ptr.annotation.p1.p0(ptr addrspace(1)", ir)
        else
            @test occursin("ptr addrspace(1) @llvm.ptr.annotation.p1(ptr addrspace(1)", ir)
        end

        # test return nothing
        LLVM.Interop.@typed_ccall("llvm.donothing", llvmcall, Cvoid, ())

        # test return Bool
        expect_bool(val, expected_val) = LLVM.Interop.@typed_ccall("llvm.expect.i1", llvmcall, Bool, (Bool,Bool), val, expected_val)
        @test expect_bool(true, false)

        # test return non-special type
        expect_int(val, expected_val) = LLVM.Interop.@typed_ccall("llvm.expect.i64", llvmcall, Int, (Int,Int), val, expected_val)
        @test expect_int(42, 0) == 42

        # test passing constant values
        let
            a = [42]
            b = [0]
            memcpy(dst, src, len) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (Ptr{Int}, Ptr{Int}, Int, Bool), dst, src, len, Val(false))
            memcpy(b, a, 1)
            @test b == [42]

            const_bool_false = memcpy
            ir = sprint(io->code_llvm(io, memcpy, Tuple{Vector{Int}, Vector{Int}, Int}))
            @test occursin(r"call void @llvm.memcpy.p0.p0.i64\(ptr .+, ptr .+, i64 .+, i1 false\)", ir)

            const_bool_true(dst, src, len) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (Ptr{Int}, Ptr{Int}, Int, Bool), dst, src, len, Val(true))
            ir = sprint(io->code_llvm(io, const_bool_true, Tuple{Vector{Int}, Vector{Int}, Int}))
            @test occursin(r"call void @llvm.memcpy.p0.p0.i64\(ptr .+, ptr .+, i64 .+, i1 true\)", ir)

            const_ptrs(len) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (Ptr{Int}, Ptr{Int}, Int, Bool), Val(Ptr{Int}(0)), Val(Ptr{Int}(1)), len, Val(true))
            ir = sprint(io->code_llvm(io, const_ptrs, Tuple{Int}))
            @test occursin(r"call void @llvm.memcpy.p0.p0.i64\(ptr null, ptr inttoptr \(i64 1 to ptr\), i64 .+, i1 true\)", ir)

            const_llvmptrs(len) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (LLVMPtr{Int,0}, LLVMPtr{Int,0}, Int, Bool), Val(LLVMPtr{Int,0}(0)), Val(LLVMPtr{Int,0}(1)), len, Val(true))
            ir = sprint(io->code_llvm(io, const_llvmptrs, Tuple{Int}))
            @test occursin(r"call void @llvm.memcpy.p0.p0.i64\(ptr null, ptr inttoptr \(i64 1 to ptr\), i64 .+, i1 true\)", ir)

            const_int(dst, src) = LLVM.Interop.@typed_ccall("llvm.memcpy.p0.p0.i64", llvmcall, Cvoid, (Ptr{Int}, Ptr{Int}, Int, Bool), dst, src, Val(999), Val(false))
            ir = sprint(io->code_llvm(io, const_int, Tuple{Vector{Int}, Vector{Int}}))
            @test occursin(r"call void @llvm.memcpy.p0.p0.i64\(ptr .+, ptr .+, i64 999, i1 false\)", ir)
        end
    end
end

end
