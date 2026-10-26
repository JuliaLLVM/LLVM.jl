## subsystem initialization

@public ismultithreaded

"""
    ismultithreaded()

Check whether LLVM is executing in thread-safe mode or not.
"""
ismultithreaded() = API.LLVMIsMultithreaded() |> Bool


## back-end initialization

@public backends

@doc """
    LLVM.InitializeAllTargetInfos()
	LLVM.InitializeXXXTargetInfo()

Enables access to specific targets.
""" InitializeAllTargetInfos

@doc """
	LLVM.InitializeAllTargets()
	LLVM.InitializeXXXTarget()
	LLVM.InitializeNativeTarget()

Enables use of specific targets.
""" InitializeAllTargets

@doc """
	LLVM.InitializeAllTargetMCs()
	LLVM.InitializeXXXTargetMC()

Enable use of machine code generation for specific targets.
""" InitializeAllTargetMCs

@doc """
	LLVM.InitializeAllAsmPrinters()
	LLVM.InitializeXXXAsmPrinter()
	LLVM.InitializeNativeAsmPrinter()

Enables use of assembly output functionality for specific targets.
""" InitializeAllAsmPrinters

@doc """
	LLVM.InitializeAllAsmParsers()
	LLVM.InitializeXXXAsmParser()
	LLVM.InitializeNativeAsmParser()

Enables use of assembly parsing functionality for specific targets.
""" InitializeAllAsmParsers

@doc """
	LLVM.InitializeAllDisassemblers()
	LLVM.InitializeXXXDisassembler()
	LLVM.InitializeNativeDisassembler()

Enables use of disassembly functionality for specific targets.
""" InitializeAllDisassemblers

const libllvm_backends = [:AArch64, :AMDGPU, :ARC, :ARM, :AVR, :BPF, :Hexagon, :Lanai,
                          :MSP430, :Mips, :NVPTX, :PowerPC, :RISCV, :Sparc, :SystemZ,
                          :VE, :WebAssembly, :X86, :XCore,
                          # Unofficial backends
                          :Colossus,
]
const libllvm_components = [:Target, :TargetInfo, :TargetMC, :AsmPrinter, :AsmParser, :Disassembler]

# the back-ends that a library supports, and their components, are discovered by looking
# for the functions that initialize them. this reimplements LLVM macros and `static inline`
# functions that are hard to call.
function initializer(lib::Library, backend::Symbol, component::Symbol)
    check_loaded(lib)
    Libdl.dlsym(lib.handle, Symbol(:LLVMInitialize, backend, component); throw_error=false)
end

"""
    backends()

Return a list of back-ends supported by the current LLVM library.
"""
backends() = filter(backend -> initializer(library(), backend, :Target) !== nothing,
                    libllvm_backends)

function initialize(backend::Symbol, component::Symbol)
    lib = library()
    fn = initializer(lib, backend, component)
    if fn === nothing
        if initializer(lib, backend, :Target) === nothing
            error("The $backend back-end is not part of LLVM library :$(lib.name).")
        else
            error("The $backend back-end does not contain a $component component.")
        end
    end
    ccall(fn, Cvoid, ())
end

# generate subsystem initialization routines for every back-end
for backend in libllvm_backends, component in libllvm_components
    jl_fname = Symbol(:Initialize, backend, component)
    jl_all_fname = Symbol(:Initialize, :All, component, :s)
    @eval begin
        @public $jl_fname
        @doc (@doc $jl_all_fname) $jl_fname
        $jl_fname() = initialize($(QuoteNode(backend)), $(QuoteNode(component)))
    end
end

# same, for initializing subsystems for all back-ends at once
for component in libllvm_components
    jl_fname = Symbol(:Initialize, :All, component, :s)
    @eval begin
        @public $jl_fname
        function $jl_fname()
            lib = library()
            for backend in libllvm_backends
                fn = initializer(lib, backend, $(QuoteNode(component)))
                fn === nothing || ccall(fn, Cvoid, ())
            end
            return
        end
    end
end

# same, for the native back-end
for component in [:Target, :AsmPrinter, :AsmParser, :Disassembler]
    jl_fname = Symbol(:Initialize, :Native, component)
    jl_all_fname = Symbol(:Initialize, :All, component, :s)
    api_fname = Symbol(:LLVMExtra, jl_fname)
    @eval begin
        @public $jl_fname
        @doc (@doc $jl_all_fname) $jl_fname
        function $jl_fname()
            if Bool(API.$api_fname())
               throw(LLVMException("No native target available."))
            end
        end
    end
end
