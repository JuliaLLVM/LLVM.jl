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

# discover supported back-ends and their components by looking at available symbols.
# this reimplements LLVM macros and `static inline` functions that are hard to call.
#
# this runs when the package is defined, so it avoids closures (like a `do` block), which
# would compile the Base functions they're passed to and store that code in the image.
let library = Libdl.dlopen(libllvm)
    supported_backends = Symbol[]
    for backend in libllvm_backends
        initializer = "LLVMInitialize$(backend)Target"
        if Libdl.dlsym(library, initializer; throw_error=false) !== nothing
            push!(supported_backends, backend)
        end
    end
    @eval begin
        """
            backends()

        Return a list of back-ends supported by the LLVM library.
        """
        backends() = $supported_backends
    end

    # generate subsystem initialization routines for every back-end
    supported_components = Dict{Symbol,Vector{Symbol}}()
    for component in libllvm_components
        supported_components[component] = Symbol[]
    end
    for backend in libllvm_backends
        backend_supported = backend in supported_backends

        for component in libllvm_components
            jl_fname = Symbol(:Initialize, backend, component)
            jl_all_fname = Symbol(:Initialize, :All, component, :s)
            @eval begin
                @public $jl_fname
                @doc (@doc $jl_all_fname) $jl_fname
            end

            api_fname = Symbol(:LLVM, jl_fname)
            if backend_supported
                component_supported = Libdl.dlsym(library, api_fname; throw_error=false) !== nothing
                if component_supported
                    push!(supported_components[component], backend)
                    @eval $jl_fname() = ccall(($(QuoteNode(api_fname)),libllvm), Cvoid, ())
                else
                    @eval $jl_fname() = error($"The $backend back-end does not contain a $component component.")
                end
            else
                @eval $jl_fname() = error($"The $backend back-end is not part of your LLVM library.")
            end
        end
    end

    # same, for initializing subsystems for all back-ends at once
    for component in keys(supported_components)
        jl_fname = Symbol(:Initialize, :All, component, :s)
        exprs = Expr[]
        for backend in supported_components[component]
            fname = Symbol(:Initialize, backend, component)
            push!(exprs, :($fname()))
        end

        @eval begin
            @public $jl_fname
            function $jl_fname()
                $(exprs...)
                return
            end
        end
    end

    Libdl.dlclose(library)
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
