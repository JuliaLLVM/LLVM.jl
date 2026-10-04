using Documenter, LLVM
using LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.Analysis, LLVM.ORC

function main()
    ci = get(ENV, "CI", "") == "true"

    # the instruction types depend on the version of LLVM, and some of them are also
    # aliased (e.g., as a union of instructions that share a property), so list them
    # explicitly instead of using `@autodocs`
    open(joinpath(@__DIR__, "src", "lib", "instruction-types.md"), "w") do io
        println(io, "# Instruction types\n")
        println(io, "The types of the instructions that LLVM supports.\n")
        println(io, "```@docs")
        for op in LLVM.opcodes
            println(io, "LLVM.", op, "Inst")
        end
        println(io, "```")
    end

    makedocs(
        sitename = "LLVM.jl",
        authors = "Tim Besard",
        format = Documenter.HTML(
            # Use clean URLs on CI
            prettyurls = ci,
            # one docstring per pass
            size_threshold_ignore = ["lib/passes.md"],
        ),
        modules = [LLVM],
        checkdocs_ignored_modules = [LLVM.API],
        pages = [
            "Home"    => "index.md",
            "Usage"  => [
                "man/essentials.md",
                "man/types.md",
                "man/values.md",
                "man/modules.md",
                "man/functions.md",
                "man/blocks.md",
                "man/instructions.md",
                "man/metadata.md",
                "man/analyses.md",
                "man/transforms.md",
                "man/codegen.md",
                "man/execution.md",
                "man/interop.md",
            ],
            "API reference" => [
                "lib/essentials.md",
                "lib/enums.md",
                "lib/types.md",
                "lib/values.md",
                "lib/modules.md",
                "lib/functions.md",
                "lib/blocks.md",
                "lib/instructions.md",
                "lib/instruction-types.md",
                "lib/metadata.md",
                "lib/analyses.md",
                "lib/transforms.md",
                "lib/passes.md",
                "lib/codegen.md",
                "lib/execution.md",
                "lib/interop.md",
            ]
        ],
        doctest = true,
        doctestfilters = [
            r"0x[0-9a-f]+",     # pointer values
            r"@julia_\w+_\d+",  # function names in generated code
            r"(?s)\nStacktrace:.*",  # stack traces (path- and version-dependent)
        ]
    )

    if ci
        deploydocs(
            repo = "github.com/JuliaLLVM/LLVM.jl.git"
        )
    end
end

isinteractive() || main()
