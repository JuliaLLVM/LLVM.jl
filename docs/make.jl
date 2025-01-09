using Documenter, LLVM

function main()
    ci = get(ENV, "CI", "") == "true"

    makedocs(
        sitename = "LLVM.jl",
        authors = "Tim Besard",
        format = Documenter.HTML(
            # Use clean URLs on CI
            prettyurls = ci
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
                "lib/types.md",
                "lib/values.md",
                "lib/modules.md",
                "lib/functions.md",
                "lib/blocks.md",
                "lib/instructions.md",
                "lib/metadata.md",
                "lib/analyses.md",
                "lib/transforms.md",
                "lib/codegen.md",
                "lib/execution.md",
                "lib/interop.md",
            ]
        ],
        doctest = true,
        doctestfilters = [
            r"0x[0-9a-f]+",     # pointer values
            r"@julia_\w+_\d+"   # function names in generated code
        ]
    )

    if ci
        deploydocs(
            repo = "github.com/JuliaLLVM/LLVM.jl.git"
        )
    end
end

isinteractive() || main()
