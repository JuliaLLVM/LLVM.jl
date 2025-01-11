# LLVM C API wrapper

*A Julia wrapper for the LLVM C API.*

[![][docs-stable-img]][docs-stable-url] [![][docs-dev-img]][docs-dev-url] [![][github-img]][github-url] [![PkgEval][pkgeval-img]][pkgeval-url] [![][codecov-img]][codecov-url]

[docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[docs-stable-url]: http://JuliaLLVM.github.io/LLVM.jl/stable

[docs-dev-img]: https://img.shields.io/badge/docs-dev-blue.svg
[docs-dev-url]: http://JuliaLLVM.github.io/LLVM.jl/dev

[github-img]: https://github.com/JuliaLLVM/LLVM.jl/actions/workflows/ci.yml/badge.svg
[github-url]: https://github.com/JuliaLLVM/LLVM.jl/actions/workflows/ci.yml

[pkgeval-img]: https://juliaci.github.io/NanosoldierReports/pkgeval_badges/L/LLVM.svg
[pkgeval-url]: https://juliaci.github.io/NanosoldierReports/pkgeval_badges/L/LLVM.html

[codecov-img]: https://codecov.io/gh/JuliaLLVM/LLVM.jl/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/JuliaLLVM/LLVM.jl

The LLVM.jl package is a Julia wrapper for the LLVM C API, and can be used to work with the
LLVM compiler framework from Julia. You can use the package to work with LLVM code generated
by Julia, to interoperate with the Julia compiler, or to create your own compiler. It is
heavily used by the different GPU compilers for the Julia programming language.


## Requirements

LLVM.jl is supported on Julia 1.10+, and thus requires LLVM 15. However, the package is
really only **intended to be used with the LLVM library shipped with Julia**. That means you
can not use it with other LLVM libraries, like the one provided by your operating system. It
is recommended to use the official binaries from
[julialang.org](https://julialang.org/downloads/), but custom builds are supported too (as
long as they provide a dynamically-linked copy of the LLVM library).


## Installation

LLVM.jl can be installed with the Julia package manager.
From the Julia REPL, type `]` to enter the Pkg REPL mode and run:

```
pkg> add LLVM
```

Or, equivalently, via the `Pkg` API:

```julia
julia> import Pkg; Pkg.add("LLVM")
```
