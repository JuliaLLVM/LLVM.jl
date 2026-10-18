## target triples

@public Triple, isx86, isaarch64, isarm, isthumb, isriscv, isppc, isnvptx, isamdgpu,
        isspirv, iswasm, isdarwin, iswindows, islinux

"""
    LLVM.Triple(str::AbstractString)

A target triple, like `x86_64-unknown-linux-gnu`, interpreted by the loaded LLVM library
like C++'s `llvm::Triple`. The string is used as it is, like C++ passes do with
`Triple(M.getTargetTriple())`: spellings that LLVM only understands after normalization,
like the OS of `x86_64-w64-mingw32`, are not recognized. Use [`normalize`](@ref
LLVM.normalize(::LLVM.Triple)) to get a normalized triple.

Two triples are equal if they are spelled the same. `string(triple)` returns the string.

# Properties

    triple.arch
    triple.vendor
    triple.os
    triple.environment
    triple.object_format

The architecture, vendor, operating system, environment and object file format that LLVM
derives from the triple, as symbols named like LLVM does (e.g., `:x86_64`, `:i386`,
`:aarch64`, `:apple`, `:linux`, `:windows`, `:gnu`, `:elf`, `:macho`, `:coff`), or
`:unknown`. The names come from the loaded LLVM library, so other versions of LLVM may
know about more of them, or name them differently (e.g., the architecture of
`amdgcn-amd-amdhsa` is `:amdgcn` before LLVM 23, and `:amdgpu` since). The predicates
below don't depend on such names. The object format defaults to the one of the operating system
when the triple doesn't specify it, so `triple.object_format == :elf` is C++'s
`isOSBinFormatELF()`.

See also: [`isx86`](@ref), [`isaarch64`](@ref), [`isarm`](@ref), [`isthumb`](@ref),
[`isriscv`](@ref), [`isppc`](@ref), [`isnvptx`](@ref), [`isamdgpu`](@ref),
[`isspirv`](@ref), [`iswasm`](@ref), [`isdarwin`](@ref), [`iswindows`](@ref),
[`islinux`](@ref).
"""
struct Triple
    str::String
    Triple(str::AbstractString) = new(String(str))
end
@properties Triple

Base.string(triple::Triple) = getfield(triple, :str)
Base.show(io::IO, triple::Triple) = print(io, "LLVM.Triple(", repr(string(triple)), ")")
Base.:(==)(a::Triple, b::Triple) = string(a) == string(b)
Base.hash(triple::Triple, h::UInt) = hash(string(triple), hash(Triple, h))

"""
    normalize(triple::LLVM.Triple)

Normalize the given target triple, e.g., turning `x86_64-w64-mingw32` into
`x86_64-w64-windows-gnu`.
"""
normalize(triple::Triple) = Triple(normalize(string(triple)))

function triple_component(name::Cstring)
    str = unsafe_string(name)
    return isempty(str) ? :unknown : Symbol(str)
end

arch(triple::Triple) = triple_component(API.LLVMExtraTripleGetArchTypeName(string(triple)))
vendor(triple::Triple) =
    triple_component(API.LLVMExtraTripleGetVendorTypeName(string(triple)))
os(triple::Triple) = triple_component(API.LLVMExtraTripleGetOSTypeName(string(triple)))
environment(triple::Triple) =
    triple_component(API.LLVMExtraTripleGetEnvironmentTypeName(string(triple)))
object_format(triple::Triple) =
    triple_component(API.LLVMExtraTripleGetObjectFormatTypeName(string(triple)))

@property Triple arch
@property Triple vendor
@property Triple os
@property Triple environment
@property Triple object_format

for (jl_fname, api_fname, description) in [
        (:isx86, :IsX86, "32- or 64-bit x86 (C++'s `isX86`)"),
        (:isaarch64, :IsAArch64, "AArch64, in any endianness or pointer size (C++'s `isAArch64`)"),
        (:isarm, :IsARM, "32-bit ARM, not Thumb (C++'s `isARM`)"),
        (:isthumb, :IsThumb, "Thumb (C++'s `isThumb`)"),
        (:isriscv, :IsRISCV, "32- or 64-bit RISC-V (C++'s `isRISCV`)"),
        (:isppc, :IsPPC, "32- or 64-bit PowerPC, in any endianness (C++'s `isPPC`)"),
        (:isnvptx, :IsNVPTX, "NVPTX (C++'s `isNVPTX`)"),
        (:isamdgpu, :IsAMDGPU, "AMDGPU, i.e., AMDGCN or R600 (C++'s `isAMDGPU`)"),
        (:isspirv, :IsSPIRV, "SPIR-V (C++'s `isSPIRV`)"),
        (:iswasm, :IsWasm, "WebAssembly (C++'s `isWasm`)"),
        (:isdarwin, :IsOSDarwin, "a Darwin operating system, like macOS or iOS (C++'s `isOSDarwin`)"),
        (:iswindows, :IsOSWindows, "Windows (C++'s `isOSWindows`)"),
        (:islinux, :IsOSLinux, "Linux (C++'s `isOSLinux`)")]
    api_fname = Symbol(:LLVMExtraTriple, api_fname)
    doc = """
        LLVM.$jl_fname(triple::LLVM.Triple)

    Check whether the target triple is for $description, according to the loaded LLVM
    library.
    """
    @eval begin
        $jl_fname(triple::Triple) = API.$api_fname(string(triple)) |> Bool
        @doc $doc $jl_fname
    end
end
