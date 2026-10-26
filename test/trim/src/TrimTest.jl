# an application that depends on LLVM.jl, compiled into a trimmed executable (`juliac
# --trim`), which does not include Julia's LLVM. LLVM.jl's initialization still runs, so it
# has to be verifiable and work without that library, and functions whose IR LLVM.jl
# generates are compiled into the executable.
module TrimTest

using LLVM: LLVM
using LLVM.Build: add!
using LLVM.Interop: @llvmgenerated

# a function that LLVM.jl does not precompile, so its IR is generated when compiling the
# executable
@llvmgenerated builder function add(x::T, y::T)::T where {T<:Integer}
    add!(builder, x, y)
end

function @main(args::Vector{String})
    n = length(args) + 40
    add(n, 2) == 42 || return 1
    add(UInt16(n), UInt16(2)) == 0x002a || return 1

    buf = Int[1, 2, 3]
    GC.@preserve buf begin
        ptr = reinterpret(Core.LLVMPtr{Int,0}, pointer(buf))
        unsafe_store!(ptr, unsafe_load(ptr, 2) + 40, 3)
    end
    buf[3] == 42 || return 1

    println(Core.stdout, "Hello from a trimmed executable!")
    return 0
end

end
