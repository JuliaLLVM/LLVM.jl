# an application that depends on LLVM.jl without using it in the code that is compiled into a
# trimmed executable (`juliac --trim`), which does not include Julia's LLVM: LLVM.jl's
# initialization still runs, so it has to be verifiable and work without that library
module TrimTest

using LLVM: LLVM

function @main(args::Vector{String})
    println(Core.stdout, "Hello from a trimmed executable!")
    return 0
end

end
