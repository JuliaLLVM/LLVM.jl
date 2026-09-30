@testset "essentials" begin

@test LLVM.InitializeNativeTarget() === nothing
@test LLVM.InitializeAllTargetInfos() === nothing
@test LLVM.InitializeAllTargetMCs() === nothing
@test LLVM.InitializeNativeAsmPrinter() === nothing

end
