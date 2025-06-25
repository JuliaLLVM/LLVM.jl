@testset "essentials" begin

@test InitializeNativeTarget() === nothing
@test InitializeAllTargetInfos() === nothing
@test InitializeAllTargetMCs() === nothing
@test InitializeNativeAsmPrinter() === nothing

end
