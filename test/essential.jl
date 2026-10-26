@testset "essentials" begin

@test LLVM.InitializeNativeTarget() === nothing
@test LLVM.InitializeAllTargetInfos() === nothing
@test LLVM.InitializeAllTargetMCs() === nothing
@test LLVM.InitializeNativeAsmPrinter() === nothing

end

@testset "API wrappers" begin

# the wrappers cover every supported version of LLVM, and functions that are not available
# on the version in use throw an error instead of calling into LLVM
@test LLVM.version().major in LLVM.API.WRAPPED_VERSIONS ||
      LLVM.version().major > last(LLVM.API.WRAPPED_VERSIONS)
if LLVM.version() >= v"21"
    @test_throws "LLVMConstMul is not available on LLVM $(LLVM.version().major) (only on LLVM 15 to 20)" LLVM.API.LLVMConstMul(C_NULL, C_NULL)
end
if LLVM.version() < v"23"
    @test_throws "LLVMByteTypeInContext is not available on LLVM $(LLVM.version().major) (only on LLVM 23 and later)" LLVM.API.LLVMByteTypeInContext(C_NULL, 8)
end

# enums contain the members of every version, but members that are not available on the
# version in use are rejected when passed to LLVM
unavailable = LLVM.version() >= v"23" ? LLVM.API.LLVMBr : LLVM.API.LLVMCondBr
@test !LLVM.isavailable(unavailable)
@test LLVM.isavailable(LLVM.API.LLVMRet)
@test_throws ArgumentError LLVM.API.to_abi(LLVM.library(), LLVM.API.LLVMOpcode, unavailable)
@test LLVM.API.to_abi(LLVM.library(), LLVM.API.LLVMOpcode, LLVM.API.LLVMRet) === LLVM.API.LLVMRet
# including members of anonymous enums, which are integer constants
@test LLVM.isavailable(LLVM.API.LLVMGEPFlagInBounds) == (LLVM.version() >= v"19")
# values that are not members are passed as-is (e.g., calling conventions without a name)
@test LLVM.API.to_abi(LLVM.library(), LLVM.API.LLVMCallConv, 71) === LLVM.API.LLVMCallConv(71)

# values of enums that differ between versions are translated, e.g., LLVM 23 inserted a
# value kind before that of floating-point constants
@dispose ctx=Context() begin
    @test LLVM.API.LLVMGetValueKind(ConstantFP(1.0)) === LLVM.API.LLVMConstantFPValueKind
    @test LLVM.API.LLVMGetValueKind(ConstantInt(1)) === LLVM.API.LLVMConstantIntValueKind
    @test ConstantFP(1.0) isa ConstantFP
    @test PointerNull(LLVM.PointerType(LLVM.Int8Type())) isa PointerNull
end

# the translation tables of every version are consistent
for name in names(LLVM.API; all=true)
    T = getfield(LLVM.API, name)
    T isa DataType && T <: LLVM.API.CEnum.Cenum || continue
    for major in LLVM.API.WRAPPED_VERSIONS
        tables = LLVM.API.abi_tables(T, major)
        tables === nothing && continue
        for x in instances(T)
            raw = tables.to[Integer(x)+1]
            raw >= 0 || continue
            canonical = tables.from[raw+1]
            @test tables.to[canonical+1] == raw
        end
    end
end

end
