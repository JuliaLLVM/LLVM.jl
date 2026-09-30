@testset "enumerations" begin

# the scoped names are aliases of the values of the C API
@test LLVM.Linkage.Internal === LLVM.API.LLVMInternalLinkage
@test LLVM.Linkage.T === LLVM.API.LLVMLinkage
@test LLVM.IntPredicate.EQ === LLVM.API.LLVMIntEQ
@test LLVM.RealPredicate.False === LLVM.API.LLVMRealPredicateFalse
@test LLVM.Opcode.BitCast === LLVM.API.LLVMBitCast
@test LLVM.TypeKind.Function === LLVM.API.LLVMFunctionTypeKind
@test LLVM.DLLStorageClass.Import === LLVM.API.LLVMDLLImportStorageClass
@test LLVM.ThreadLocalMode.NotThreadLocal === LLVM.API.LLVMNotThreadLocal
@test LLVM.CodeGenOptLevel.Aggressive === LLVM.API.LLVMCodeGenLevelAggressive
@test LLVM.CodeGenFileType.Assembly === LLVM.API.LLVMAssemblyFile

# including values that the API backfills on older versions of LLVM
@test LLVM.AtomicRMWBinOp.FMinimumNum === LLVM.API.LLVMAtomicRMWBinOpFMinimumNum

for (scope, typename) in ((s[1], s[2]) for s in LLVM.enum_scopes)
    mod = getfield(LLVM, scope)
    T = getfield(LLVM.API, typename)
    @test mod.T === T
    members = filter(n -> n !== :T && n !== scope, names(mod; all=true))
    members = filter(n -> isdefined(mod, n) && getfield(mod, n) isa T, members)
    @test !isempty(members)
    for name in members
        val = getfield(mod, name)
        @test val isa T
        @static if VERSION >= v"1.11"
            @test Base.ispublic(mod, name)
        end
        # values are displayed using a name that evaluates to the value
        str = repr(val)
        @test startswith(str, "LLVM.$scope.")
        @test eval(Meta.parse(str)) === val
        @test string(val) == str
    end
    @static if VERSION >= v"1.11"
        @test Base.ispublic(LLVM, scope)
    end

    # the scopes are not part of a vocabulary
    for vocabulary in (LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC)
        @test scope ∉ names(vocabulary)
    end
end

# values without a name are displayed as a constructor call
@test repr(LLVM.Linkage.T(123)) == "LLVM.Linkage.T(123)"
@test eval(Meta.parse(repr(LLVM.Linkage.T(123)))) === LLVM.API.LLVMLinkage(123)
@test sprint(show, MIME("text/plain"), LLVM.Linkage.Internal) == "LLVM.Linkage.Internal"
@test "$(LLVM.Linkage.Internal)" == "LLVM.Linkage.Internal"

# they can be used with the API
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    gv = GlobalVariable(mod, LLVM.Int32Type(), "gv")
    gv.linkage = LLVM.Linkage.Internal
    @test gv.linkage === LLVM.Linkage.Internal
    f = LLVM.Function(mod, "f", LLVM.FunctionType(LLVM.VoidType()))
    f.callconv = LLVM.CallConv.Fast
    @test f.callconv == LLVM.CallConv.Fast
end
@test parse(LLVM.AtomicOrdering.T, "acquire") === LLVM.AtomicOrdering.Acquire

end
