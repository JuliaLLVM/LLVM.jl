@testset "atomics" begin

O = LLVM.API
NA, UN, MO = O.LLVMAtomicOrderingNotAtomic, O.LLVMAtomicOrderingUnordered,
             O.LLVMAtomicOrderingMonotonic
AC, RE, AR = O.LLVMAtomicOrderingAcquire, O.LLVMAtomicOrderingRelease,
             O.LLVMAtomicOrderingAcquireRelease
SC = O.LLVMAtomicOrderingSequentiallyConsistent

@testset "orderings" begin
    @test is_stronger(SC, AR) && is_stronger(AR, AC) && is_stronger(AR, RE)
    @test !is_stronger(AC, RE) && !is_stronger(RE, AC) && !is_stronger(MO, MO)
    @test is_stronger(MO, UN) && is_stronger(UN, NA)
    @test is_acquire_or_stronger(AC) && is_acquire_or_stronger(SC) && !is_acquire_or_stronger(RE)
    @test is_release_or_stronger(RE) && is_release_or_stronger(AR) && !is_release_or_stronger(AC)
    @test merged_ordering(AC, RE) == AR
    @test merged_ordering(RE, AC) == AR
    @test merged_ordering(MO, AC) == AC
    @test merged_ordering(SC, RE) == SC
    @test strongest_failure_ordering(RE) == MO
    @test strongest_failure_ordering(AR) == AC
    @test strongest_failure_ordering(SC) == SC
    @test_throws ArgumentError strongest_failure_ordering(UN)

    @test parse(O.LLVMAtomicOrdering, "acq_rel") == AR
    @test parse(O.LLVMAtomicOrdering, "acquire_release") == AR
    @test parse(O.LLVMAtomicOrdering, "seq_cst") == SC
    @test parse(O.LLVMAtomicOrdering, "sequentially_consistent") == SC
    @test_throws ArgumentError parse(O.LLVMAtomicOrdering, "consume")
    @test parse(O.LLVMAtomicRMWBinOp, "add") == O.LLVMAtomicRMWBinOpAdd
    @test parse(O.LLVMAtomicRMWBinOp, "fminimum") == O.LLVMAtomicRMWBinOpFMinimum
    @test_throws ArgumentError parse(O.LLVMAtomicRMWBinOp, "mul")
end

@testset "builders" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("atomics") begin
    T_int = LLVM.Int32Type()
    T_float = LLVM.FloatType()
    T_ptr = LLVM.PointerType(T_int)
    ptr_str = supports_typed_pointers(ctx) ? "i32\\* %0" : "ptr %0"
    f = LLVM.Function(mod, "f", LLVM.FunctionType(LLVM.VoidType(), [T_ptr, T_int, T_float]))
    ptr, int, float = f.parameters
    position!(builder, LLVM.at_end(BasicBlock(f, "entry")))

    ld = load!(builder, T_int, ptr; ordering=AC, scope="agent", align=8, volatile=true)
    @test occursin(Regex("load atomic volatile i32, $ptr_str syncscope\\(\"agent\"\\) acquire, align 8"),
                   string(ld))
    @test ld.ordering == AC && ld.syncscope.name == "agent" && ld.volatile
    @test !isatomic(load!(builder, T_int, ptr))

    st = store!(builder, int, ptr; ordering=RE, align=4)
    @test occursin(Regex("store atomic i32 %1, $ptr_str release, align 4"), string(st))

    fn = fence!(builder, AR; scope="workgroup")
    @test occursin("fence syncscope(\"workgroup\") acq_rel", string(fn))
    @test fn.ordering == AR
    fn.ordering = SC
    @test fn.ordering == SC

    rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpAdd, ptr, int, MO; align=16, volatile=true)
    @test occursin(Regex("atomicrmw volatile add $ptr_str, i32 %1 monotonic, align 16"), string(rmw))
    rmw.ordering = AC
    @test rmw.ordering == AC
    @test_throws "at least monotonic" rmw.ordering = UN
    @test_throws "Fences must have" fn.ordering = MO

    cx = atomic_cmpxchg!(builder, ptr, int, int, AR; scope="agent", weak=true)
    @test occursin(Regex("cmpxchg weak $ptr_str, i32 %1, i32 %1 syncscope\\(\"agent\"\\) acq_rel acquire"),
                   string(cx))
    @test merged_ordering(cx) == AR
    @test_throws "no property `ordering`" cx.ordering
    @test_throws "no property `ordering`" cx.ordering = SC
    cx2 = atomic_cmpxchg!(builder, ptr, int, int, RE, AC)
    @test cx2.success_ordering == RE && cx2.failure_ordering == AC
    @test atomic_cmpxchg!(builder, ptr, int, int, SC).failure_ordering == SC

    ret!(builder)
    @test verify(mod) === nothing

    # invalid IR is rejected before building it
    position!(builder, LLVM.at_end(BasicBlock(f, "invalid")))
    @test_throws "release semantics" load!(builder, T_int, ptr; ordering=RE)
    @test_throws "synchronization scope" load!(builder, T_int, ptr; scope="agent")
    @test_throws "acquire semantics" store!(builder, int, ptr; ordering=AC)
    @test_throws "power-of-two number of bytes" load!(builder, LLVM.IntType(7), ptr; ordering=MO)
    @test_throws "power of 2" load!(builder, T_int, ptr; align=3)
    @test_throws "Fences must have" fence!(builder, MO)
    @test_throws "Fences must have" fence!(builder, MO, SyncScope("agent"))
    @test_throws "at least monotonic" atomic_rmw!(builder, O.LLVMAtomicRMWBinOpAdd, ptr, int, UN)
    @test_throws "floating-point value" atomic_rmw!(builder, O.LLVMAtomicRMWBinOpFAdd, ptr, int, MO)
    @test_throws "integer value" atomic_rmw!(builder, O.LLVMAtomicRMWBinOpAdd, ptr, float, MO)
    @test_throws "integer or pointer values" atomic_cmpxchg!(builder, ptr, float, float, SC)
    @test_throws "same type" atomic_cmpxchg!(builder, ptr, int, float, SC)
    @test_throws "release or acq_rel" atomic_cmpxchg!(builder, ptr, int, int, SC, RE)
    @test isempty(builder.insert_block.instructions)
end
end

@testset "metadata" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("atomics") begin
    T_int = LLVM.Int32Type()
    f = LLVM.Function(mod, "f", LLVM.FunctionType(T_int, [LLVM.PointerType(T_int), T_int]))
    ptr, int = f.parameters
    position!(builder, LLVM.at_end(BasicBlock(f, "entry")))

    rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpAdd, ptr, int, MO)
    mmra!(rmw, "amdgpu-as" => "local")
    tag = rmw.metadata["mmra"]
    @test length(tag.operands) == 2
    mmra!(rmw, "amdgpu-as" => "local", "amdgpu-as" => "global")
    @test length(rmw.metadata["mmra"].operands) == 2
    @test all(op -> op isa MDNode, rmw.metadata["mmra"].operands)
    mmra!(rmw)
    @test !haskey(rmw.metadata, "mmra")

    mmra!(rmw, "amdgpu-as" => "local")
    rmw.metadata["amdgpu.no.fine.grained.memory"] = MDNode(Metadata[])
    rmw.metadata[LLVM.MD_range] = MDNode([ConstantInt(Int32(0)), ConstantInt(Int32(10))])
    cx = atomic_cmpxchg!(builder, ptr, int, int, MO)
    copy_atomic_metadata!(cx, rmw)
    @test haskey(cx.metadata, "mmra")
    @test haskey(cx.metadata, "amdgpu.no.fine.grained.memory")
    @test !haskey(cx.metadata, LLVM.MD_range)

    ret!(builder, rmw)
end
end

@testset "expansion" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("expansion") begin
    T_i8, T_i32, T_float = LLVM.Int8Type(), LLVM.Int32Type(), LLVM.FloatType()
    function newfun(name, T)
        f = LLVM.Function(mod, name, LLVM.FunctionType(T, [LLVM.PointerType(T), T]))
        position!(builder, LLVM.at_end(BasicBlock(f, "entry")))
        return f, f.parameters...
    end

    # computing the values of atomic operations
    f, ptr, val = newfun("value", T_i32)
    ret!(builder, atomic_rmw_value!(builder, O.LLVMAtomicRMWBinOpSub, val, val))
    @test occursin("sub i32", string(f))
    f, ptr, val = newfun("fvalue", T_float)
    ret!(builder, atomic_rmw_value!(builder, O.LLVMAtomicRMWBinOpFMax, val, val))
    @test occursin("llvm.maxnum", string(f))
    if LLVM.isavailable(O.LLVMAtomicRMWBinOpUIncWrap)
        f, ptr, val = newfun("uinc", T_i32)
        ret!(builder, atomic_rmw_value!(builder, O.LLVMAtomicRMWBinOpUIncWrap, val, val))
    end
    f, ptr, val = newfun("cas", T_i32)
    loaded, success = atomic_cmpxchg_value!(builder, ptr, val, val; align=4)
    @test success.value_type == LLVM.Int1Type()
    ret!(builder, loaded)
    @test occursin("select", string(f))

    # lowering to non-atomic code
    f, ptr, val = newfun("lower_rmw", T_i32)
    rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpAdd, ptr, val, SC)
    ret!(builder, rmw)
    @test lower_atomic!(rmw)
    @test !occursin("= atomicrmw", string(f))
    f, ptr, val = newfun("lower_aligned", T_i32)
    rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpAdd, ptr, val, SC; align=2, volatile=true)
    ret!(builder, rmw)
    @test lower_atomic!(rmw)
    @test occursin(r"load volatile i32, .*, align 2", string(f))
    @test occursin(r"store volatile i32 .*, align 2", string(f))
    f, ptr, val = newfun("lower_cmpxchg", T_i32)
    cx = atomic_cmpxchg!(builder, ptr, val, val, SC)
    ret!(builder, extract_value!(builder, cx, 0))
    @test lower_atomic!(cx)
    @test !occursin("= cmpxchg", string(f))
    f, ptr, val = newfun("lower_volatile_cmpxchg", T_i32)
    cx = atomic_cmpxchg!(builder, ptr, val, val, SC; volatile=true)
    ret!(builder, extract_value!(builder, cx, 0))
    @test lower_atomic!(cx)
    @test occursin(r"br i1 .*, label %cmpxchg.store, label %cmpxchg.end", string(f))
    @test !occursin("select", string(f))

    # expanding to a cmpxchg loop
    f, ptr, val = newfun("expand", T_float)
    rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpFAdd, ptr, val, AR; scope="agent")
    mmra!(rmw, "amdgpu-as" => "global")
    ret!(builder, rmw)
    @test expand_to_cmpxchg!(rmw)
    ir = string(f)
    @test occursin(r"load atomic float, .* syncscope\(\"agent\"\) monotonic", ir)
    @test occursin(r"cmpxchg .* i32 .* syncscope\(\"agent\"\) acq_rel acquire", ir)
    @test !occursin("= atomicrmw", ir)
    LLVM.version() >= v"19" && @test occursin(r"cmpxchg .*!mmra", ir)
    if LLVM.version() >= v"20"   # vector fadd
        T_vec = LLVM.VectorType(T_float, 2)
        f, ptr, val = newfun("expand_vector", T_vec)
        rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpFAdd, ptr, val, MO)
        ret!(builder, rmw)
        @test expand_to_cmpxchg!(rmw)
        @test occursin(r"load atomic (i64|<2 x float>)", string(f))
    end

    # expanding partword atomics
    f, ptr, val = newfun("partword_add", T_i8)
    rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpAdd, ptr, val, MO; align=1)
    ret!(builder, rmw)
    @test expand_partword!(rmw, 4)
    @test occursin(r"cmpxchg .* i32 .* monotonic monotonic", string(f))
    f, ptr, val = newfun("partword_volatile", T_i8)
    rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpMax, ptr, val, MO; align=1, volatile=true)
    ret!(builder, rmw)
    @test expand_partword!(rmw, 4)
    @test occursin(r"load atomic volatile i32", string(f))
    @test occursin(r"cmpxchg volatile .* i32", string(f))
    f, ptr, val = newfun("partword_or", T_i8)
    rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpOr, ptr, val, MO; align=1)
    ret!(builder, rmw)
    @test expand_partword!(rmw, 4)
    @test occursin(r"atomicrmw or .* i32 .* monotonic, align 4", string(f))
    f, ptr, val = newfun("partword_cmpxchg", T_i8)
    cx = atomic_cmpxchg!(builder, ptr, val, val, AR; align=1)
    ret!(builder, extract_value!(builder, cx, 0))
    @test expand_partword!(cx, 4)
    @test occursin("partword.cmpxchg.loop", string(f))
    f, ptr, val = newfun("wordsized", T_i32)
    rmw = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpAdd, ptr, val, MO)
    ret!(builder, rmw)
    @test !expand_partword!(rmw, 4)
    @test occursin("= atomicrmw add", string(f))

    # accessing a value in the word that contains it
    f, ptr, val = newfun("mask", T_i8)
    pm = partword_mask!(builder, T_i8, ptr; align=1, word_size=4)
    @test pm.word_type == T_i32 && pm.value_type == T_i8 && pm.aligned_addr_alignment == 4
    @test pm.inv_mask !== nothing
    word = load!(builder, pm.word_type, pm.aligned_addr; align=pm.aligned_addr_alignment)
    new_word = insert_masked_value!(builder, word, val, pm)
    store!(builder, new_word, pm.aligned_addr; align=pm.aligned_addr_alignment)
    ret!(builder, extract_masked_value!(builder, word, pm))
    f, ptr, val = newfun("mask_sizes", T_i32)
    pm = partword_mask!(builder, T_i32, ptr; align=4, word_size=4)
    @test pm.inv_mask === nothing
    pm = partword_mask!(builder, T_i32, ptr; align=4, word_size=8)
    @test pm.word_type == LLVM.Int64Type()
    @test occursin("i64 4294967295", string(f))
    ret!(builder, val)

    # casting atomics to integers
    f, ptr, val = newfun("cast", T_float)
    ld = load!(builder, T_float, ptr; ordering=AC, align=4)
    st = store!(builder, val, ptr; ordering=RE, align=4, volatile=true)
    xchg = atomic_rmw!(builder, O.LLVMAtomicRMWBinOpXchg, ptr, val, MO)
    ret!(builder, fadd!(builder, ld, xchg))
    new_ld = cast_atomic_to_integer!(ld)
    @test new_ld.value_type == T_i32 && new_ld.ordering == AC
    new_st = cast_atomic_to_integer!(st)
    @test new_st.volatile && new_st.ordering == RE
    new_xchg = cast_atomic_to_integer!(xchg)
    @test new_xchg.value_type == T_i32
    @test cast_atomic_to_integer!(new_ld) == new_ld

    @test verify(mod) === nothing
end
end

end
