@testset "instructions" begin

@testset "irbuilder" begin

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type(), LLVM.Int32Type(),
                                             LLVM.FloatType(), LLVM.FloatType(),
                                             LLVM.PointerType(LLVM.Int32Type()),
                                             LLVM.PointerType(LLVM.Int32Type()),
                                             LLVM.PointerType(LLVM.Int8Type())])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    entrybb = BasicBlock(fn, "entry")
    position!(builder, LLVM.at_end(entrybb))
    @test builder.insert_block == entrybb

    @test builder.debug_location === nothing
    LLVM.DIBuilder(mod) do dib
        difile = LLVM.file!(dib, "test.jl", "/tmp")
        LLVM.compile_unit!(dib, LLVM.API.LLVMDWARFSourceLanguageJulia,
                          difile, "LLVM.jl Tests")
        sp = LLVM.subprogram!(dib, difile, "SomeFunction", difile, 1,
                             LLVM.subroutine_type!(dib, difile, nothing))
        loc = DILocation(1, 1, sp)
        builder.debug_location = loc
        @test builder.debug_location == loc
        builder.debug_location = nothing
        @test builder.debug_location === nothing
    end

    retinst1 = ret!(builder)
    @check_ir retinst1 "ret void"
    retinst1.debug_location = builder.debug_location
    @test retinst1.debug_location === nothing

    retinst2 = ret!(builder, ConstantInt(LLVM.Int32Type(), 0))
    @check_ir retinst2 "ret i32 0"

    retinst3 = ret!(builder, Value[])
    @check_ir retinst3 "ret void poison"
    thenbb = BasicBlock(fn, "then")
    elsebb = BasicBlock(fn, "else")

    brinst1 = br!(builder, thenbb)
    @check_ir brinst1 "br label %then"

    cond1 = isnull!(builder, fn.parameters[1], "cond")
    brinst2 = br!(builder, cond1, thenbb, elsebb)
    @check_ir brinst2 "br i1 %cond, label %then, label %else"

    resumeinst = resume!(builder, UndefValue(LLVM.Int32Type()))
    @check_ir resumeinst "resume i32 undef"

    unreachableinst = unreachable!(builder)
    @check_ir unreachableinst "unreachable"

    int1 = fn.parameters[1]
    int2 = fn.parameters[2]

    float1 = fn.parameters[3]
    float2 = fn.parameters[4]

    binopinst = binop!(builder, LLVM.API.LLVMAdd, int1, int2)
    @check_ir binopinst "add i32 %0, %1"

    addinst = add!(builder, int1, int2)
    @check_ir addinst "add i32 %0, %1"

    nswaddinst = nswadd!(builder, int1, int2)
    @check_ir nswaddinst "add nsw i32 %0, %1"

    nuwaddinst = nuwadd!(builder, int1, int2)
    @check_ir nuwaddinst "add nuw i32 %0, %1"

    faddinst = fadd!(builder, float1, float2)
    @check_ir faddinst "fadd float %2, %3"

    subinst = sub!(builder, int1, int2)
    @check_ir subinst "sub i32 %0, %1"

    nswsubinst = nswsub!(builder, int1, int2)
    @check_ir nswsubinst "sub nsw i32 %0, %1"

    nuwsubinst = nuwsub!(builder, int1, int2)
    @check_ir nuwsubinst "sub nuw i32 %0, %1"

    fsubinst = fsub!(builder, float1, float2)
    @check_ir fsubinst "fsub float %2, %3"

    mulinst = mul!(builder, int1, int2)
    @check_ir mulinst "mul i32 %0, %1"

    nswmulinst = nswmul!(builder, int1, int2)
    @check_ir nswmulinst "mul nsw i32 %0, %1"

    nuwmulinst = nuwmul!(builder, int1, int2)
    @check_ir nuwmulinst "mul nuw i32 %0, %1"

    fmulinst = fmul!(builder, float1, float2)
    @check_ir fmulinst "fmul float %2, %3"

    udivinst = udiv!(builder, int1, int2)
    @check_ir udivinst "udiv i32 %0, %1"

    sdivinst = sdiv!(builder, int1, int2)
    @check_ir sdivinst "sdiv i32 %0, %1"

    exactsdivinst = exactsdiv!(builder, int1, int2)
    @check_ir exactsdivinst "sdiv exact i32 %0, %1"

    fdivinst = fdiv!(builder, float1, float2)
    @check_ir fdivinst "fdiv float %2, %3"

    ureminst = urem!(builder, int1, int2)
    @check_ir ureminst "urem i32 %0, %1"

    sreminst = srem!(builder, int1, int2)
    @check_ir sreminst "srem i32 %0, %1"

    freminst = frem!(builder, float1, float2)
    @check_ir freminst "frem float %2, %3"

    shlinst = shl!(builder, int1, int2)
    @check_ir shlinst "shl i32 %0, %1"

    lshrinst = lshr!(builder, int1, int2)
    @check_ir lshrinst "lshr i32 %0, %1"

    ashrinst = ashr!(builder, int1, int2)
    @check_ir ashrinst "ashr i32 %0, %1"

    andinst = and!(builder, int1, int2)
    @check_ir andinst "and i32 %0, %1"

    orinst = or!(builder, int1, int2)
    @check_ir orinst "or i32 %0, %1"

    xorinst = xor!(builder, int1, int2)
    @check_ir xorinst "xor i32 %0, %1"

    allocainst = alloca!(builder, LLVM.Int32Type())
    @check_ir allocainst "alloca i32"
    @test allocainst.allocated_type == LLVM.Int32Type()
    @test !hasproperty(xorinst, :allocated_type)
    @test allocainst.alignment == 4
    allocainst.alignment = 16
    @test allocainst.alignment == 16
    @check_ir allocainst "alloca i32, align 16"
    @test_throws ArgumentError allocainst.alignment = 0
    @test_throws ArgumentError allocainst.alignment = 3
    @test_throws ArgumentError allocainst.alignment = 2^32
    @test allocainst.alignment == 16

    # only stack allocations and memory accesses have an alignment
    @test !hasproperty(xorinst, :alignment)
    @test_throws "no property `alignment`" xorinst.alignment
    @test_throws "no property `alignment`" xorinst.alignment = 4

    aligned_allocainst = alloca!(builder, LLVM.Int32Type(); align=32)
    @check_ir aligned_allocainst "alloca i32, align 32"
    @test_throws "power of 2" alloca!(builder, LLVM.Int32Type(); align=3)

    array_allocainst = array_alloca!(builder, LLVM.Int32Type(), int1)
    @check_ir array_allocainst "alloca i32, i32 %0"

    aligned_array_allocainst = array_alloca!(builder, LLVM.Int32Type(), int1; align=8)
    @check_ir aligned_array_allocainst "alloca i32, i32 %0, align 8"

    addrspace_allocainst = alloca!(builder, LLVM.Int32Type(); addrspace=5, align=4)
    @check_ir addrspace_allocainst "alloca i32, align 4, addrspace(5)"
    @test addrspace_allocainst.value_type.addrspace == 5
    addrspace_array_allocainst = array_alloca!(builder, LLVM.Int32Type(), int1;
                                               addrspace=3)
    @check_ir addrspace_array_allocainst "alloca i32, i32 %0, align 4, addrspace(3)"
    @test_throws ArgumentError alloca!(builder, LLVM.Int32Type(); addrspace=-1)
    @test_throws ArgumentError alloca!(builder, LLVM.Int32Type(); addrspace=2^24)
    @test_throws ArgumentError array_alloca!(builder, LLVM.Int32Type(), int1;
                                             addrspace=1.0)

    mallocinst = malloc!(builder, LLVM.Int32Type())
    if supports_typed_pointers(ctx)
        @check_ir mallocinst r"bitcast i8\* %.+ to i32\*"
        @check_ir mallocinst.operands[1] r"call i8\* @malloc\(.+\)"
    else
        @check_ir mallocinst r"call ptr @malloc\(.+\)"
    end

    ptr = fn.parameters[6]

    array_mallocinst = array_malloc!(builder, LLVM.Int8Type(), ConstantInt(Int32(42)))
    if LLVM.version() >= v"21"
        @check_ir array_mallocinst r"call ptr @malloc\(.+\)"
    elseif supports_typed_pointers(ctx)
        @check_ir array_mallocinst r"call i8\* @malloc\(.+, i32 42\)"
    else
        @check_ir array_mallocinst r"call ptr @malloc\(.+, i32 42\)"
    end

    memsetisnt = memset!(builder, ptr, ConstantInt(Int8(1)), ConstantInt(Int32(2)), 4)
    if supports_typed_pointers(ctx)
        @check_ir memsetisnt r"call void @llvm.memset.p0i8.i32\(i8\* align 4 %.+, i8 1, i32 2, i1 false\)"
    else
        @check_ir memsetisnt r"call void @llvm.memset.p0.i32\(ptr align 4 %.+, i8 1, i32 2, i1 false\)"
    end

    memcpyinst = memcpy!(builder, allocainst, 4, ptr, 8, ConstantInt(Int32(32)))
    if supports_typed_pointers(ctx)
        @check_ir memcpyinst r"call void @llvm.memcpy.p0i8.p0i8.i32\(i8\* align 4 %.+, i8\* align 8 %.+, i32 32, i1 false\)"
    else
        @check_ir memcpyinst r"call void @llvm.memcpy.p0.p0.i32\(ptr align 4 %.+, ptr align 8 %.+, i32 32, i1 false\)"
    end

    memmoveinst = memmove!(builder, allocainst, 4, ptr, 8, ConstantInt(Int32(32)))
    if supports_typed_pointers(ctx)
        @check_ir memmoveinst r"call void @llvm.memmove.p0i8.p0i8.i32\(i8\* align 4 %.+, i8\* align 8 %.+, i32 32, i1 false\)"
    else
        @check_ir memmoveinst r"call void @llvm.memmove.p0.p0.i32\(ptr align 4 %.+, ptr align 8 %.+, i32 32, i1 false\)"
    end

    ptr1 = fn.parameters[5]

    freeinst = free!(builder, ptr1)
    @check_ir freeinst "tail call void @free"

    loadinst = load!(builder, LLVM.Int32Type(), ptr1)
    if supports_typed_pointers(ctx)
        @check_ir loadinst "load i32, i32* %4"
    else
        @check_ir loadinst "load i32, ptr %4"
    end
    loadinst.alignment = 4
    @test loadinst.alignment == 4
    @test loadinst.pointer_operand == ptr1
    @test !hasproperty(loadinst, :value_operand)

    @test !isatomic(loadinst)
    loadinst.ordering = LLVM.API.LLVMAtomicOrderingSequentiallyConsistent
    @test isatomic(loadinst)
    if supports_typed_pointers(ctx)
        @check_ir loadinst "load atomic i32, i32* %4 seq_cst"
    else
        @check_ir loadinst "load atomic i32, ptr %4 seq_cst"
    end
    @test loadinst.ordering == LLVM.API.LLVMAtomicOrderingSequentiallyConsistent

    @test loadinst.syncscope == SyncScope("system")
    loadinst.syncscope = SyncScope("singlethread")
    @test loadinst.syncscope == SyncScope("singlethread")

    storeinst = store!(builder, int1, ptr1)
    if supports_typed_pointers(ctx)
        @check_ir storeinst "store i32 %0, i32* %4"
    else
        @check_ir storeinst "store i32 %0, ptr %4"
    end
    @test storeinst.pointer_operand == ptr1
    @test storeinst.value_operand == int1

    fenceinst = fence!(builder, LLVM.API.LLVMAtomicOrderingSequentiallyConsistent)
    @check_ir fenceinst "fence"

    gepinst = gep!(builder, LLVM.Int32Type(), ptr1, [int1])
    if supports_typed_pointers(ctx)
        @check_ir gepinst "getelementptr i32, i32* %4, i32 %0"
    else
        @check_ir gepinst "getelementptr i32, ptr %4, i32 %0"
    end
    @test gepinst.pointer_operand == ptr1
    @test gepinst.source_element_type == LLVM.Int32Type()
    @test !gepinst.inbounds
    gepinst.inbounds = true
    @test gepinst.inbounds
    @check_ir gepinst "getelementptr inbounds i32"
    gepinst.inbounds = false
    @test !gepinst.inbounds

    gepinst1 = inbounds_gep!(builder, LLVM.Int32Type(), ptr1, [int1])
    if supports_typed_pointers(ctx)
        @check_ir gepinst1 "getelementptr inbounds i32, i32* %4, i32 %0"
    else
        @check_ir gepinst1 "getelementptr inbounds i32, ptr %4, i32 %0"
    end
    @test gepinst1.inbounds

    single_thread = false
    atomic_rmw_inst = atomic_rmw!(builder,
        LLVM.API.LLVMAtomicRMWBinOpAdd, ptr1, int1,
        LLVM.API.LLVMAtomicOrderingSequentiallyConsistent, single_thread)
    if supports_typed_pointers(ctx)
        @check_ir atomic_rmw_inst "atomicrmw add i32* %4, i32 %0 seq_cst"
    else
        @check_ir atomic_rmw_inst "atomicrmw add ptr %4, i32 %0 seq_cst"
    end
    @test atomic_rmw_inst.binop == LLVM.API.LLVMAtomicRMWBinOpAdd
    @test atomic_rmw_inst.pointer_operand == ptr1
    @test atomic_rmw_inst.value_operand == int1
    @test atomic_rmw_inst.syncscope == SyncScope("system")
    atomic_rmw_inst.syncscope = SyncScope("agent")
    @test atomic_rmw_inst.syncscope == SyncScope("agent")
    @test atomic_rmw_inst.syncscope.name == "agent"
    @test sprint(show, atomic_rmw_inst.syncscope) == "SyncScope(\"agent\")"
    for str in ("singlethread", "system", "agent")
        @test SyncScope(str).name == str
        @test SyncScope(SubString(str)) == SyncScope(str)
    end
    @test SyncScope("agent").context == ctx
    @test_throws ArgumentError LLVM.SyncScope(1000, ctx).name
    @test sprint(show, LLVM.SyncScope(1000, ctx)) == "SyncScope(target-specific scope 1000)"

    atomic_cmpxchg_inst = atomic_cmpxchg!(builder, ptr1, int1, int2,
        LLVM.API.LLVMAtomicOrderingSequentiallyConsistent, LLVM.API.LLVMAtomicOrderingAcquire, single_thread)
    if supports_typed_pointers(ctx)
        @check_ir atomic_cmpxchg_inst "cmpxchg i32* %4, i32 %0, i32 %1 seq_cst acquire"
    else
        @check_ir atomic_cmpxchg_inst "cmpxchg ptr %4, i32 %0, i32 %1 seq_cst acquire"
    end
    @test atomic_cmpxchg_inst.success_ordering == LLVM.API.LLVMAtomicOrderingSequentiallyConsistent
    atomic_cmpxchg_inst.success_ordering = LLVM.API.LLVMAtomicOrderingAcquireRelease
    @test atomic_cmpxchg_inst.success_ordering == LLVM.API.LLVMAtomicOrderingAcquireRelease
    @test atomic_cmpxchg_inst.failure_ordering == LLVM.API.LLVMAtomicOrderingAcquire
    atomic_cmpxchg_inst.failure_ordering = LLVM.API.LLVMAtomicOrderingMonotonic
    @test atomic_cmpxchg_inst.failure_ordering == LLVM.API.LLVMAtomicOrderingMonotonic
    @test atomic_cmpxchg_inst.pointer_operand == ptr1
    @test atomic_cmpxchg_inst.compare_operand == int1
    @test atomic_cmpxchg_inst.new_value_operand == int2
    @test_throws "read-only" atomic_cmpxchg_inst.compare_operand = int2
    @test !atomic_cmpxchg_inst.weak
    atomic_cmpxchg_inst.weak = true
    @test atomic_cmpxchg_inst.weak
    @test occursin("cmpxchg weak", string(atomic_cmpxchg_inst))

    single_thread = true
    atomic_rmw_inst = atomic_rmw!(builder,
        LLVM.API.LLVMAtomicRMWBinOpAdd, ptr1, int1,
        LLVM.API.LLVMAtomicOrderingSequentiallyConsistent, single_thread)
    if supports_typed_pointers(ctx)
        @check_ir atomic_rmw_inst "atomicrmw add i32* %4, i32 %0 syncscope(\"singlethread\") seq_cst"
    else
        @check_ir atomic_rmw_inst "atomicrmw add ptr %4, i32 %0 syncscope(\"singlethread\") seq_cst"
    end

    atomic_rmw_inst = atomic_rmw!(builder,
        LLVM.API.LLVMAtomicRMWBinOpAdd, ptr1, int1,
        LLVM.API.LLVMAtomicOrderingSequentiallyConsistent, SyncScope("agent"))
    if supports_typed_pointers(ctx)
        @check_ir atomic_rmw_inst "atomicrmw add i32* %4, i32 %0 syncscope(\"agent\") seq_cst"
    else
        @check_ir atomic_rmw_inst "atomicrmw add ptr %4, i32 %0 syncscope(\"agent\") seq_cst"
    end

    atomic_rmw_inst = atomic_rmw!(builder,
        LLVM.API.LLVMAtomicRMWBinOpAdd, ptr1, int1,
        LLVM.API.LLVMAtomicOrderingMonotonic, SyncScope("agent"))
    if supports_typed_pointers(ctx)
        @check_ir atomic_rmw_inst "atomicrmw add i32* %4, i32 %0 syncscope(\"agent\") monotonic"
    else
        @check_ir atomic_rmw_inst "atomicrmw add ptr %4, i32 %0 syncscope(\"agent\") monotonic"
    end

    @test !atomic_rmw_inst.volatile
    atomic_rmw_inst.volatile = true
    @test atomic_rmw_inst.volatile
    @test occursin("atomicrmw volatile add", string(atomic_rmw_inst))
    atomic_rmw_inst.volatile = false
    @test !atomic_rmw_inst.volatile
    @test !atomic_cmpxchg_inst.volatile
    atomic_cmpxchg_inst.volatile = true
    @test atomic_cmpxchg_inst.volatile

    # operations that are newer than the C API of some LLVM versions
    for op in (LLVM.API.LLVMAtomicRMWBinOpUIncWrap, LLVM.API.LLVMAtomicRMWBinOpUDecWrap,
               LLVM.API.LLVMAtomicRMWBinOpUSubCond, LLVM.API.LLVMAtomicRMWBinOpUSubSat)
        if LLVM.isavailable(op)
            for scope in (true, SyncScope("agent"))
                inst = atomic_rmw!(builder, op, ptr1, int1,
                                   LLVM.API.LLVMAtomicOrderingMonotonic, scope)
                @test inst.binop == op
            end
        else
            @test_throws ArgumentError atomic_rmw!(builder, op, ptr1, int1,
                                                   LLVM.API.LLVMAtomicOrderingMonotonic, false)
        end
    end
    @test LLVM.isavailable(LLVM.API.LLVMAtomicRMWBinOpAdd)
    @test LLVM.isavailable(LLVM.API.LLVMAtomicRMWBinOpUIncWrap) == (LLVM.version() >= v"16")
    @test LLVM.isavailable(LLVM.API.LLVMAtomicRMWBinOpFMaximum) == (LLVM.version() >= v"21")
    @test LLVM.isavailable(LLVM.API.LLVMAtomicRMWBinOpFMaximumNum) == (LLVM.version() >= v"23")
    @test !LLVM.isavailable(LLVM.API.LLVMAtomicRMWBinOp(1000))

    # operations are classified on every version of LLVM
    fp_names = ["fadd", "fsub", "fmax", "fmin", "fmaximum", "fminimum", "fmaximumnum",
                "fminimumnum"]
    for op in instances(LLVM.AtomicRMWBinOp.T)
        @test LLVM.isfloatingpoint(op) == (LLVM.irname(op) in fp_names)
    end
    @test !LLVM.isfloatingpoint(LLVM.API.LLVMAtomicRMWBinOpXchg)

    # operations can be named on every version of LLVM
    @test parse(LLVM.AtomicRMWBinOp.T, "fmaximumnum") == LLVM.API.LLVMAtomicRMWBinOpFMaximumNum
    @test parse(LLVM.AtomicRMWBinOp.T, "fminimumnum") == LLVM.API.LLVMAtomicRMWBinOpFMinimumNum
    @test_throws ArgumentError parse(LLVM.AtomicRMWBinOp.T, "fmaximumnumber")
    @test tryparse(LLVM.AtomicRMWBinOp.T, "fmaximumnum") == LLVM.API.LLVMAtomicRMWBinOpFMaximumNum
    @test tryparse(LLVM.AtomicRMWBinOp.T, "fmaximumnumber") === nothing
    @test tryparse(LLVM.AtomicOrdering.T, "acq_rel") == LLVM.API.LLVMAtomicOrderingAcquireRelease
    @test tryparse(LLVM.AtomicOrdering.T, "acquire_release") == LLVM.API.LLVMAtomicOrderingAcquireRelease
    @test tryparse(LLVM.AtomicOrdering.T, "relaxed") === nothing
    @test_throws ArgumentError parse(LLVM.AtomicOrdering.T, "relaxed")
    rmw_names = ["xchg", "add", "sub", "and", "nand", "or", "xor", "max", "min", "umax",
                 "umin", "fadd", "fsub", "fmax", "fmin", "uinc_wrap", "udec_wrap",
                 "usub_cond", "usub_sat", "fmaximum", "fminimum", "fmaximumnum",
                 "fminimumnum"]
    for name in rmw_names
        @test LLVM.irname(parse(LLVM.AtomicRMWBinOp.T, name)) == name
    end
    # ... and enumerated, including the ones that the C API in use doesn't define
    @test collect(LLVM.irname.(instances(LLVM.AtomicRMWBinOp.T))) == rmw_names
    @test typemax(LLVM.AtomicRMWBinOp.T) == LLVM.AtomicRMWBinOp.FMinimumNum
    @test Symbol(LLVM.AtomicRMWBinOp.UIncWrap) == :LLVMAtomicRMWBinOpUIncWrap
    available_ops = filter(LLVM.isavailable, instances(LLVM.AtomicRMWBinOp.T))
    @test LLVM.AtomicRMWBinOp.FMin in available_ops
    @test (LLVM.AtomicRMWBinOp.FMaximumNum in available_ops) == (LLVM.version() >= v"23")
    @test LLVM.irname(LLVM.AtomicRMWBinOp.UIncWrap) == "uinc_wrap"
    for name in ("not_atomic", "unordered", "monotonic", "acquire", "release", "acq_rel",
                 "seq_cst")
        @test LLVM.irname(parse(LLVM.AtomicOrdering.T, name)) == name
    end
    @test LLVM.irname(parse(LLVM.AtomicOrdering.T, "acquire_release")) == "acq_rel"
    # the IR name is what LLVM prints
    @test occursin(" $(LLVM.irname(atomic_rmw_inst.binop)) ", string(atomic_rmw_inst))
    @test occursin(" $(LLVM.irname(atomic_rmw_inst.ordering))", string(atomic_rmw_inst))

    truncinst = trunc!(builder, int1, LLVM.Int16Type())
    @check_ir truncinst "trunc i32 %0 to i16"

    zextinst = zext!(builder, int1, LLVM.Int64Type())
    @check_ir zextinst "zext i32 %0 to i64"

    sextinst = sext!(builder, int1, LLVM.Int64Type())
    @check_ir sextinst "sext i32 %0 to i64"

    fptouiinst = fptoui!(builder, float1, LLVM.Int32Type())
    @check_ir fptouiinst "fptoui float %2 to i32"

    fptosiinst = fptosi!(builder, float1, LLVM.Int32Type())
    @check_ir fptosiinst "fptosi float %2 to i32"

    uitofpinst = uitofp!(builder, int1, LLVM.FloatType())
    @check_ir uitofpinst "uitofp i32 %0 to float"

    sitofpinst = sitofp!(builder, int1, LLVM.FloatType())
    @check_ir sitofpinst "sitofp i32 %0 to float"

    fptruncinst = fptrunc!(builder, float1, LLVM.HalfType())
    @check_ir fptruncinst "fptrunc float %2 to half"

    fpextinst = fpext!(builder, float1, LLVM.DoubleType())
    @check_ir fpextinst "fpext float %2 to double"

    ptrtointinst = ptrtoint!(builder, fn.parameters[5], LLVM.Int32Type())
    if supports_typed_pointers(ctx)
        @check_ir ptrtointinst "ptrtoint i32* %4 to i32"
    else
        @check_ir ptrtointinst "ptrtoint ptr %4 to i32"
    end

    inttoptrinst = inttoptr!(builder, int1, LLVM.PointerType(LLVM.Int32Type()))
    if supports_typed_pointers(ctx)
        @check_ir inttoptrinst "inttoptr i32 %0 to i32*"
    else
        @check_ir inttoptrinst "inttoptr i32 %0 to ptr"
    end

    bitcastinst = bitcast!(builder, int1, LLVM.FloatType())
    @check_ir bitcastinst "bitcast i32 %0 to float"
    ptr1 = fn.parameters[5]
    if supports_typed_pointers(ctx)
        typ1 = ptr1.value_type
        ptr2 = LLVM.PointerType(typ1.element_type, 2)
        addrspacecastinst = addrspacecast!(builder, ptr1, ptr2)
        @check_ir addrspacecastinst "addrspacecast i32* %4 to i32 addrspace(2)*"
    else
        ptr2 = LLVM.PointerType(2)
        @test !hasproperty(ptr2, :element_type) || ptr2.element_type === nothing
        addrspacecastinst = addrspacecast!(builder, ptr1, ptr2)
        @check_ir addrspacecastinst "addrspacecast ptr %4 to ptr addrspace(2)"
    end

    zextorbitcastinst = zextorbitcast!(builder, int1, LLVM.FloatType())
    @check_ir zextorbitcastinst "bitcast i32 %0 to float"

    sextorbitcastinst = sextorbitcast!(builder, int1, LLVM.FloatType())
    @check_ir sextorbitcastinst "bitcast i32 %0 to float"

    truncorbitcastinst = truncorbitcast!(builder, int1, LLVM.FloatType())
    @check_ir truncorbitcastinst "bitcast i32 %0 to float"

    castinst = cast!(builder, LLVM.API.LLVMBitCast, int1, LLVM.FloatType())
    @check_ir castinst "bitcast i32 %0 to float"

    if supports_typed_pointers(ctx)
        floatptrtyp = LLVM.PointerType(LLVM.FloatType())

        pointercastinst = pointercast!(builder, ptr1, floatptrtyp)
        @check_ir pointercastinst "bitcast i32* %4 to float*"
    end

    intcastinst = intcast!(builder, int1, LLVM.Int64Type())
    @check_ir intcastinst "sext i32 %0 to i64"

    fpcastinst = fpcast!(builder, float1, LLVM.DoubleType())
    @check_ir fpcastinst "fpext float %2 to double"

    icmpinst = icmp!(builder, LLVM.API.LLVMIntEQ, int1, int2)
    @check_ir icmpinst "icmp eq i32 %0, %1"
    @test icmpinst.predicate == LLVM.API.LLVMIntEQ

    fcmpinst = fcmp!(builder, LLVM.API.LLVMRealOEQ, float1, float2)
    @check_ir fcmpinst "fcmp oeq float %2, %3"
    @test fcmpinst.predicate == LLVM.API.LLVMRealOEQ

    phiinst = phi!(builder, LLVM.Int32Type())
    @check_ir phiinst "phi i32 "

    selectinst = LLVM.select!(builder, cond1, int1, int2)
    @check_ir selectinst "select i1 %cond, i32 %0, i32 %1"

    trap = LLVM.Function(mod, "llvm.trap", LLVM.FunctionType(LLVM.VoidType()))

    callinst = call!(builder, LLVM.FunctionType(LLVM.VoidType()), trap)

    @check_ir callinst "call void @llvm.trap()"
    @test callinst.called_operand == trap
    @test callinst.called_function == trap
    @test callinst.called_type == LLVM.FunctionType(LLVM.VoidType())

    # tail calls: `tailcall` is a Bool view of `tailcall_kind`
    @test !callinst.tailcall
    @test callinst.tailcall_kind == LLVM.API.LLVMTailCallKindNone
    callinst.tailcall = true
    @test callinst.tailcall
    @test callinst.tailcall_kind == LLVM.API.LLVMTailCallKindTail
    @check_ir callinst "tail call void @llvm.trap()"
    callinst.tailcall_kind = LLVM.API.LLVMTailCallKindMustTail
    @test callinst.tailcall
    @check_ir callinst "musttail call void @llvm.trap()"
    callinst.tailcall = true    # doesn't demote a `musttail` call
    @test callinst.tailcall_kind == LLVM.API.LLVMTailCallKindMustTail
    callinst.tailcall = false
    @test !callinst.tailcall
    @test callinst.tailcall_kind == LLVM.API.LLVMTailCallKindNone
    callinst.tailcall_kind = LLVM.API.LLVMTailCallKindNoTail
    @test !callinst.tailcall
    @check_ir callinst "notail call void @llvm.trap()"
    callinst.tailcall = false   # keeps the `notail` marker
    @test callinst.tailcall_kind == LLVM.API.LLVMTailCallKindNoTail
    callinst.tailcall_kind = LLVM.API.LLVMTailCallKindNone
    @check_ir callinst "call void @llvm.trap()"

    neginst = neg!(builder, int1)
    @check_ir neginst "sub i32 0, %0"

    nswneginst = nswneg!(builder, int1)
    @check_ir nswneginst "sub nsw i32 0, %0"

    fneginst = fneg!(builder, float1)
    @check_ir fneginst "fneg float %2"

    notinst = not!(builder, int1)
    @check_ir notinst "xor i32 %0, -1"

    strinst = globalstring!(builder, "foobar")
    @check_ir strinst "private unnamed_addr constant [7 x i8] c\"foobar\\00\""

    str2inst = globalstring!(builder, "foobar"; addrspace=2, add_null=false)
    @check_ir str2inst "private unnamed_addr addrspace(2) constant [6 x i8] c\"foobar\""

    strptrinst = globalstring_ptr!(builder, "foobar")
    if supports_typed_pointers(ctx)
        @check_ir strptrinst "i8* getelementptr inbounds ([7 x i8], [7 x i8]* @2, i32 0, i32 0)"
    else
        # ... so it is folded away now.
        @check_ir strptrinst "private unnamed_addr constant [7 x i8] c\"foobar\\00\""
    end

    isnullinst = isnull!(builder, int1)
    @check_ir isnullinst "icmp eq i32 %0, 0"

    isnotnullinst = isnotnull!(builder, int1)
    @check_ir isnotnullinst "icmp ne i32 %0, 0"

    ptr1 = fn.parameters[5]
    ptr2 = fn.parameters[6]
    ptrdiffinst = ptrdiff!(builder, LLVM.Int32Type(), ptr1, ptr2)
    if supports_typed_pointers(ctx)
        @check_ir ptrdiffinst r"sdiv exact i64 %.+, ptrtoint \(i32\* getelementptr \(i32, i32\* null, i32 1\) to i64\)"
    else
        @check_ir ptrdiffinst r"sdiv exact i64 %.+, ptrtoint \(ptr getelementptr \(i32, ptr null, i32 1\) to i64\)"
    end

    position!(builder)
end

# by default, stack memory is allocated in the alloca address space of the data layout
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    mod.datalayout = "A5"
    fn = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.VoidType()))
    position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
    @check_ir alloca!(builder, LLVM.Int32Type()) "addrspace(5)"
    @check_ir array_alloca!(builder, LLVM.Int32Type(), ConstantInt(Int32(2))) "addrspace(5)"
    @test !occursin("addrspace", string(alloca!(builder, LLVM.Int32Type(); addrspace=0)))
end

end


@testset "synchronization scopes" begin
    ir = """
        define void @f(ptr %p) {
          %x = cmpxchg ptr %p, i32 0, i32 1 syncscope("agent") monotonic monotonic
          ret void
        }"""
    @dispose ctx=Context() begin
        typed_ir = supports_typed_pointers(ctx) ? replace(ir, "ptr" => "i32*") : ir
        mod = parse(LLVM.Module, typed_ir)
        inst = first(first(mod.functions["f"].blocks).instructions)
        scope = inst.syncscope
        @test scope.name == "agent"
        @test scope.context == ctx
        @test scope == SyncScope("agent")

        @dispose ctx2=Context() begin
            # the scope is resolved in the instruction's context, not the active one
            SyncScope("workgroup")
            @test inst.syncscope.name == "agent"
            @test sprint(show, inst.syncscope) == "SyncScope(\"agent\")"
            @test inst.syncscope == scope

            # scopes with the same name in different contexts are different
            other = SyncScope("agent")
            @test other.context == ctx2
            @test other != scope
            @test SyncScope("agent"; context=ctx) == scope

            # and can't be used with instructions of another context, not even the
            # well-known ones
            for name in ("agent", "system", "singlethread")
                @test_throws "another context" inst.syncscope = SyncScope(name)
            end
            @test inst.syncscope == scope
            inst.syncscope = SyncScope("workgroup"; context=ctx)
            @test inst.syncscope.name == "workgroup"
            # names are looked up in the instruction's context
            inst.syncscope = "agent"
            @test inst.syncscope == scope
            inst.syncscope = :workgroup
            @test inst.syncscope == SyncScope("workgroup"; context=ctx)
            inst.syncscope = "agent"

            # or with a builder of another context
            foreign_scopes = (other, SyncScope("system"))
            context!(ctx) do
            @dispose builder=IRBuilder() begin
                fn = mod.functions["f"]
                bb = first(fn.blocks)
                ptr = fn.parameters[1]
                position!(builder, LLVM.before(inst))
                n = count(Returns(true), bb.instructions)
                MO = LLVM.API.LLVMAtomicOrderingMonotonic
                i32 = LLVM.Int32Type()
                val = ConstantInt(i32, 0)
                for scope in foreign_scopes
                    @test_throws "another context" load!(builder, i32, ptr;
                                                         ordering=MO, scope)
                    @test_throws "another context" store!(builder, val, ptr;
                                                          ordering=MO, scope)
                    @test_throws "another context" fence!(builder,
                        LLVM.API.LLVMAtomicOrderingAcquire, scope)
                    @test_throws "another context" fence!(builder,
                        LLVM.API.LLVMAtomicOrderingAcquire; scope)
                    @test_throws "another context" atomic_rmw!(builder,
                        LLVM.API.LLVMAtomicRMWBinOpAdd, ptr, val, MO, scope)
                    @test_throws "another context" atomic_rmw!(builder,
                        LLVM.API.LLVMAtomicRMWBinOpAdd, ptr, val, MO; scope)
                    @test_throws "another context" atomic_cmpxchg!(builder, ptr, val, val,
                                                                   MO, MO, scope)
                    @test_throws "another context" atomic_cmpxchg!(builder, ptr, val, val,
                                                                   MO; scope)
                    # also for non-atomic accesses
                    @test_throws "another context" load!(builder, i32, ptr; scope)
                end
                # nothing was emitted
                @test count(Returns(true), bb.instructions) == n

                # names are resolved in the builder's context
                ld = load!(builder, i32, ptr; ordering=MO, scope="agent")
                @test ld.syncscope == scope
                ld = load!(builder, i32, ptr; ordering=MO, scope=:agent)
                @test ld.syncscope == scope
                ld = load!(builder, i32, ptr; scope="system")
                @test !isatomic(ld)
            end
            end
        end
        dispose(mod)
    end
end


@testset "call sites" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    f = LLVM.Function(mod, "f", ft)
    g = LLVM.Function(mod, "g", ft)
    h = LLVM.Function(mod, "h", LLVM.FunctionType(LLVM.Int32Type()))
    caller = LLVM.Function(mod, "caller", LLVM.FunctionType(LLVM.VoidType(), [LLVM.PointerType(ft)]))
    position!(builder, LLVM.at_end(BasicBlock(caller, "entry")))

    # direct calls
    call = call!(builder, ft, f)
    @test call.called_function == f
    call.called_operand = g
    @test call.called_function == g
    @test call.called_type == ft
    @check_ir call "call void @g()"

    # indirect calls, or calls of a function with a different type, are not direct calls
    ptr = caller.parameters[1]
    indirect = call!(builder, ft, ptr)
    @test indirect.called_function === nothing
    @test indirect.called_operand == ptr
    if !supports_typed_pointers(ctx)
        mismatch = call!(builder, ft, h)
        @test mismatch.called_function === nothing
        @test mismatch.called_operand == h
    end

    # the callee needs to have the same type as the called operand
    @test_throws ArgumentError call.called_operand = ConstantInt(Int32(0))

    ret!(builder)
    verify(mod)
end
end

@testset "aggregates" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    inner = LLVM.StructType([LLVM.Int8Type(), LLVM.Int16Type()])
    outer = LLVM.StructType([LLVM.Int32Type(), inner])
    f = LLVM.Function(mod, "f", LLVM.FunctionType(LLVM.Int8Type(), [outer]))
    position!(builder, LLVM.at_end(BasicBlock(f, "entry")))
    agg = f.parameters[1]

    ev = extract_value!(builder, agg, 1)
    @test ev.indices == [1]
    @test_throws BoundsError ev.indices[2]
    @test_throws CanonicalIndexError ev.indices[1] = 0
    ev2 = extract_value!(builder, ev, 0)
    @test ev2.indices == [0]
    iv = insert_value!(builder, agg, ev, 1)
    @test iv.indices == [1]
    @test !hasproperty(ev2, :pointer_operand)

    # nested elements can be selected using a path of indices
    ev3 = extract_value!(builder, agg, [1, 0])
    @check_ir ev3 "extractvalue { i32, { i8, i16 } } %0, 1, 0"
    @test ev3.indices == [1, 0]
    @test ev3.value_type == LLVM.Int8Type()
    iv2 = insert_value!(builder, agg, ev3, [1, 0])
    @check_ir iv2 r"insertvalue \{ i32, \{ i8, i16 \} \} %0, i8 %\d+, 1, 0"
    @test iv2.indices == [1, 0]

    # indices are checked
    @test_throws ArgumentError extract_value!(builder, agg, 2)
    @test_throws ArgumentError extract_value!(builder, agg, [1, 2])
    @test_throws ArgumentError extract_value!(builder, agg, [0, 0])
    @test_throws ArgumentError extract_value!(builder, agg, Int[])
    @test_throws ArgumentError insert_value!(builder, agg, ev3, [1, 1])
    @test_throws ArgumentError insert_value!(builder, agg, ev3, 0)

    # the field index of struct_gep! is zero-based too, and checked
    ptr = alloca!(builder, outer)
    gep = struct_gep!(builder, outer, ptr, 1)
    @check_ir gep r"getelementptr inbounds (nuw )?\{ i32, \{ i8, i16 \} \}"
    @check_ir gep "i32 0, i32 1"
    @test_throws ArgumentError struct_gep!(builder, outer, ptr, 2)
    @test_throws ArgumentError struct_gep!(builder, outer, ptr, -1)
    @test_throws ArgumentError struct_gep!(builder, LLVM.StructType("opaque"), ptr, 0)
    @test_throws MethodError struct_gep!(builder, LLVM.ArrayType(LLVM.Int32Type(), 2), ptr, 0)

    ret!(builder, ev2)
    verify(mod)
end
end

@testset "operations" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.Int32Type(), [LLVM.Int32Type(), LLVM.PointerType(LLVM.Int32Type())])
    f = LLVM.Function(mod, "f", ft)
    entry = BasicBlock(f, "entry")
    exit = BasicBlock(f, "exit")
    x, ptr = f.parameters
    position!(builder, LLVM.at_end(entry))
    a = add!(builder, x, x, "a")
    b = mul!(builder, x, x, "b")
    ld = load!(builder, LLVM.Int32Type(), ptr)
    st = store!(builder, a, ptr)
    br!(builder, exit)
    position!(builder, LLVM.at_end(exit))
    c = sub!(builder, a, b, "c")
    ret = ret!(builder, c)

    # ordering within a block
    @test comes_before(a, b)
    @test !comes_before(b, a)
    @test !comes_before(a, a)
    @test_throws ArgumentError comes_before(a, c)


    # memory effects
    @test !may_read_from_memory(a) && !may_write_to_memory(a) && !may_have_side_effects(a)
    @test may_read_from_memory(ld) && !may_write_to_memory(ld)
    @test !may_read_from_memory(st) && may_write_to_memory(st) && may_have_side_effects(st)

    # transferring names
    position!(builder, LLVM.before(ret))
    d = sub!(builder, a, b)
    @test take_name!(d, c) == d
    @test d.name == "c"
    @test c.name == ""
    replace_uses!(c, d)
    erase!(c)
    verify(mod)
    @test take_name!(d, d) == d
    @test d.name == "c"

    # transferring the name of an intrinsic makes a function the intrinsic
    trap = LLVM.Function(mod, "llvm.trap", LLVM.FunctionType(LLVM.VoidType()))
    other = LLVM.Function(mod, "other", LLVM.FunctionType(LLVM.VoidType()))
    take_name!(other, trap)
    @test other.name == "llvm.trap"
    @test isintrinsic(other)
    @test !isintrinsic(trap)
    erase!(trap)

    # stripping pointer casts
    gv = GlobalVariable(mod, LLVM.Int32Type(), "gv")
    alias = GlobalAlias(mod, LLVM.Int32Type(), gv, "alias")
    cast = const_addrspacecast(gv, LLVM.PointerType(LLVM.Int32Type(), 1))
    @test strip_pointer_casts(cast) == gv
    @test strip_pointer_casts(gv) == gv
    @test strip_pointer_casts(alias) == alias
    @test strip_pointer_casts_and_aliases(alias) == gv
    @test strip_pointer_casts_and_aliases(const_addrspacecast(alias, LLVM.PointerType(LLVM.Int32Type(), 1))) == gv
end
end

@testset "erasing while iterating" begin
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    f = LLVM.Function(mod, "f", LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()]))
    bb = BasicBlock(f, "entry")
    position!(builder, LLVM.at_end(bb))
    x = f.parameters[1]
    for i in 1:10
        add!(builder, x, ConstantInt(Int32(i)))
    end
    ret!(builder)

    # the instruction that was just returned can be erased
    for inst in bb.instructions
        if inst isa LLVM.AddInst
            erase!(inst)
        end
    end
    @test length(collect(bb.instructions)) == 1

    # the same holds for blocks
    for i in 1:3
        position!(builder, LLVM.at_end(BasicBlock(f, "unreachable$i")))
        unreachable!(builder)
    end
    for bb in f.blocks
        bb.name == "entry" || erase!(bb)
    end
    @test length(f.blocks) == 1
    verify(mod)
end
end

@testset "arguments" begin
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type(), LLVM.Int64Type()])
    f = LLVM.Function(mod, "f", ft)
    for (i, arg) in enumerate(f.parameters)
        @test arg.index == i
        @test arg.parent.parameters[arg.index] == arg
    end
end
end

@testset "LLVM 22 instructions" begin
if LLVM.version() >= v"22"
    @dispose ctx=Context() begin
        mod = parse(LLVM.Module, """
            define i64 @ptrtoaddr_test(ptr %p) {
            entry:
                %result = ptrtoaddr ptr %p to i64
                ret i64 %result
            }
            """)

        ptrtoaddr = first(first(mod.functions["ptrtoaddr_test"].blocks).instructions)
        @test ptrtoaddr isa LLVM.PtrToAddrInst

        dispose(mod)
    end
end
end

@testset "switch cases" begin
    @dispose ctx=Context() begin
        mod = parse(LLVM.Module, """
            define void @switch_test(i32 %value) {
            entry:
                switch i32 %value, label %default [
                    i32 1, label %one
                    i32 2, label %two
                ]
            one:
                ret void
            two:
                ret void
            default:
                ret void
            }
            """)

        switch = first(mod.functions["switch_test"].blocks).terminator
        @test convert(Int, switch.case_values[1]) == 1
        @test convert(Int, switch.case_values[2]) == 2
        @test_throws BoundsError switch.case_values[3]

        switch.case_values[2] = ConstantInt(Int32(3))
        @test convert(Int, switch.case_values[2]) == 3
        @test switch.successors[3] == mod.functions["switch_test"].blocks[3]
        @check_ir switch "i32 3, label %two"
        @test_throws BoundsError switch.case_values[0] = ConstantInt(Int32(0))
        @test_throws ArgumentError switch.case_values[1] = ConstantInt(Int64(0))
        @test_throws ArgumentError switch.case_values[1] = ConstantInt(Int32(3))
        # assigning a case its current value is fine
        switch.case_values[2] = switch.case_values[2]
        @test convert(Int, switch.case_values[2]) == 3

        # the cases of a switch are a mutable view of values and destinations
        f = mod.functions["switch_test"]
        _, one, two, default = f.blocks
        cases = switch.cases
        @test length(cases) == 2
        @test cases[1] == (ConstantInt(Int32(1)), one)
        @test cases[2] == (ConstantInt(Int32(3)), two)
        @test_throws BoundsError cases[3]

        @test push!(cases, (ConstantInt(Int32(4)), default)) === cases
        @test length(cases) == 3
        @test cases[3] == (ConstantInt(Int32(4)), default)
        @check_ir switch "i32 4, label %default"
        append!(cases, [(ConstantInt(Int32(5)), one), (ConstantInt(Int32(6)), two)])
        @test [convert(Int, val) for (val, _) in cases] == [1, 3, 4, 5, 6]
        @test switch.default_dest == default

        cases[1] = (ConstantInt(Int32(7)), two)
        @test cases[1] == (ConstantInt(Int32(7)), two)
        cases[1] = (ConstantInt(Int32(7)), one)   # the same value is fine
        @test cases[1] == (ConstantInt(Int32(7)), one)

        @test_throws ArgumentError push!(cases, (ConstantInt(Int32(4)), one))
        @test_throws ArgumentError push!(cases, (ConstantInt(Int64(8)), one))
        @test_throws ArgumentError cases[1] = (ConstantInt(Int32(3)), one)
        other = LLVM.Function(mod, "other", LLVM.FunctionType(LLVM.VoidType()))
        elsewhere = BasicBlock(other, "elsewhere")
        @test_throws ArgumentError push!(cases, (ConstantInt(Int32(9)), elsewhere))
        @test_throws ArgumentError cases[1] = (ConstantInt(Int32(9)), elsewhere)
        erase!(other)
        @test length(cases) == 5
        verify(mod)

        dispose(mod)
    end
end

@testset "poison-generating flags" begin
    @dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
        ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type(), LLVM.Int32Type()])
        fn = LLVM.Function(mod, "SomeFunction", ft)
        position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
        a, b = fn.parameters

        # nuw and nsw
        for inst in [add!(builder, a, b), sub!(builder, a, b), mul!(builder, a, b),
                     shl!(builder, a, b)]
            @test !inst.nuw && !inst.nsw
            inst.nuw = true
            @test inst.nuw && !inst.nsw
            @check_ir inst " nuw i32"
            inst.nsw = true
            @test inst.nuw && inst.nsw
            @check_ir inst " nuw nsw i32"
            inst.nuw = false
            inst.nsw = false
            @test !inst.nuw && !inst.nsw
        end
        @test nuwadd!(builder, a, b).nuw
        @test nswsub!(builder, a, b).nsw
        trunc = trunc!(builder, a, LLVM.Int8Type())
        if LLVM.version() >= v"19"
            trunc.nuw = true
            trunc.nsw = true
            @check_ir trunc "trunc nuw nsw i32"
        else
            @test !hasproperty(trunc, :nuw)
            @test_throws "no property `nuw`" trunc.nuw = true
        end

        # exact
        for inst in [udiv!(builder, a, b), sdiv!(builder, a, b), lshr!(builder, a, b),
                     ashr!(builder, a, b)]
            @test !inst.exact
            inst.exact = true
            @test inst.exact
            @check_ir inst " exact i32"
            inst.exact = false
            @test !inst.exact
        end
        @test exactsdiv!(builder, a, b).exact

        # disjoint
        or = or!(builder, a, b)
        if LLVM.version() >= v"18"
            @test !or.disjoint
            or.disjoint = true
            @test or.disjoint
            @check_ir or "or disjoint i32"
        else
            @test_throws "no property `disjoint`" or.disjoint
        end

        # nneg
        zext = zext!(builder, a, LLVM.Int64Type())
        uitofp = uitofp!(builder, a, LLVM.DoubleType())
        if LLVM.version() >= v"18"
            @test !zext.nneg
            zext.nneg = true
            @test zext.nneg
            @check_ir zext "zext nneg i32"
        else
            @test_throws "no property `nneg`" zext.nneg = true
        end
        if LLVM.version() >= v"19"
            uitofp.nneg = true
            @test uitofp.nneg
            @check_ir uitofp "uitofp nneg i32"
        else
            @test_throws "no property `nneg`" uitofp.nneg = true
        end

        # samesign
        icmp = icmp!(builder, LLVM.API.LLVMIntULT, a, b)
        if LLVM.version() >= v"20"
            @test !icmp.samesign
            icmp.samesign = true
            @test icmp.samesign
            @check_ir icmp "icmp samesign ult i32"
        else
            @test_throws "no property `samesign`" icmp.samesign = true
        end

        # the flags are only available on instructions that support them
        xor = xor!(builder, a, b)
        for flag in (:nuw, :nsw, :exact, :disjoint, :nneg, :samesign)
            @test !hasproperty(xor, flag)
            @test_throws "no property `$flag`" getproperty(xor, flag)
        end
        @test_throws "no property `nuw`" or.nuw
        @test_throws "no property `exact`" icmp.exact = true
    end
end

@testset "operand bundles" begin
    typed_ir = """
        declare void @x()
        declare void @y()
        declare void @z()

        define void @f() {
            call void @x()
            call void @y() [ "deopt"(i32 1, i64 2) ]
            call void @z() [ "deopt"(), "unknown"(i8* null) ]
            ret void
        }

        define void @g() {
            ret void
        }"""
    opaque_ir = """
        declare void @x()
        declare void @y()
        declare void @z()

        define void @f() {
            call void @x()
            call void @y() [ "deopt"(i32 1, i64 2) ]
            call void @z() [ "deopt"(), "unknown"(ptr null) ]
            ret void
        }

        define void @g() {
            ret void
        }"""
    @dispose ctx=Context() begin
        mod = parse(LLVM.Module, supports_typed_pointers(ctx) ? typed_ir : opaque_ir)

        @testset "iteration" begin
            f = mod.functions["f"]
            bb = first(f.blocks)
            cx, cy, cz = bb.instructions

            ## operands includes the function, and each operand bundle input separately
            @test length(cx.operands) == 1
            @test length(cy.operands) == 3
            @test length(cz.operands) == 2

            ## arguments excludes all those
            @test length(cx.arguments) == 0
            @test length(cy.arguments) == 0
            @test length(cz.arguments) == 0

            let bundles = cx.operand_bundles
                @test isempty(bundles)
            end

            let bundles = cy.operand_bundles
                @test length(bundles) == 1
                bundle = first(bundles)
                @test bundle.tag == "deopt"
                @test string(bundle) == "\"deopt\"(i32 1, i64 2)"

                inputs = bundle.inputs
                @test length(inputs) == 2
                @test inputs[1] == LLVM.ConstantInt(Int32(1))
                @test inputs[2] == LLVM.ConstantInt(Int64(2))
            end

            let bundles = cz.operand_bundles
                @test length(bundles) == 2
                let bundle = bundles[1]
                    inputs = bundle.inputs
                    @test length(inputs) == 0
                    @test string(bundle) == "\"deopt\"()"
                end
                let bundle = bundles[2]
                    inputs = bundle.inputs
                    @test length(inputs) == 1
                    if supports_typed_pointers(ctx)
                        @test string(bundle) == "\"unknown\"(i8* null)"
                    else
                        @test string(bundle) == "\"unknown\"(ptr null)"
                    end
                end
            end
        end

        @testset "creation" begin
            g = mod.functions["g"]
            bb = first(g.blocks)
            inst = first(bb.instructions)

            inputs = [LLVM.ConstantInt(Int32(1)), LLVM.ConstantInt(Int64(2))]
            bundle1 = OperandBundle("unknown", inputs)
            @test bundle1 isa OperandBundle
            @test bundle1.tag == "unknown"
            @test bundle1.inputs == inputs
            @test string(bundle1) == "\"unknown\"(i32 1, i64 2)"

            # use in a call
            f = mod.functions["x"]
            ft = f.function_type
            @dispose builder=IRBuilder() begin
                position!(builder, LLVM.before(inst))
                inst = call!(builder, ft, f, Value[], [bundle1])

                bundles = inst.operand_bundles
                @test length(bundles) == 1

                # test the ability to directly forward `operand_bundles`
                inst2 = call!(builder, ft, f, Value[], bundles)

                bundle2 = bundles[1]
                @test bundle2 isa OperandBundle
                @test bundle2.tag == "unknown"
                @test bundle2.inputs == inputs
                @test string(bundle2) == "\"unknown\"(i32 1, i64 2)"
            end
        end

        dispose(mod)
    end
end


@testset "fast math" begin
@dispose ctx=Context() mod=LLVM.Module("my_module") begin
    # emit some IR
    param_types = [LLVM.FloatType()]
    ret_type = LLVM.FloatType()
    fun_type = LLVM.FunctionType(ret_type, param_types)
    fun = LLVM.Function(mod, "add_sub", fun_type)
    @dispose builder=IRBuilder() begin
        entry = BasicBlock(fun, "entry")
        position!(builder, LLVM.at_end(entry))
        # add and substract 42

        a = fadd!(builder, fun.parameters[1], LLVM.ConstantFP(Float32(42.)), "a")
        b = fsub!(builder, a, LLVM.ConstantFP(Float32(42.)), "b")
        retinst = ret!(builder, b)

        # support for removing/insertion
        remove!(retinst)
        move!(retinst, builder.position)
    end
    verify(mod)

    # optimize
    function optimize(mod)
        host_triple = LLVM.default_triple()
        host_t = LLVM.Target(triple=host_triple)
        @dispose tm=LLVM.TargetMachine(host_t, host_triple) begin
            run!("default<O3>", mod, tm)
        end
    end
    optimize(mod)
    verify(mod)

    # ensure we still have our two operations
    @test length(fun.blocks) == 1
    bb = fun.blocks[1]
    instns = collect(bb.instructions)
    @test length(instns) == 3
    @test instns[1] isa LLVM.FAddInst
    @test instns[2] isa LLVM.FAddInst
    @test instns[3] isa LLVM.RetInst

    # make them fast math
    @test !instns[1].fast_math.contract
    instns[1].fast_math.fast = true
    @test instns[1].fast_math.contract
    instns[2].fast_math = instns[1].fast_math
    @test instns[2].fast_math.fast
    @test !hasproperty(instns[3], :fast_math)
    @test_throws "has no property `fast_math`" instns[3].fast_math
    @test_throws "has no property `fast_math`" instns[3].fast_math = (; fast=true)
    @test supports_fast_math(instns[1])
    @test !supports_fast_math(instns[3])

    # optimize again
    optimize(mod)
    verify(mod)

    # observe there's only a single return now
    @test length(fun.blocks) == 1
    bb = fun.blocks[1]
    instns = collect(bb.instructions)
    @test length(instns) == 1
    @test instns[1] isa LLVM.RetInst
end

# whether phi, select and call instructions support fast-math flags depends on their type
@dispose ctx=Context() mod=LLVM.Module("SomeModule") builder=IRBuilder() begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.FloatType(), LLVM.Int32Type(), LLVM.Int1Type()])
    f = LLVM.Function(mod, "f", ft)
    position!(builder, LLVM.at_end(BasicBlock(f, "entry")))
    x, y, c = f.parameters
    fsel = select!(builder, c, x, x)
    isel = select!(builder, c, y, y)
    @test supports_fast_math(fsel)
    @test !supports_fast_math(isel)
    @test hasproperty(isel, :fast_math)
    @test_throws ArgumentError isel.fast_math
end
end

end
