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
    position!(builder, entrybb)
    @test position(builder) == entrybb

    @test debuglocation(builder) === nothing
    loc = DILocation(1, 1)
    debuglocation!(builder, loc)
    @test debuglocation(builder) == loc
    debuglocation!(builder)
    @test debuglocation(builder) === nothing

    retinst1 = ret!(builder)
    @check_ir retinst1 "ret void"
    debuglocation!(builder, retinst1)

    retinst2 = ret!(builder, ConstantInt(LLVM.Int32Type(), 0))
    @check_ir retinst2 "ret i32 0"

    retinst3 = ret!(builder, Value[])
    @check_ir retinst3 "ret void poison"
    thenbb = BasicBlock(fn, "then")
    elsebb = BasicBlock(fn, "else")

    brinst1 = br!(builder, thenbb)
    @check_ir brinst1 "br label %then"

    cond1 = isnull!(builder, parameters(fn)[1], "cond")
    brinst2 = br!(builder, cond1, thenbb, elsebb)
    @check_ir brinst2 "br i1 %cond, label %then, label %else"

    resumeinst = resume!(builder, UndefValue(LLVM.Int32Type()))
    @check_ir resumeinst "resume i32 undef"

    unreachableinst = unreachable!(builder)
    @check_ir unreachableinst "unreachable"

    int1 = parameters(fn)[1]
    int2 = parameters(fn)[2]

    float1 = parameters(fn)[3]
    float2 = parameters(fn)[4]

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

    array_allocainst = array_alloca!(builder, LLVM.Int32Type(), int1)
    @check_ir array_allocainst "alloca i32, i32 %0"

    mallocinst = malloc!(builder, LLVM.Int32Type())
    if supports_typed_pointers(ctx)
        @check_ir mallocinst r"bitcast i8\* %.+ to i32\*"
        @check_ir operands(mallocinst)[1] r"call i8\* @malloc\(.+\)"
    else
        @check_ir mallocinst r"call ptr @malloc\(.+\)"
    end

    ptr = parameters(fn)[6]

    array_mallocinst = array_malloc!(builder, LLVM.Int8Type(), ConstantInt(Int32(42)))
    if supports_typed_pointers(ctx)
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

    ptr1 = parameters(fn)[5]

    freeinst = free!(builder, ptr1)
    @check_ir freeinst "tail call void @free"

    loadinst = load!(builder, LLVM.Int32Type(), ptr1)
    if supports_typed_pointers(ctx)
        @check_ir loadinst "load i32, i32* %4"
    else
        @check_ir loadinst "load i32, ptr %4"
    end
    alignment!(loadinst, 4)
    @test alignment(loadinst) == 4

    @test !is_atomic(loadinst)
    ordering!(loadinst, LLVM.API.LLVMAtomicOrderingSequentiallyConsistent)
    @test is_atomic(loadinst)
    if supports_typed_pointers(ctx)
        @check_ir loadinst "load atomic i32, i32* %4 seq_cst"
    else
        @check_ir loadinst "load atomic i32, ptr %4 seq_cst"
    end
    @test ordering(loadinst) == LLVM.API.LLVMAtomicOrderingSequentiallyConsistent

    @test syncscope(loadinst) == SyncScope("system")
    syncscope!(loadinst, SyncScope("singlethread"))
    @test syncscope(loadinst) == SyncScope("singlethread")

    storeinst = store!(builder, int1, ptr1)
    if supports_typed_pointers(ctx)
        @check_ir storeinst "store i32 %0, i32* %4"
    else
        @check_ir storeinst "store i32 %0, ptr %4"
    end

    fenceinst = fence!(builder, LLVM.API.LLVMAtomicOrderingNotAtomic)
    @check_ir fenceinst "fence"

    gepinst = gep!(builder, LLVM.Int32Type(), ptr1, [int1])
    if supports_typed_pointers(ctx)
        @check_ir gepinst "getelementptr i32, i32* %4, i32 %0"
    else
        @check_ir gepinst "getelementptr i32, ptr %4, i32 %0"
    end

    gepinst1 = inbounds_gep!(builder, LLVM.Int32Type(), ptr1, [int1])
    if supports_typed_pointers(ctx)
        @check_ir gepinst1 "getelementptr inbounds i32, i32* %4, i32 %0"
    else
        @check_ir gepinst1 "getelementptr inbounds i32, ptr %4, i32 %0"
    end

    single_thread = false
    atomic_rmw_inst = atomic_rmw!(builder,
        LLVM.API.LLVMAtomicRMWBinOpAdd, ptr1, int1,
        LLVM.API.LLVMAtomicOrderingSequentiallyConsistent, single_thread)
    if supports_typed_pointers(ctx)
        @check_ir atomic_rmw_inst "atomicrmw add i32* %4, i32 %0 seq_cst"
    else
        @check_ir atomic_rmw_inst "atomicrmw add ptr %4, i32 %0 seq_cst"
    end
    @test binop(atomic_rmw_inst) == LLVM.API.LLVMAtomicRMWBinOpAdd
    @test syncscope(atomic_rmw_inst) == SyncScope("system")
    syncscope!(atomic_rmw_inst, SyncScope("agent"))
    @test syncscope(atomic_rmw_inst) == SyncScope("agent")

    atomic_cmpxchg_inst = atomic_cmpxchg!(builder, ptr1, int1, int2,
        LLVM.API.LLVMAtomicOrderingSequentiallyConsistent, LLVM.API.LLVMAtomicOrderingAcquire, single_thread)
    if supports_typed_pointers(ctx)
        @check_ir atomic_cmpxchg_inst "cmpxchg i32* %4, i32 %0, i32 %1 seq_cst acquire"
    else
        @check_ir atomic_cmpxchg_inst "cmpxchg ptr %4, i32 %0, i32 %1 seq_cst acquire"
    end
    @test success_ordering(atomic_cmpxchg_inst) == LLVM.API.LLVMAtomicOrderingSequentiallyConsistent
    success_ordering!(atomic_cmpxchg_inst, LLVM.API.LLVMAtomicOrderingAcquireRelease)
    @test success_ordering(atomic_cmpxchg_inst) == LLVM.API.LLVMAtomicOrderingAcquireRelease
    @test failure_ordering(atomic_cmpxchg_inst) == LLVM.API.LLVMAtomicOrderingAcquire
    failure_ordering!(atomic_cmpxchg_inst, LLVM.API.LLVMAtomicOrderingMonotonic)
    @test failure_ordering(atomic_cmpxchg_inst) == LLVM.API.LLVMAtomicOrderingMonotonic
    @test !isweak(atomic_cmpxchg_inst)
    weak!(atomic_cmpxchg_inst, true)
    @test isweak(atomic_cmpxchg_inst)

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

    ptrtointinst = ptrtoint!(builder, parameters(fn)[5], LLVM.Int32Type())
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
    ptr1 = parameters(fn)[5]
    if supports_typed_pointers(ctx)
        typ1 = value_type(ptr1)
        ptr2 = LLVM.PointerType(eltype(typ1), 2)
        addrspacecastinst = addrspacecast!(builder, ptr1, ptr2)
        @check_ir addrspacecastinst "addrspacecast i32* %4 to i32 addrspace(2)*"
    else
        ptr2 = LLVM.PointerType(2)
        @test_throws ErrorException eltype(ptr2)
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
    @test predicate(icmpinst) == LLVM.API.LLVMIntEQ

    fcmpinst = fcmp!(builder, LLVM.API.LLVMRealOEQ, float1, float2)
    @check_ir fcmpinst "fcmp oeq float %2, %3"
    @test predicate(fcmpinst) == LLVM.API.LLVMRealOEQ

    phiinst = phi!(builder, LLVM.Int32Type())
    @check_ir phiinst "phi i32 "

    selectinst = LLVM.select!(builder, cond1, int1, int2)
    @check_ir selectinst "select i1 %cond, i32 %0, i32 %1"

    trap = LLVM.Function(mod, "llvm.trap", LLVM.FunctionType(LLVM.VoidType()))

    callinst = call!(builder, LLVM.FunctionType(LLVM.VoidType()), trap)

    @check_ir callinst "call void @llvm.trap()"
    @test called_operand(callinst) == trap
    @test called_type(callinst) == LLVM.FunctionType(LLVM.VoidType())

    neginst = neg!(builder, int1)
    @check_ir neginst "sub i32 0, %0"

    nswneginst = nswneg!(builder, int1)
    @check_ir nswneginst "sub nsw i32 0, %0"

    nuwneginst = nuwneg!(builder, int1)
    @check_ir nuwneginst "sub nuw i32 0, %0"

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

    ptr1 = parameters(fn)[5]
    ptr2 = parameters(fn)[6]
    ptrdiffinst = ptrdiff!(builder, LLVM.Int32Type(), ptr1, ptr2)
    if supports_typed_pointers(ctx)
        @check_ir ptrdiffinst r"sdiv exact i64 %.+, ptrtoint \(i32\* getelementptr \(i32, i32\* null, i32 1\) to i64\)"
    else
        @check_ir ptrdiffinst r"sdiv exact i64 %.+, ptrtoint \(ptr getelementptr \(i32, ptr null, i32 1\) to i64\)"
    end

    position!(builder)
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
            f = functions(mod)["f"]
            bb = first(blocks(f))
            cx, cy, cz = instructions(bb)

            ## operands includes the function, and each operand bundle input separately
            @test length(operands(cx)) == 1
            @test length(operands(cy)) == 3
            @test length(operands(cz)) == 2

            ## arguments excludes all those
            @test length(arguments(cx)) == 0
            @test length(arguments(cy)) == 0
            @test length(arguments(cz)) == 0

            let bundles = operand_bundles(cx)
                @test isempty(bundles)
            end

            let bundles = operand_bundles(cy)
                @test length(bundles) == 1
                bundle = first(bundles)
                @test LLVM.tag(bundle) == "deopt"
                @test string(bundle) == "\"deopt\"(i32 1, i64 2)"

                inputs = LLVM.inputs(bundle)
                @test length(inputs) == 2
                @test inputs[1] == LLVM.ConstantInt(Int32(1))
                @test inputs[2] == LLVM.ConstantInt(Int64(2))
            end

            let bundles = operand_bundles(cz)
                @test length(bundles) == 2
                let bundle = bundles[1]
                    inputs = LLVM.inputs(bundle)
                    @test length(inputs) == 0
                    @test string(bundle) == "\"deopt\"()"
                end
                let bundle = bundles[2]
                    inputs = LLVM.inputs(bundle)
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
            g = functions(mod)["g"]
            bb = first(blocks(g))
            inst = first(instructions(bb))

            inputs = [LLVM.ConstantInt(Int32(1)), LLVM.ConstantInt(Int64(2))]
            bundle1 = OperandBundle("unknown", inputs)
            @test bundle1 isa OperandBundle
            @test LLVM.tag(bundle1) == "unknown"
            @test LLVM.inputs(bundle1) == inputs
            @test string(bundle1) == "\"unknown\"(i32 1, i64 2)"

            # use in a call
            f = functions(mod)["x"]
            ft = function_type(f)
            @dispose builder=IRBuilder() begin
                position!(builder, inst)
                inst = call!(builder, ft, f, Value[], [bundle1])

                bundles = operand_bundles(inst)
                @test length(bundles) == 1

                # test the ability to directly forward `operand_bundles`
                inst2 = call!(builder, ft, f, Value[], bundles)

                bundle2 = bundles[1]
                @test bundle2 isa OperandBundle
                @test LLVM.tag(bundle2) == "unknown"
                @test LLVM.inputs(bundle2) == inputs
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
        position!(builder, entry)
        # add and substract 42

        a = fadd!(builder, parameters(fun)[1], LLVM.ConstantFP(Float32(42.)), "a")
        b = fsub!(builder, a, LLVM.ConstantFP(Float32(42.)), "b")
        retinst = ret!(builder, b)

        # support for removing/insertion
        remove!(retinst)
        insert!(builder, retinst)
    end
    verify(mod)

    # optimize
    function optimize(mod)
        host_triple = triple()
        host_t = Target(triple=host_triple)
        @dispose tm=TargetMachine(host_t, host_triple) begin
            run!("default<O3>", mod, tm)
        end
    end
    optimize(mod)
    verify(mod)

    # ensure we still have our two operations
    @test length(blocks(fun)) == 1
    bb = blocks(fun)[1]
    instns = collect(instructions(bb))
    @test length(instns) == 3
    @test instns[1] isa LLVM.FAddInst
    @test instns[2] isa LLVM.FAddInst
    @test instns[3] isa LLVM.RetInst

    # make them fast math
    @test !fast_math(instns[1]).contract
    fast_math!(instns[1]; all=true)
    @test fast_math(instns[1]).contract
    fast_math!(instns[2]; all=true)
    @test_throws ArgumentError fast_math(instns[3])
    @test_throws ArgumentError fast_math!(instns[3]; all=true)

    # optimize again
    optimize(mod)
    verify(mod)

    # observe there's only a single return now
    @test length(blocks(fun)) == 1
    bb = blocks(fun)[1]
    instns = collect(instructions(bb))
    @test length(instns) == 1
    @test instns[1] isa LLVM.RetInst
end
end

end
