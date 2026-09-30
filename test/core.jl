@testset "core" begin

using BFloat16s

struct TestStruct
    x::Bool
    y::Int64
    z::Float16
end

struct AnotherTestStruct
    x::Int
end

struct TestSingleton
end

@testset "context" begin

@test context(; throw_error=false) === nothing

let
    ctx = Context()
    @test context() == ctx
    let ctx2 = Context()
        @test context() == ctx2
        @test ctx !== ctx2
        dispose(ctx2)
    end
    @test context() == ctx
    dispose(ctx)
    @test context(; throw_error=false) === nothing
end

Context() do ctx end

@dispose ctx=Context() begin end

@dispose ctx=Context() begin
    @test supports_typed_pointers(ctx) isa Bool
    if LLVM.version() > v"17"
        @test supports_typed_pointers(ctx) == false
    end
end

# disposing a context during exception unwinding should leak it instead of freeing it,
# so that values captured by the exception (or by test machinery recording it) can still
# be displayed afterwards
let
    val = Ref{Any}()
    @test_throws ErrorException Context() do ctx
        val[] = ConstantInt(Int32(42))
        error("some error")
    end
    @test occursin("42", string(val[]))
end

# `@dispose` should dispose of resources that were already constructed when constructing a
# later one throws, so that the context doesn't remain active (JuliaLLVM/LLVM.jl#429)
Context() do ctx
    @test_throws LLVMException @dispose ctx2=Context() mod=parse(LLVM.Module, UInt8[1,2,3,4]) begin
        error("unreachable")
    end
    @test context() == ctx
end

@test context(; throw_error=false) === nothing

end


@testset "type" begin

@dispose ctx=Context() begin
    typ = LLVM.Int1Type()
    @test typeof(typ.ref) == LLVM.API.LLVMTypeRef                 # untyped

    @test typeof(LLVM.IntegerType(typ.ref)) == LLVM.IntegerType   # type reconstructed
    if LLVM.typecheck_enabled
        @test_throws ErrorException LLVM.FunctionType(typ.ref)    # wrong type
    end
    @test_throws UndefRefError LLVM.FunctionType(LLVM.API.LLVMTypeRef(C_NULL))

    @test typeof(typ.ref) == LLVM.API.LLVMTypeRef
    @test typeof(LLVMType(typ.ref)) == LLVM.IntegerType           # type reconstructed
    @test_throws UndefRefError LLVMType(LLVM.API.LLVMTypeRef(C_NULL))

    @test LLVM.IntType(8).width == 8

    @test issized(LLVM.Int1Type())
    @test !issized(LLVM.VoidType())
    @test_throws ErrorException sizeof(typ)
end

# integer
@dispose ctx=Context() begin
    typ = LLVM.Int1Type()
    @test context(typ) == ctx

    show(devnull, typ)

    @test !isempty(typ)
end

# floating-point

# function
@dispose ctx=Context() begin
    x = LLVM.Int1Type()
    y = [LLVM.Int8Type(), LLVM.Int16Type()]
    ft = LLVM.FunctionType(x, y)
    @test context(ft) == ctx

    @test !isvararg(ft)
    @test ft.return_type == x
    @test ft.parameters == y
    @test_throws BoundsError ft.parameters[3]
end

# sequential
@dispose ctx=Context() begin
    eltyp = LLVM.Int32Type()

    ptrtyp = LLVM.PointerType(eltyp)
    if supports_typed_pointers(ctx)
        @test eltype(ptrtyp) == eltyp
    end

    @test context(ptrtyp) == context(eltyp)

    @test ptrtyp.addrspace == 0

    ptrtyp = LLVM.PointerType(eltyp, 1)
    @test ptrtyp.addrspace == 1
end
@dispose ctx=Context() begin
    eltyp = LLVM.Int32Type()

    arrtyp = LLVM.ArrayType(eltyp, 2)
    @test eltype(arrtyp) == eltyp
    @test context(arrtyp) == context(eltyp)
    @test !isempty(arrtyp)

    @test length(arrtyp) == 2
end
@dispose ctx=Context() begin
    eltyp = LLVM.Int32Type()

    arrtyp = LLVM.ArrayType(eltyp, 0)
    @test isempty(arrtyp)
end
if LLVM.version() >= v"17" && Sys.WORD_SIZE == 64
    # arrays can have more than 2^32 elements
    @dispose ctx=Context() begin
        arrtyp = LLVM.ArrayType(LLVM.Int8Type(), 2^32 + 1)
        @test length(arrtyp) == 2^32 + 1
        @test string(arrtyp) == "[4294967297 x i8]"
    end
end
@dispose ctx=Context() begin
    eltyp = LLVM.Int32Type()

    vectyp = LLVM.VectorType(eltyp, 2)
    @test eltype(vectyp) == eltyp
    @test context(vectyp) == context(eltyp)

    @test length(vectyp) == 2
end

# structure
@dispose ctx=Context() begin
    elem = [LLVM.Int32Type(), LLVM.FloatType()]

    let st = LLVM.StructType(elem)
        @test context(st) == ctx
        @test !ispacked(st)
        @test !isopaque(st)
        @test st.name === nothing

        let elem_it = st.elements
            @test eltype(elem_it) == LLVMType

            @test length(elem_it) == length(elem)

            @test first(elem_it) == elem[1]
            @test last(elem_it) == elem[end]
            @test_throws BoundsError elem_it[3]

            i = 1
            for el in elem_it
                @test el == elem[i]
                i += 1
            end

            @test collect(elem_it) == elem
        end
    end

    let st = LLVM.StructType("foo")
        @test st.name == "foo"
        @test isopaque(st)
        elements!(st, elem)
        @test collect(st.elements) == elem
        @test !isopaque(st)
    end
end

# other
@dispose ctx=Context() begin
    typ = LLVM.VoidType()
    @test context(typ) == ctx
end
@dispose ctx=Context() begin
    typ = LLVM.LabelType()
    @test context(typ) == ctx
end
@dispose ctx=Context() begin
    typ = LLVM.MetadataType()
    @test context(typ) == ctx
end
@dispose ctx=Context() begin
    typ = LLVM.TokenType()
    @test context(typ) == ctx
end

# type iteration
@dispose ctx=Context() begin
    st = LLVM.StructType("SomeType")

    let ts = ctx.types
        @test keytype(ts) == String
        @test valtype(ts) == LLVMType

        @test haskey(ts, "SomeType")
        @test ts["SomeType"] == st

        @test !haskey(ts, "SomeOtherType")
        @test_throws KeyError ts["SomeOtherType"]
    end
end

end


@testset "value" begin

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, entry)
    @test entry.name == "entry"

    typ = LLVM.Int32Type()
    val = alloca!(builder, typ, "foo")
    @test context(val) == ctx
    @test typeof(val.ref) == LLVM.API.LLVMValueRef                # untyped

    @test typeof(LLVM.Instruction(val.ref)) == LLVM.AllocaInst    # type reconstructed
    if LLVM.typecheck_enabled
        @test_throws ErrorException LLVM.Function(val.ref)        # wrong
    end
    @test_throws UndefRefError LLVM.Function(LLVM.API.LLVMValueRef(C_NULL))

    @test typeof(Value(val.ref)) == LLVM.AllocaInst               # type reconstructed
    @test_throws UndefRefError Value(LLVM.API.LLVMValueRef(C_NULL))

    # abstractly-typed values are converted without knowing their concrete type
    vals = Value[val, fn, fn.parameters[1], ConstantInt(Int32(1))]
    @test all(v -> Base.unsafe_convert(LLVM.API.LLVMValueRef, v) === v.ref, vals)
    @test Base.cconvert(Ptr{LLVM.API.LLVMValueRef}, vals) == [v.ref for v in vals]

    # wrapper types need to consist of a single reference
    @eval struct InvalidValue <: LLVM.Value
        ref::LLVM.API.LLVMValueRef
        data::Int
    end
    @test_throws ErrorException LLVM.register(InvalidValue, LLVM.API.LLVMArgumentValueKind)
    @test typeof(Value(fn.parameters[1].ref)) == LLVM.Argument

    show(devnull, val)

    @test val.value_type == LLVM.PointerType(typ)
    @test_throws ErrorException sizeof(val)
    @test val.name == "foo"
    @test !isconstant(val)
    @test !isundef(val)

    val.name = "bar"
    @test val.name == "bar"
end

# usage

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, entry)

    valueinst1 = add!(builder, fn.parameters[1],
                      ConstantInt(Int32(1)))
    @test !isterminator(valueinst1)

    userinst = add!(builder, valueinst1,
                    ConstantInt(Int32(1)))

    # use iteration
    let usepairs = valueinst1.uses
        @test eltype(usepairs) == Use

        usepair = first(usepairs)
        @test usepair.value == valueinst1
        @test usepair.user == userinst

        for _usepair in usepairs
            @test usepair == _usepair
        end

        @test [use.value for use in usepairs] == [valueinst1]
        @test [use.user for use in usepairs] == [userinst]
    end

    valueinst2 = add!(builder, fn.parameters[1],
                    ConstantInt(Int32(2)))

    replace_uses!(valueinst1, valueinst2)
    @test [use.user for use in valueinst2.uses] == [userinst]
end

# users

@dispose ctx=Context() begin
    # operand iteration
    mod = parse(LLVM.Module,  """
        define void @fun1(i32) {
        top:
          %1 = add i32 %0, 1
          ret void
        }

        declare void @fun2()""")
    fun = mod.functions["fun1"]

    for (i, instr) in enumerate(first(fun.blocks).instructions)
        ops = instr.operands
        @test eltype(ops) == Value
        if i == 1
            @test length(ops) == 2
            @test ops[1] == first(fun.parameters)
            @test ops[2] == ConstantInt(LLVM.Int32Type(), 1)
            @test_throws BoundsError ops[3]
        elseif i == 2
            @test length(ops) == 0
            @test collect(ops) == []
        end
    end

    fun = mod.functions["fun2"]

    @test_throws BoundsError first(fun.blocks)
    @test_throws BoundsError last(fun.blocks)
    @test_throws BoundsError fun.blocks[1]

    dispose(mod)
end

# constants

@dispose ctx=Context() begin
    @testset "constants" begin

    typ = LLVM.Int32Type()
    ptrtyp = LLVM.PointerType(typ)

    let val = null(typ)
        @test isnull(val)
    end

    let val = all_ones(typ)
        @test !isnull(val)
    end

    let val = PointerNull(ptrtyp)
        @test isnull(val)
    end

    let val = UndefValue(typ)
        @test isundef(val)
        @test val isa LLVM.Constant
    end

    let val = PoisonValue(typ)
        @test ispoison(val)
        @test val isa LLVM.Constant
    end

    end
end

# scalar
@dispose ctx=Context() begin
    @testset "integer constants" begin

    # manual construction of small values
    let
        typ = LLVM.Int32Type()
        constval = ConstantInt(typ, -1)
        @test convert(Int, constval) == -1
        @test convert(UInt32, constval) == typemax(UInt32)
    end

    # manual construction of large values
    let
        typ = LLVM.Int64Type()
        constval = ConstantInt(typ, BigInt(2)^100-1)
        @test convert(Int, constval) == -1
    end

    # automatic construction
    let
        constval = ConstantInt(UInt32(1))
        @test convert(UInt, constval) == 1
    end
    let
        constval = ConstantInt(false)
        @test constval.value_type == LLVM.Int1Type()
        @test !convert(Bool, constval)

        constval = ConstantInt(true)
        @test convert(Bool, constval)
    end

    # issue #81
    for T in [Int32, UInt32, Int64, UInt64]
        constval = ConstantInt(typemax(T))
        @test convert(T, constval) == typemax(T)
    end

    end


    @testset "floating point constants" begin

    let
        typ = LLVM.HalfType()
        c = ConstantFP(typ, Float16(1.1f0))
        @test convert(Float16, c) == Float16(1.1f0)
    end
    let
        typ = LLVM.FloatType()
        c = ConstantFP(typ, 1.1f0)
        @test convert(Float32, c) == 1.1f0
    end
    let
        typ = LLVM.DoubleType()
        c = ConstantFP(typ, 1.1)
        @test convert(Float64, c) == 1.1
    end
    let
        typ = LLVM.BFloatType()
        c = ConstantFP(typ, BFloat16(1.1))
        @test convert(BFloat16, c) == BFloat16(1.1)
        d = ConstantFP(BFloat16(1.1))
        @test convert(BFloat16, d) == BFloat16(1.1)
    end
    let
        typ = LLVM.X86FP80Type()
        c = ConstantFP(typ, 1.1)
        @test convert(Float64, c) == 1.1
    end
    for T in [LLVM.FP128Type, LLVM.PPCFP128Type]
        typ = T()
        c = ConstantFP(typ, 1.1)
        @test convert(Float64, c) == 1.1
    end

    # from and to bit patterns
    for (typ, bits) in [(LLVM.HalfType(), 0x3c00), (LLVM.BFloatType(), 0x3f80),
                        (LLVM.FloatType(), 0x3f800000),
                        (LLVM.DoubleType(), 0x3ff0000000000000),
                        (LLVM.X86FP80Type(), UInt128(0x3fff) << 64 | 0x8000000000000000),
                        (LLVM.FP128Type(), UInt128(0x3fff) << 112),
                        (LLVM.PPCFP128Type(), UInt128(0x3ff0000000000000))]
        c = ConstantFP(typ; bits)
        @test c.value_type == typ
        @test convert(Float64, c) == 1.0
        @test c.bitpattern === bits
        @test ConstantFP(typ, 1.0).bitpattern === bits
        # patterns can be passed using wider integers
        @test ConstantFP(typ; bits=UInt128(bits)).bitpattern === bits
    end
    let
        # full-precision constants of wider types
        bits = 0x3ffb999999999999999999999999999a  # 0.1
        c = ConstantFP(LLVM.FP128Type(); bits)
        @test c.bitpattern == bits
        @test ConstantFP(LLVM.FP128Type(), 0.1).bitpattern != bits
        @check_ir c "fp128 0xL999999999999999A3FFB999999999999"
    end
    let
        # NaN payloads
        c = ConstantFP(LLVM.FloatType(); bits=0x7fa00001)
        @test isnan(convert(Float32, c))
        @test c.bitpattern === 0x7fa00001
    end
    @test_throws ArgumentError ConstantFP(LLVM.HalfType(); bits=0x10000)
    @test_throws ArgumentError ConstantFP(LLVM.X86FP80Type(); bits=UInt128(1) << 80)
    for T in [Float16, Float32, Float64]
        c = ConstantFP(typemax(T))
        @test convert(T, c) == typemax(T)
    end

    end


    @testset "array aggregate constants" begin

    # from Julia values
    let
        vec = Int128[1,2,3,4]
        ca = ConstantArray(vec)
        @test ca isa ConstantArray
        @test size(vec) == size(ca)
        @test length(vec) == length(ca)
        @test ca[1] == ConstantInt(vec[1])
        @test collect(ca) == ConstantInt.(vec)
    end
    let
        # tests for ConstantAggregateZero, constructed indirectly.
        # should behave similarly to ConstantArray since it can get returned there.
        ca = ConstantArray(Int[])
        @test ca isa ConstantAggregateZero
        @test size(ca) == (0,)
        @test length(ca) == 0
        @test isempty(collect(ca))
    end

    # multidimensional
    let
        vec = rand(Int, 2,3,4)
        ca = ConstantArray(vec)
        @test size(vec) == size(ca)
        @test length(vec) == length(ca)
        @test collect(ca) == ConstantInt.(vec)
    end

    # multidimensional, with rows that aren't stored as packed data
    let
        mod = parse(LLVM.Module, "@g = global [2 x [2 x i32]] [[2 x i32] zeroinitializer, [2 x i32] [i32 1, i32 2]]")
        ca = mod.globals["g"].initializer
        @test ca isa ConstantArray
        @test convert.(Int, collect(ca)) == [0 0; 1 2]
        dispose(mod)
    end

    end

    @testset "struct aggregate constants" begin

    # from Julia values
    let
        test_struct = TestStruct(true, -99, 1.5)
        constant_struct = ConstantStruct(test_struct, anonymous=true)
        constant_struct_type = constant_struct.value_type

        @test constant_struct_type isa LLVM.StructType
        @test context(constant_struct) == ctx
        @test !ispacked(constant_struct_type)
        @test !isopaque(constant_struct_type)

        @test collect(constant_struct_type.elements) ==
            [LLVM.Int1Type(), LLVM.Int64Type(), LLVM.HalfType()]

        expected_operands = [
            ConstantInt(LLVM.Int1Type(), Int(true)),
            ConstantInt(LLVM.Int64Type(), -99),
            ConstantFP(LLVM.HalfType(), 1.5)
        ]
        @test collect(constant_struct.operands) == expected_operands
    end
    let
        test_struct = TestStruct(false, 52, -2.5)
        constant_struct = ConstantStruct(test_struct)
        constant_struct_type = constant_struct.value_type

        @test constant_struct_type isa LLVM.StructType

        expected_operands = [
            ConstantInt(LLVM.Int1Type(), Int(false)),
            ConstantInt(LLVM.Int64Type(), 52),
            ConstantFP(LLVM.HalfType(), -2.5)
        ]
        @test collect(constant_struct.operands) == expected_operands

        # re-creating the same type shouldn't fail
        ConstantStruct(TestStruct(true, 42, 0))
        # unless it's a conflicting type
        @test_throws ArgumentError ConstantStruct(AnotherTestStruct(1), "TestStruct")

    end
    let
        test_struct = TestSingleton()
        constant_struct = ConstantStruct(test_struct)
        constant_struct_type = constant_struct.value_type

        @test isempty(constant_struct.operands)
    end
    let
        @test_throws ArgumentError ConstantStruct(1)
    end

    end


    @testset "array data constants" begin

    let
        vec = Int32[1,2,3,4]
        eltyp = LLVM.Int32Type()
        cda = ConstantDataArray(eltyp, vec)
        @test cda isa ConstantDataArray
        @test cda.value_type == LLVM.ArrayType(eltyp, 4)
        @test collect(cda) == ConstantInt.(vec)
    end

    # from Julia values
    for T in [Int8, Int16, Int32, Int64]
        vec = T[1,2,3,4]
        cda = ConstantDataArray(vec)
        @test cda isa ConstantDataArray
        @test size(vec) == size(cda)
        @test collect(cda) == ConstantInt.(vec)
    end
    for T in [Float32, Float64, BFloat16]
        vec = if T == BFloat16
            # LLVM 16 cannot select the vectorized integer conversion that `T[1,2,3,4]`
            # compiles to on hosts with AVX512BF16 (JuliaMath/BFloat16s.jl#107)
            reinterpret(BFloat16, UInt16[0x3f80, 0x4000, 0x4040, 0x4080])
        else
            T[1,2,3,4]
        end
        cda = ConstantDataArray(vec)
        @test cda isa ConstantDataArray
        @test size(vec) == size(cda)
        @test collect(cda) == ConstantFP.(vec)
    end

    # from vectors that aren't stored contiguously
    for vec in [Int32(1):Int32(3), view(Int32[1,0,2,0,3], 1:2:5),
                reinterpret(Int32, Int64[1, 2])]
        cda = ConstantDataArray(vec)
        @test size(cda) == size(vec)
        @test collect(cda) == ConstantInt.(vec)
    end

    # unsupported element types
    @test_throws ArgumentError ConstantDataArray([true, false])
    @test_throws ArgumentError ConstantDataArray(LLVM.IntType(24), Int32[1, 2])
    @test_throws ArgumentError ConstantDataArray(LLVM.Int16Type(), Int32[1, 2])
    @test_throws ArgumentError ConstantDataArray(LLVM.FP128Type(), Float64[1, 2])
    @test_throws ArgumentError ConstantDataArray(LLVM.Int16Type(), Union{Int8,Int16}[Int8(-1)])

    end
end

# constant expressions
@dispose ctx=Context() begin
    @testset "constant expressions" begin

    # inline assembly
    if supports_typed_pointers(ctx)
        let
            ft = LLVM.FunctionType(LLVM.VoidType())
            asm = InlineAsm(ft, "nop", "", false)
            @check_ir asm "void ()* asm \"nop\", \"\""
        end
    else
        let
            ft = LLVM.FunctionType(LLVM.VoidType())
            asm = InlineAsm(ft, "nop", "", false)
            @check_ir asm "ptr asm \"nop\", \"\""
        end
    end

    # integer
    let
        val = LLVM.ConstantInt(Int32(42))

        for f = [const_neg, const_nswneg]
            ce = f(val)::LLVM.Constant
            @check_ir ce "i32 -42"
        end

        ce = const_not(val)::LLVM.Constant
        @check_ir ce "i32 -43"

        other_val = LLVM.ConstantInt(Int32(2))

        for f in [const_add, const_nswadd, const_nuwadd]
            ce = f(val, other_val)::LLVM.Constant
            @check_ir ce "i32 44"
        end

        for f in [const_sub, const_nswsub, const_nuwsub]
            ce = f(val, other_val)::LLVM.Constant
            @check_ir ce "i32 40"
        end

        if LLVM.version() < v"21"
            for f in [const_mul, const_nswmul, const_nuwmul]
                ce = f(val, other_val)::LLVM.Constant
                @check_ir ce "i32 84"
            end
        end

        ce = const_xor(val, other_val)::LLVM.Constant
        @check_ir ce "i32 40"

        if LLVM.version() < v"19"
            ce = const_icmp(LLVM.API.LLVMIntUGT, val, other_val)::LLVM.Constant
            @check_ir ce "i1 true"

            ce = const_shl(val, other_val)::LLVM.Constant
            @check_ir ce "i32 168"
        end

        for f in [const_trunc, const_truncorbitcast]
            ce = const_trunc(val, LLVM.Int16Type())::LLVM.Constant
            @check_ir ce "i16 42"
        end

        ce = const_bitcast(val, LLVM.FloatType())::LLVM.Constant
        @check_ir ce "float 0x36F5000000000000"

        if LLVM.version() < v"18"
            ce = const_and(val, other_val)::LLVM.Constant
            @check_ir ce "i32 2"

            ce = const_or(val, other_val)::LLVM.Constant
            @check_ir ce "i32 42"

            for f in [const_uitofp, const_sitofp]
                ce = f(val, LLVM.FloatType())::LLVM.Constant
                @check_ir ce "float 4.200000e+01"
            end

            for f in [const_sext, const_zext]
                ce = f(val, LLVM.Int64Type())::LLVM.Constant
                @check_ir ce "i64 42"
            end

            for f in [const_lshr, const_ashr]
                ce = f(val, other_val)::LLVM.Constant
                @check_ir ce "i32 10"
            end

            for f in [const_sextorbitcast, const_zextorbitcast]
                ce = f(val, LLVM.Int64Type())::LLVM.Constant
                @check_ir ce "i64 42"
            end

            ce = const_intcast(val, LLVM.Int64Type(), true)::LLVM.Constant
            @check_ir ce "i64 42"

            ce = const_intcast(val, LLVM.Int16Type(), true)::LLVM.Constant
            @check_ir ce "i16 42"
        end
    end

    # floating-point
    let
        val = LLVM.ConstantFP(Float32(42.); )

        other_val = LLVM.ConstantFP(Float32(2.))
        if LLVM.version() < v"19"
            ce = const_fcmp(LLVM.API.LLVMRealUGT, val, other_val)::LLVM.Constant
            @check_ir ce "i1 true"
        end

        if LLVM.version() < v"18"
            for f in [const_fptoui, const_fptosi]
                ce = const_fptoui(val, LLVM.Int32Type())::LLVM.Constant
                @check_ir ce "i32 42"
            end

            for f in [const_fptrunc, const_fpcast]
                ce = f(val, LLVM.HalfType())::LLVM.Constant
                @check_ir ce "half 0xH5140"
            end

            for f in [const_fpext, const_fpcast]
                ce = f(val, LLVM.DoubleType())::LLVM.Constant
                @check_ir ce "double 4.200000e+01"
            end
        end
    end

    # pointer
    let
        ptr = LLVM.PointerNull(LLVM.PointerType(LLVM.Int32Type()))

        ce = const_ptrtoint(ptr, LLVM.Int32Type())::LLVM.Constant
        @check_ir ce "i32 0"

        ce = const_inttoptr(ce, ptr.value_type)::LLVM.Constant
        if supports_typed_pointers(ctx)
            @check_ir ce "i32* null"
        else
            @check_ir ce "ptr null"
        end
        @test isempty(ptr.uses)
        for f in [const_addrspacecast, const_pointercast]
            ce = f(ptr, LLVM.PointerType(LLVM.Int32Type(), 1))::LLVM.Constant
            if supports_typed_pointers(ctx)
                @check_ir ce "i32 addrspace(1)* addrspacecast (i32* null to i32 addrspace(1)*)"
            else
                @check_ir ce "ptr addrspace(1) addrspacecast (ptr null to ptr addrspace(1))"
            end
            # deletion of a constant
            if LLVM.version() < v"21"
                @test !isempty(ptr.uses)
            else
                # LLVM 21+ removed uselist from constants
                @test isempty(ptr.uses)
            end
            LLVM.unsafe_destroy!(ce)
            @test isempty(ptr.uses)
        end
    end

    # gep, inbounds_gep, select, extractelement, insertelement, shufflevector, exactvalue, insertvalue

    end
end

# convert_users_to_instructions!
if LLVM.version() >= v"17"
@testset "convert users to instructions" begin

# a global used through a constant-expression GEP inside a function
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    T_i32 = LLVM.Int32Type()
    T_arr = LLVM.ArrayType(T_i32, 4)
    gv = GlobalVariable(mod, T_arr, "gv")

    fn = LLVM.Function(mod, "f", LLVM.FunctionType(T_i32, LLVM.LLVMType[]))
    position!(builder, BasicBlock(fn, "entry"))
    ce = const_gep(T_arr, gv, LLVM.Constant[ConstantInt(Int32(0)), ConstantInt(Int32(2))])
    loadinst = load!(builder, T_i32, ce)
    ret!(builder, loadinst)

    # before: the load's pointer operand is a constant expression
    @test loadinst.operands[1] isa LLVM.ConstantExpr

    @test convert_users_to_instructions!(LLVM.Constant[gv])

    # after: the operand is an instruction, and the dead constexpr is gone
    @test loadinst.operands[1] isa LLVM.Instruction
    @check_ir loadinst.operands[1] "getelementptr"
    @test all(u -> u.user isa LLVM.Instruction, gv.uses)

    # calling again is a no-op
    @test !convert_users_to_instructions!(LLVM.Constant[gv])
end

# a global used directly (no constant expression): a no-op
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    T_i32 = LLVM.Int32Type()
    gv = GlobalVariable(mod, T_i32, "gv")

    fn = LLVM.Function(mod, "f", LLVM.FunctionType(T_i32, LLVM.LLVMType[]))
    position!(builder, BasicBlock(fn, "entry"))
    ret!(builder, load!(builder, T_i32, gv))

    @test !convert_users_to_instructions!(LLVM.Constant[gv])
end

# a phi whose incoming value is a constant expression: the materialized
# instruction lands in the incoming block, not in the phi's block
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    T_i32 = LLVM.Int32Type()
    T_ptr = LLVM.PointerType(T_i32)
    T_arr = LLVM.ArrayType(T_i32, 4)
    gv = GlobalVariable(mod, T_arr, "gv")

    fn = LLVM.Function(mod, "f", LLVM.FunctionType(T_i32, [LLVM.Int1Type()]))
    entry = BasicBlock(fn, "entry")
    left = BasicBlock(fn, "left")
    merge = BasicBlock(fn, "merge")

    position!(builder, entry)
    br!(builder, fn.parameters[1], left, merge)
    position!(builder, left)
    br!(builder, merge)
    position!(builder, merge)
    ce = const_gep(T_arr, gv, LLVM.Constant[ConstantInt(Int32(0)), ConstantInt(Int32(2))])
    phi = phi!(builder, T_ptr)
    append!(phi.incoming, [(ce, left), (null(T_ptr), entry)])
    ret!(builder, load!(builder, T_i32, phi))

    @test convert_users_to_instructions!(LLVM.Constant[gv])

    hasgep(bb) = any(inst -> occursin("getelementptr", string(inst)), bb.instructions)
    @test hasgep(left)    # materialized in the incoming block
    @test !hasgep(merge)  # not in the phi's own block
end

# options that require LLVM 19+
if LLVM.version() >= v"19"

# `func` restricts the rewrite to a single function
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    T_i32 = LLVM.Int32Type()
    T_arr = LLVM.ArrayType(T_i32, 4)
    gv = GlobalVariable(mod, T_arr, "gv")
    ft = LLVM.FunctionType(T_i32, LLVM.LLVMType[])
    idxs = LLVM.Constant[ConstantInt(Int32(0)), ConstantInt(Int32(2))]

    f1 = LLVM.Function(mod, "f1", ft)
    position!(builder, BasicBlock(f1, "entry"))
    l1 = load!(builder, T_i32, const_gep(T_arr, gv, idxs))
    ret!(builder, l1)

    f2 = LLVM.Function(mod, "f2", ft)
    position!(builder, BasicBlock(f2, "entry"))
    l2 = load!(builder, T_i32, const_gep(T_arr, gv, idxs))
    ret!(builder, l2)

    # (both loads share the same uniqued constant expression)
    @test convert_users_to_instructions!(LLVM.Constant[gv]; func=f1)
    @test l1.operands[1] isa LLVM.Instruction   # rewritten in f1
    @test l2.operands[1] isa LLVM.ConstantExpr   # untouched in f2
end

# `include_self` also converts the passed constants themselves
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    T_i32 = LLVM.Int32Type()
    T_arr = LLVM.ArrayType(T_i32, 4)
    gv = GlobalVariable(mod, T_arr, "gv")

    fn = LLVM.Function(mod, "f", LLVM.FunctionType(T_i32, LLVM.LLVMType[]))
    position!(builder, BasicBlock(fn, "entry"))
    ce = const_gep(T_arr, gv, LLVM.Constant[ConstantInt(Int32(0)), ConstantInt(Int32(1))])
    loadinst = load!(builder, T_i32, ce)
    ret!(builder, loadinst)

    # without include_self, only (constant) users of `ce` are considered: none
    @test !convert_users_to_instructions!(LLVM.Constant[ce]; include_self=false)
    @test loadinst.operands[1] isa LLVM.ConstantExpr

    # with include_self, the constant expression itself is materialized
    @test convert_users_to_instructions!(LLVM.Constant[ce]; include_self=true)
    @test loadinst.operands[1] isa LLVM.Instruction
end

# `remove_dead_constants=false` keeps the now-dead constant expression around
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    T_i32 = LLVM.Int32Type()
    T_arr = LLVM.ArrayType(T_i32, 4)
    gv = GlobalVariable(mod, T_arr, "gv")

    fn = LLVM.Function(mod, "f", LLVM.FunctionType(T_i32, LLVM.LLVMType[]))
    position!(builder, BasicBlock(fn, "entry"))
    ce = const_gep(T_arr, gv, LLVM.Constant[ConstantInt(Int32(0)), ConstantInt(Int32(2))])
    ret!(builder, load!(builder, T_i32, ce))

    @test convert_users_to_instructions!(LLVM.Constant[gv]; remove_dead_constants=false)
    # the dead constant expression is retained as a user of `gv`
    @test any(u -> u.user isa LLVM.ConstantExpr, gv.uses)
end

else

# the extra options are rejected before LLVM 19
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    gv = GlobalVariable(mod, LLVM.Int32Type(), "gv")
    @test_throws ArgumentError convert_users_to_instructions!(LLVM.Constant[gv]; include_self=true)
    @test_throws ArgumentError convert_users_to_instructions!(LLVM.Constant[gv]; func=nothing, remove_dead_constants=false)
end

end

end
end

# global values
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    st = LLVM.StructType("SomeType")
    # LLVM 21 disallows opaque types as SSA values, so give the struct a body.
    elements!(st, [LLVM.Int32Type()])
    ft = LLVM.FunctionType(st, [st])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    @test isdeclaration(fn)
    @test fn.linkage == LLVM.API.LLVMExternalLinkage
    fn.linkage = LLVM.API.LLVMAvailableExternallyLinkage
    @test fn.linkage == LLVM.API.LLVMAvailableExternallyLinkage

    @test fn.section == ""
    fn.section = "SomeSection"
    @test fn.section == "SomeSection"

    @test fn.visibility == LLVM.API.LLVMDefaultVisibility
    fn.visibility = LLVM.API.LLVMHiddenVisibility
    @test fn.visibility == LLVM.API.LLVMHiddenVisibility

    @test fn.dllstorage == LLVM.API.LLVMDefaultStorageClass
    fn.dllstorage = LLVM.API.LLVMDLLImportStorageClass
    @test fn.dllstorage == LLVM.API.LLVMDLLImportStorageClass

    @test fn.unnamed_addr == LLVM.API.LLVMNoUnnamedAddr
    fn.unnamed_addr = LLVM.API.LLVMGlobalUnnamedAddr
    @test fn.unnamed_addr == LLVM.API.LLVMGlobalUnnamedAddr
    @check_ir fn " unnamed_addr"
    fn.unnamed_addr = LLVM.API.LLVMLocalUnnamedAddr
    @test fn.unnamed_addr == LLVM.API.LLVMLocalUnnamedAddr
    @check_ir fn " local_unnamed_addr"
    fn.unnamed_addr = LLVM.API.LLVMNoUnnamedAddr
    @test fn.unnamed_addr == LLVM.API.LLVMNoUnnamedAddr
    @test_throws MethodError fn.unnamed_addr = true

    str = MDString("bar")
    md = MDNode([str])
    @test isempty(fn.metadata)
    @test !haskey(fn.metadata, "foo")
    @test_throws KeyError fn.metadata["foo"]
    fn.metadata["foo"] = md
    @test !isempty(fn.metadata)
    @test haskey(fn.metadata, "foo")
    @test fn.metadata["foo"] == md
    @test collect(values(fn.metadata)) == [md]
    delete!(fn.metadata, "foo")
    @test isempty(fn.metadata)
    fn.metadata["foo"] = md
    @test !isempty(fn.metadata)
    empty!(fn.metadata)
    @test isempty(fn.metadata)
end

# global variables
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    @test isempty(mod.globals)
    gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")
    @test !isempty(mod.globals)

    show(devnull, gv)

    @test gv.initializer === nothing
    init = ConstantInt(Int32(0))
    gv.initializer = init
    @test gv.initializer == init
    gv.initializer = nothing
    @test gv.initializer === nothing

    # `threadlocal` is a Bool view of `threadlocal_mode`
    @test !gv.threadlocal
    @test gv.threadlocal_mode == LLVM.API.LLVMNotThreadLocal
    gv.threadlocal = true
    @test gv.threadlocal
    @test gv.threadlocal_mode == LLVM.API.LLVMGeneralDynamicTLSModel
    @check_ir gv "thread_local global"
    gv.threadlocal_mode = LLVM.API.LLVMLocalExecTLSModel
    @test gv.threadlocal
    @check_ir gv "thread_local(localexec) global"
    gv.threadlocal = true       # doesn't replace a more specific model
    @test gv.threadlocal_mode == LLVM.API.LLVMLocalExecTLSModel
    gv.threadlocal = false
    @test !gv.threadlocal
    @test gv.threadlocal_mode == LLVM.API.LLVMNotThreadLocal
    gv.threadlocal = true

    @test !gv.constant
    gv.constant = true
    @test gv.constant
    @check_ir gv "constant i32"
    gv.constant = false
    @test !gv.constant
    # `isconstant` checks whether a value is a constant, which a global variable is
    @test isconstant(gv)

    @test !gv.externally_initialized
    gv.externally_initialized = true
    @test gv.externally_initialized
    @check_ir gv "externally_initialized global"
    gv.externally_initialized = false
    @test !gv.externally_initialized

    @test gv.alignment == 0
    gv.alignment = 4
    @test gv.alignment == 4
    @test_throws ArgumentError gv.alignment = 3
    gv.alignment = 0
    @test gv.alignment == 0

    @test gv.threadlocal_mode == LLVM.API.LLVMGeneralDynamicTLSModel
    gv.threadlocal_mode = LLVM.API.LLVMNotThreadLocal
    @test gv.threadlocal_mode == LLVM.API.LLVMNotThreadLocal

    # the used lists are sets of global values, stored in a special global variable
    for (set, name) in ((mod.used, "llvm.used"), (mod.compiler_used, "llvm.compiler.used"))
        @test isempty(set)
        @test !haskey(mod.globals, name)
        fn = LLVM.Function(mod, "used_function", LLVM.FunctionType(LLVM.VoidType()))
        @test push!(set, gv) === set
        @test haskey(mod.globals, name)
        @test mod.globals[name].linkage == LLVM.API.LLVMAppendingLinkage
        union!(set, [fn, gv])   # duplicates are ignored
        @test length(set) == 2
        @test collect(set) == [gv, fn]
        @test gv in set && fn in set
        @test delete!(set, gv) === set
        @test collect(set) == [fn]
        @test !(gv in set)
        delete!(set, gv)        # deleting a value that isn't in the set is a no-op
        @test length(set) == 1
        setdiff!(set, [fn])
        @test isempty(set)
        @test !haskey(mod.globals, name)
        push!(set, fn)
        @test empty!(set) === set
        @test isempty(set)
        erase!(fn)
    end

    # both lists are independent
    push!(mod.used, gv)
    @test gv in mod.used && !(gv in mod.compiler_used)
    empty!(mod.used)

    # lists created elsewhere may contain duplicates, which are only reported once
    list = ConstantArray(gv.value_type, [gv, gv])
    used = GlobalVariable(mod, list.value_type, "llvm.used")
    used.initializer = list
    used.linkage = LLVM.API.LLVMAppendingLinkage
    @test length(mod.used) == 1
    @test collect(mod.used) == [gv]
    delete!(mod.used, gv)
    @test isempty(mod.used)
    @test !haskey(mod.globals, "llvm.used")

    let gvars = mod.globals
        @test gv in gvars
        erase!(gv)
        @test isempty(gvars)
    end
end

@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    st = LLVM.StructType("SomeType")
    gv = GlobalVariable(mod, st, "SomeGlobal")

    init = null(st)
    gv.initializer = init
    @test gv.initializer == init
end

@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal", 1)

    @test gv.value_type isa LLVM.PointerType
    @test gv.value_type.addrspace == 1

    @test gv.global_value_type == LLVM.Int32Type()
end

# global aliases
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")
    gv.initializer = ConstantInt(Int32(42))

    ga = GlobalAlias(mod, LLVM.Int32Type(), gv, "SomeAlias")
    @test ga isa GlobalAlias
    @test ga isa GlobalValue
    @test !(ga isa LLVM.GlobalObject)
    show(devnull, ga)

    @test ga.name == "SomeAlias"
    @test ga.parent == mod
    @test ga.global_value_type == LLVM.Int32Type()
    @test ga.value_type == gv.value_type
    @test ga.linkage == LLVM.API.LLVMExternalLinkage
    @test ga.aliasee == gv

    # the type-inferring constructor
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)
    @dispose builder=IRBuilder() begin
        position!(builder, BasicBlock(fn, "entry"))
        ret!(builder)
    end
    fa = GlobalAlias(mod, fn, "SomeFunctionAlias")
    @test fa.global_value_type == ft
    @test fa.aliasee == fn

    # aliasee can be changed, but only to a value of the same type
    other_gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeOtherGlobal")
    other_gv.initializer = ConstantInt(Int32(0))
    ga.aliasee = other_gv
    @test ga.aliasee == other_gv
    as1_gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeAS1Global", 1)
    @test_throws ArgumentError ga.aliasee = as1_gv
    @test_throws ArgumentError GlobalAlias(mod, LLVM.Int32Type(), ConstantInt(Int32(0)), "BadAlias")
    if supports_typed_pointers(ctx)
        @test_throws ArgumentError GlobalAlias(mod, LLVM.Int64Type(), gv, "BadAlias")
    end

    # the address space is taken from the aliasee
    as1_gv.initializer = ConstantInt(Int32(0))
    as1_ga = GlobalAlias(mod, as1_gv, "SomeAS1Alias")
    @test as1_ga.value_type.addrspace == 1
    as0_ga = GlobalAlias(mod, LLVM.Int32Type(),
                         const_addrspacecast(as1_gv, LLVM.PointerType(LLVM.Int32Type())),
                         "SomeAS0Alias")
    @test as0_ga.value_type.addrspace == 0
    @test as0_ga.aliasee isa ConstantExpr

    @test verify(mod) === nothing
end

# global ifuncs
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.Int32Type())
    impl = LLVM.Function(mod, "impl", ft)
    other_impl = LLVM.Function(mod, "other_impl", ft)
    resolver_ft = LLVM.FunctionType(impl.value_type)
    resolver_fn = LLVM.Function(mod, "resolver", resolver_ft)
    other_resolver_fn = LLVM.Function(mod, "other_resolver", resolver_ft)
    @dispose builder=IRBuilder() begin
        for (f, ret) in ((impl, ConstantInt(Int32(0))), (other_impl, ConstantInt(Int32(1))),
                         (resolver_fn, impl), (other_resolver_fn, other_impl))
            position!(builder, BasicBlock(f, "entry"))
            ret!(builder, ret)
        end
    end

    ifunc = GlobalIFunc(mod, ft, resolver_fn, "SomeIFunc")
    @test ifunc isa GlobalIFunc
    @test ifunc isa LLVM.GlobalObject
    show(devnull, ifunc)

    @test ifunc.name == "SomeIFunc"
    @test ifunc.global_value_type == ft
    @test ifunc.resolver == resolver_fn

    ifunc.resolver = other_resolver_fn
    @test ifunc.resolver == other_resolver_fn
    as1_gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeAS1Global", 1)
    @test_throws ArgumentError ifunc.resolver = as1_gv
    @test_throws ArgumentError GlobalIFunc(mod, ft, ConstantInt(Int32(0)), "BadIFunc")

    @test verify(mod) === nothing

    @test ifunc in mod.ifuncs
    erase!(ifunc)
    @test isempty(mod.ifuncs)
end

# aliases and ifuncs are recognized when encountered as operands
@dispose ctx=Context() mod=LLVM.Module("SomeModule") builder=IRBuilder() begin
    gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")
    gv.initializer = ConstantInt(Int32(42))
    ga = GlobalAlias(mod, gv, "SomeAlias")

    ft = LLVM.FunctionType(LLVM.Int32Type())
    fn = LLVM.Function(mod, "SomeFunction", ft)
    resolver_fn = LLVM.Function(mod, "resolver", LLVM.FunctionType(fn.value_type))
    position!(builder, BasicBlock(resolver_fn, "entry"))
    ret!(builder, fn)
    ifunc = GlobalIFunc(mod, ft, resolver_fn, "SomeIFunc")

    position!(builder, BasicBlock(fn, "entry"))
    ld = load!(builder, LLVM.Int32Type(), ga)
    call = call!(builder, ft, ifunc)
    ret!(builder, add!(builder, ld, call))

    @test ld.operands[1] isa GlobalAlias
    @test ld.operands[1] == ga
    @test call.called_operand isa GlobalIFunc
    @test call.called_operand == ifunc
    @test ga in [use.user for use in gv.uses]

    @test verify(mod) === nothing
end

end


@testset "metadata" begin

@dispose ctx=Context() begin
    str = MDString("foo")
    @test convert(String, str) == "foo"

    # wrap as Value
    val = Value(str)
    @test val isa LLVM.MetadataAsValue

    # back to Metadata
    md = Metadata(val)
    @test md == str

    # more specific conversion
    @test convert(MDString, val) == str
end

@dispose ctx=Context() begin
    int = ConstantInt(42)
    @test convert(Int, int) == 42

    # wrap as Metadata
    md = Metadata(int)
    @test md isa LLVM.ValueAsMetadata

    # back to Value
    val = Value(md)
    @test val == int

    # more specific conversion
    @test convert(ConstantInt, val) == int
end

@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    f1 = LLVM.Function(mod, "f1", ft)

    push!(mod.metadata["function"].operands, MDNode([f1]))
    @test Value(mod.metadata["function"].operands[1].operands[1]) == f1

    f2 = LLVM.Function(mod, "f2", ft)
    replace_metadata_uses!(f1, f2)
    @test Value(mod.metadata["function"].operands[1].operands[1]) == f2
end

# different type; requires a hack
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft1 = LLVM.FunctionType(LLVM.VoidType())
    f1 = LLVM.Function(mod, "f1", ft1)

    push!(mod.metadata["function"].operands, MDNode([f1]))
    @test Value(mod.metadata["function"].operands[1].operands[1]) == f1

    ft2 = LLVM.FunctionType(LLVM.Int32Type())
    f2 = LLVM.Function(mod, "f2", ft2)
    replace_metadata_uses!(f1, f2)
    @test Value(mod.metadata["function"].operands[1].operands[1]) == f2
end

@dispose ctx=Context() begin
    str = MDString("foo")
    node = MDNode([str])
    ops = node.operands
    @test length(ops) == 1
    @test ops[1] == str
end

# null metadata, represented as null pointers in the API, by `nothing` in Julia
@dispose ctx=Context() begin
    ir = """
            !0 = !{i32 42, null, !"string"}
            !foo = !{!0}
        """
    mod = parse(LLVM.Module, ir)

    foo_md = mod.metadata["foo"].operands[1]
    @test foo_md.operands[1] !== nothing
    @test foo_md.operands[2] === nothing
    @test foo_md.operands[3] !== nothing

    bar_md = MDNode([ConstantInt(Int32(42)), nothing, MDString("string")])
    @test foo_md == bar_md

    dispose(mod)
end

@testset "debuginfo" begin

@dispose ctx=Context() begin
    mod = parse(LLVM.Module, raw"""
        define double @test(i64 signext %0, double %1) !dbg !5 {
        top:
          %2 = sitofp i64 %0 to double, !dbg !7
          %3 = fadd double %2, %1, !dbg !18
          ret double %3, !dbg !17
        }

        !llvm.module.flags = !{!0, !1}
        !llvm.dbg.cu = !{!2}

        !0 = !{i32 2, !"Dwarf Version", i32 4}
        !1 = !{i32 1, !"Debug Info Version", i32 3}
        !2 = distinct !DICompileUnit(language: DW_LANG_Julia, file: !3, producer: "julia", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, nameTableKind: GNU)
        !3 = !DIFile(filename: "promotion.jl", directory: ".")
        !4 = !{}
        !5 = distinct !DISubprogram(name: "+", linkageName: "julia_+_2055", scope: null, file: !3, line: 321, type: !6, scopeLine: 321, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
        !6 = !DISubroutineType(types: !4)
        !7 = !DILocation(line: 94, scope: !8, inlinedAt: !10)
        !8 = distinct !DISubprogram(name: "Float64;", linkageName: "Float64", scope: !9, file: !9, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
        !9 = !DIFile(filename: "float.jl", directory: ".")
        !10 = !DILocation(line: 7, scope: !11, inlinedAt: !13)
        !11 = distinct !DISubprogram(name: "convert;", linkageName: "convert", scope: !12, file: !12, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
        !12 = !DIFile(filename: "number.jl", directory: ".")
        !13 = !DILocation(line: 269, scope: !14, inlinedAt: !15)
        !14 = distinct !DISubprogram(name: "_promote;", linkageName: "_promote", scope: !3, file: !3, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
        !15 = !DILocation(line: 292, scope: !16, inlinedAt: !17)
        !16 = distinct !DISubprogram(name: "promote;", linkageName: "promote", scope: !3, file: !3, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)
        !17 = !DILocation(line: 321, scope: !5)
        !18 = !DILocation(line: 326, scope: !19, inlinedAt: !17)
        !19 = distinct !DISubprogram(name: "+;", linkageName: "+", scope: !9, file: !9, type: !6, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !4)""")

    fun = mod.functions["test"]
    bb = first(collect(fun.blocks))
    inst = first(collect(bb.instructions))

    @test haskey(inst.metadata, "dbg")
    loc = inst.metadata["dbg"]

    @test loc isa DILocation
    @test loc.line == 94
    @test loc.column == 0

    scope = loc.scope
    @test scope isa DISubProgram
    @test scope.line == 0
    @test scope.name == "Float64;"

    file = scope.file
    @test file isa DIFile
    @test file.filename == "float.jl"
    @test file.directory == "."
    @test file.source == ""

    loc = loc.inlined_at
    @test loc isa DILocation
    @test loc.line == 7

    loc = loc.inlined_at
    @test loc isa DILocation

    loc = loc.inlined_at
    @test loc isa DILocation

    loc = loc.inlined_at
    @test loc isa DILocation

    loc = loc.inlined_at
    @test loc === nothing

    dispose(mod)
end

end

end


@testset "module" begin

@dispose ctx=Context() begin
    @dispose mod=LLVM.Module("SomeModule") begin
        @test context(mod) == ctx

        @test mod.name == "SomeModule"
        mod.name = "SomeOtherName"
        @test mod.name == "SomeOtherName"
    end

    LLVM.Module("SomeModule") do mod
    end
end

@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    clone = copy(mod)
    @test mod != clone
    @test context(clone) == ctx
    dispose(clone)

    show(devnull, mod)

    asm = mod.inline_asm
    @test isempty(asm)
    @test String(asm) == ""
    @test push!(asm, "nop") === asm
    @test !isempty(asm)
    @test String(asm) == "nop\n"   # fragments are terminated by a newline
    push!(mod.inline_asm, SubString("nop; ret", 1, 3), "ret\n")
    @test String(asm) == string(asm) == "nop\nnop\nret\n"
    @test occursin("module asm \"ret\"", string(mod))
    @test repr(asm) == "ModuleInlineAsm(\"SomeModule\"): \"nop\\nnop\\nret\\n\""
    @test empty!(asm) === asm
    @test isempty(asm)
    # replacing the assembly, by emptying before adding
    push!(empty!(push!(asm, "nop")), "ret")
    @test String(asm) == "ret\n"

    dummyTriple = "SomeTriple"
    mod.triple = dummyTriple
    @test mod.triple == dummyTriple

    dummyLayout = "e-p:64:64:64"
    mod.datalayout = dummyLayout
    @test string(mod.datalayout) == dummyLayout

    md = Metadata(ConstantInt(42))

    mod_flags = mod.flags
    mod_flags["foobar", LLVM.API.LLVMModuleFlagBehaviorError] = md

    @test occursin("!llvm.module.flags = !{!0}", string(mod))
    @test occursin(r"!0 = !\{i\d+ 1, !\"foobar\", i\d+ 42\}", string(mod))

    @test mod_flags["foobar"] == md
    @test_throws KeyError mod_flags["foobaz"]

    @test mod.sdk_version === nothing
    mod.sdk_version = v"1.2.3"
    @test mod.sdk_version == v"1.2.3"
end

# metadata iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    node = MDNode([MDString("SomeMDString")])

    let mds = mod.metadata
        @test keytype(mds) == String
        @test valtype(mds) == NamedMDNode

        @test !haskey(mds, "SomeMDNode")
        @test !(node in mds["SomeMDNode"].operands)
        @test haskey(mds, "SomeMDNode") # getindex is mutating

        ops = mds["SomeMDNode"].operands
        @test push!(ops, node) === ops
        @test node in mds["SomeMDNode"].operands

        # the operands are a view of the named metadata node
        other = MDNode([MDString("SomeOtherMDString")])
        push!(ops, other)
        @test length(ops) == 2
        @test ops == [node, other]
        @test ops[2] == other
        @test_throws BoundsError ops[3]

        ops[1] = other
        @test mds["SomeMDNode"].operands == [other, other]
        @test_throws BoundsError ops[3] = node

        @test empty!(ops) === ops
        @test isempty(ops)
        @test isempty(mds["SomeMDNode"].operands)

        push!(ops, node)
        @test mds["SomeMDNode"].operands == [node]
        @test collect(ops) == [node]
    end
end

# global variable iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    dummygv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")

    let gvs = mod.globals
        @test eltype(gvs) == typeof(dummygv)

        @test first(gvs) == dummygv
        @test last(gvs) == dummygv

        for gv in gvs
            @test gv == dummygv
        end

        @test collect(gvs) == [dummygv]

        @test haskey(gvs, "SomeGlobal")
        @test gvs["SomeGlobal"] == dummygv

        @test !haskey(gvs, "SomeOtherGlobal")
        @test_throws KeyError gvs["SomeOtherGlobal"]
    end
end

# global variable ordering
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    c = GlobalVariable(mod, LLVM.Int32Type(), "c")
    a = GlobalVariable(mod, LLVM.Int32Type(), "a")
    b = GlobalVariable(mod, LLVM.Int32Type(), "b")
    gvs = mod.globals

    @test [gv.name for gv in gvs] == ["c", "a", "b"]
    move_before(b, c)
    @test [gv.name for gv in gvs] == ["b", "c", "a"]
    move_after(b, a)
    @test [gv.name for gv in gvs] == ["c", "a", "b"]
    move_before(a, a)
    @test [gv.name for gv in gvs] == ["c", "a", "b"]
    @test length(collect(gvs)) == 3

    @test sort!(gvs) === gvs
    @test [gv.name for gv in gvs] == ["a", "b", "c"]
    @test sort!(gvs; rev=true) === gvs
    @test [gv.name for gv in gvs] == ["c", "b", "a"]
    @test all(haskey(gvs, name) for name in ("a", "b", "c"))
    @test occursin(r"(?s)@c.*@b.*@a", string(mod))
end

# global alias and ifunc iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")
    ft = LLVM.FunctionType(gv.value_type)
    resolver_fn = LLVM.Function(mod, "resolver", ft)
    @dispose builder=IRBuilder() begin
        position!(builder, BasicBlock(resolver_fn, "entry"))
        ret!(builder, null(gv.value_type))
    end

    # names are unique across all global values, so use a different prefix for each kind
    for (iter, T, create) in
        ((mod.aliases, GlobalAlias, name -> GlobalAlias(mod, gv, "alias_$name")),
         (mod.ifuncs, GlobalIFunc,
          name -> GlobalIFunc(mod, LLVM.FunctionType(LLVM.VoidType()), resolver_fn, "ifunc_$name")))
        @test eltype(iter) == T
        @test isempty(iter)
        @test_throws BoundsError first(iter)
        @test_throws BoundsError last(iter)

        x = create("x")
        y = create("ÿ")
        @test endswith(y.name, "_ÿ")
        @test !isempty(iter)
        @test collect(iter) == [x, y]
        @test first(iter) == x
        @test last(iter) == y
        @test x.next == y
        @test y.next === nothing
        @test y.prev == x
        @test x.prev === nothing

        @test haskey(iter, y.name)
        @test iter[y.name] == y
        @test !haskey(iter, "z")
        @test_throws KeyError iter["z"]
    end

    # aliases and ifuncs are not global variables or functions
    @test collect(mod.globals) == [gv]
    @test collect(mod.functions) == [resolver_fn]
end

# function iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    st = LLVM.StructType("SomeType")
    elements!(st, [LLVM.Int32Type()])
    ft = LLVM.FunctionType(st, [st])
    @test isempty(mod.functions)

    @test_throws BoundsError first(mod.functions)
    @test_throws BoundsError last(mod.functions)

    dummyfn = LLVM.Function(mod, "SomeFunction", ft)
    let fns = mod.functions
        @test eltype(fns) == LLVM.Function

        @test !isempty(fns)

        @test first(fns) == dummyfn
        @test last(fns) == dummyfn

        for fn in fns
            @test fn == dummyfn
        end

        @test collect(fns) == [dummyfn]

        @test haskey(fns, "SomeFunction")
        @test fns["SomeFunction"] == dummyfn

        @test !haskey(fns, "SomeOtherFunction")
        @test_throws KeyError fns["SomeOtherFunction"]
    end

    anotherfn = LLVM.Function(mod, "SomeOtherFunction", ft)
    @test first(mod.functions) == dummyfn
    @test last(mod.functions) == anotherfn
    @test dummyfn.prev === nothing
    @test dummyfn.next == anotherfn
    @test anotherfn.prev == dummyfn
    @test anotherfn.next === nothing
end

# function ordering
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    c = LLVM.Function(mod, "c", ft)
    a = LLVM.Function(mod, "a", ft)
    b = LLVM.Function(mod, "b", ft)
    fns = mod.functions

    @test [f.name for f in fns] == ["c", "a", "b"]
    move_before(b, c)
    @test [f.name for f in fns] == ["b", "c", "a"]
    move_after(b, a)
    @test [f.name for f in fns] == ["c", "a", "b"]
    move_after(a, a)
    @test [f.name for f in fns] == ["c", "a", "b"]
    @test length(collect(fns)) == 3

    @test sort!(fns) === fns
    @test [f.name for f in fns] == ["a", "b", "c"]
    @test sort!(fns; rev=true) === fns
    @test [f.name for f in fns] == ["c", "b", "a"]
    @test all(haskey(fns, name) for name in ("a", "b", "c"))
    @test occursin(r"(?s)@c.*@b.*@a", string(mod))
end

# textual IR
@dispose ctx=Context() builder=IRBuilder() source_mod=LLVM.Module("SomeModule") begin
    invalid_ir = "invalid"
    @test_throws LLVMException parse(LLVM.Module, invalid_ir)

    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(source_mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, entry)

    ret!(builder)

    verify(source_mod)


    ir = string(source_mod)

    let
        mod = parse(LLVM.Module, ir)
        verify(mod)
        @test haskey(mod.functions, "SomeFunction")
        dispose(mod)
    end
end

# binary bitcode
@dispose ctx=Context() builder=IRBuilder() source_mod=LLVM.Module("SomeModule") begin
    invalid_bitcode = unsafe_wrap(Vector{UInt8}, "invalid")
    invalid_signature = LLVMException("Invalid bitcode signature")
    @test_throws invalid_signature parse(LLVM.Module, invalid_bitcode)
    @test_throws invalid_signature parse(LLVM.Module, invalid_bitcode; lazy=true)

    # contexts we did not create lack our diagnostic handler, which used to make
    # LLVM print the parse error and exit the process
    # (it may be allocated where a disposed context used to be, so don't let memcheck
    #  mistake it for that one)
    let foreign_ctx = LLVM.mark_untracked(Context(LLVM.API.LLVMContextCreate()))
        context!(foreign_ctx) do
            @test_throws invalid_signature parse(LLVM.Module, invalid_bitcode)
            @test_throws invalid_signature parse(LLVM.Module, invalid_bitcode; lazy=true)
        end
        LLVM.API.LLVMContextDispose(foreign_ctx)
    end

    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(source_mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, entry)

    ret!(builder)

    verify(source_mod)


    @dispose bitcode_buf = convert(MemoryBuffer, source_mod) begin
        @dispose mod=parse(LLVM.Module, bitcode_buf) begin
            verify(mod)
            @test haskey(mod.functions, "SomeFunction")
        end
    end


    let bitcode = convert(Vector{UInt8}, source_mod)
        @dispose mod = parse(LLVM.Module, bitcode) begin
            verify(mod)
            @test haskey(mod.functions, "SomeFunction")
        end

        # lazy parse: module header is read but function bodies stay deferred
        let lazy_bitcode = copy(bitcode)  # kept alive for the module's lifetime
            @dispose mod = parse(LLVM.Module, lazy_bitcode; lazy=true) begin
                verify(mod)
                @test haskey(mod.functions, "SomeFunction")
            end
        end

        # a valid header followed by truncated contents fails deeper in the reader
        let truncated_bitcode = bitcode[1:end÷2]
            @test_throws LLVMException parse(LLVM.Module, truncated_bitcode)
            @test_throws LLVMException parse(LLVM.Module, truncated_bitcode; lazy=true)
        end

        mktemp() do path, io
            mark(io)
            @test write(io, source_mod) > 0
            flush(io)
            reset(io)

            @test read(io) == bitcode
        end

        @test String(bitcode) == sprint(write, source_mod)
    end
end

end


@testset "function" begin

# personalities can be other constants referring to a function
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)
    pers_fn = LLVM.Function(mod, "PersonalityFunction",
                            LLVM.FunctionType(LLVM.Int32Type(); vararg=true))

    pers_alias = GlobalAlias(mod, pers_fn, "PersonalityAlias")
    fn.personality = pers_alias
    @test fn.personality == pers_alias
    @test fn.personality isa GlobalAlias

    pers_cast = const_bitcast(pers_fn, LLVM.PointerType(LLVM.Int8Type()))
    fn.personality = pers_cast
    @test fn.personality == pers_cast
    if supports_typed_pointers(ctx)
        @test fn.personality isa ConstantExpr
    end
end

@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    show(devnull, fn)

    @test fn.personality === nothing
    pers_ft = LLVM.FunctionType(LLVM.Int32Type(); vararg=true)
    pers_fn = LLVM.Function(mod, "PersonalityFunction", ft)
    fn.personality = pers_fn
    @test fn.personality == pers_fn
    fn.personality = nothing
    @test fn.personality === nothing
    erase!(pers_fn)

    @test !isintrinsic(fn)

    @test fn.callconv == LLVM.API.LLVMCCallConv
    fn.callconv = LLVM.API.LLVMFastCallConv
    @test fn.callconv == LLVM.API.LLVMFastCallConv

    @test fn.gc == ""
    fn.gc = "SomeGC"
    @test fn.gc == "SomeGC"

    @test fn.alignment == 0
    fn.alignment = 16
    @test fn.alignment == 16
    @check_ir fn "align 16"
    @test_throws ArgumentError fn.alignment = 3
    fn.alignment = 0
    @test fn.alignment == 0

    let fns = mod.functions
        @test fn in fns
        erase!(fn)
        @test isempty(fns)
    end
end

# non-overloaded intrinsic
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    intr_ft = LLVM.FunctionType(LLVM.VoidType())
    intr_fn = LLVM.Function(mod, "llvm.trap", intr_ft)
    @test isintrinsic(intr_fn)

    intr = Intrinsic(intr_fn)
    show(devnull, intr)

    @test !isoverloaded(intr)

    @test intr.name == "llvm.trap"

    ft = LLVM.FunctionType(intr)
    @test ft isa LLVM.FunctionType
    @test ft.return_type == LLVM.VoidType()

    fn = LLVM.Function(mod, intr)
    @test fn isa LLVM.Function

    if supports_typed_pointers(ctx)
        @test eltype(fn.value_type) == ft
    end
    @test isintrinsic(fn)

    @test intr == Intrinsic("llvm.trap")
end

# overloaded intrinsic
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    intr_ft = LLVM.FunctionType(LLVM.DoubleType(), [LLVM.DoubleType()])
    intr_fn = LLVM.Function(mod, "llvm.sin.f64", intr_ft)
    @test isintrinsic(intr_fn)

    intr = Intrinsic(intr_fn)
    show(devnull, intr)

    @test isoverloaded(intr)

    @test intr.name == "llvm.sin"
    @test LLVM.overloaded_name(intr, [LLVM.DoubleType()]) == "llvm.sin.f64"

    ft = LLVM.FunctionType(intr, [LLVM.DoubleType()])
    @test ft isa LLVM.FunctionType
    @test ft.return_type == LLVM.DoubleType()

    fn = LLVM.Function(mod, intr, [LLVM.DoubleType()])
    @test fn isa LLVM.Function
    if supports_typed_pointers(ctx)
        @test eltype(fn.value_type) == ft
    end
    @test isintrinsic(fn)

    @test intr == Intrinsic("llvm.sin")
end

# function and instruction attributes
@dispose ctx=Context() mod=LLVM.Module("SomeModule") builder=LLVM.IRBuilder() begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)
    caller = LLVM.Function(mod, "CallSomeFunction", ft)
    top = LLVM.BasicBlock(caller, "top")
    position!(builder, top)
    instr = call!(builder, ft, fn, LLVM.Value[ fn.parameters... ])

    let attrs = fn.function_attributes, instr_attrs = instr.function_attributes
        @test eltype(attrs) == Attribute
        @test eltype(instr_attrs) == Attribute

        @test length(attrs) == 0
        @test length(instr_attrs) == 0

        let attr = EnumAttribute("sspreq", 0)
            @test attr.kind != 0
            @test attr.value == 0
            push!(attrs, attr)
            @test collect(attrs) == [attr]

            delete!(attrs, attr)
            @test length(attrs) == 0
        end
        let instr_attr = EnumAttribute("sspreq", 0)
            @test instr_attr.kind != 0
            @test instr_attr.value == 0
            push!(instr_attrs, instr_attr)
            @test collect(instr_attrs) == [instr_attr]

            delete!(instr_attrs, instr_attr)
            @test length(instr_attrs) == 0
        end

        let attr = StringAttribute("nounwind", "")
            @test attr.kind == "nounwind"
            @test attr.value == ""
            push!(attrs, attr)
            @test collect(attrs) == [attr]

            delete!(attrs, attr)
            @test length(attrs) == 0
        end
        let instr_attr = StringAttribute("nounwind", "")
            @test instr_attr.kind == "nounwind"
            @test instr_attr.value == ""
            push!(instr_attrs, instr_attr)
            @test collect(instr_attrs) == [instr_attr]

            delete!(instr_attrs, instr_attr)
            @test length(instr_attrs) == 0
        end

        let attr = TypeAttribute("sret", LLVM.Int32Type())
            @test attr.kind != 0
            @test attr.value ==  LLVM.Int32Type()

            push!(attrs, attr)
            @test collect(attrs) == [attr]

            delete!(attrs, attr)
            @test length(attrs) == 0
        end
        let instr_attr = TypeAttribute("sret", LLVM.Int32Type())
            @test instr_attr.kind != 0
            @test instr_attr.value ==  LLVM.Int32Type()

            push!(instr_attrs, instr_attr)
            @test collect(instr_attrs) == [instr_attr]

            delete!(instr_attrs, instr_attr)
            @test length(instr_attrs) == 0
        end

        if LLVM.version() >= v"19"
            let attr = ConstantRangeAttribute("range", 32, UInt64[0], UInt64[100])
                @test attr isa ConstantRangeAttribute
                @test attr.kind != 0
                push!(fn.return_attributes, attr)
                collected = collect(fn.return_attributes)
                @test any(a -> a isa ConstantRangeAttribute, collected)
                delete!(fn.return_attributes, attr)
            end
        end
    end

    for i in 1:length(fn.parameters)
        let attrs = fn.parameter_attributes[i]
            @test eltype(attrs) == Attribute
            @test length(attrs) == 0
        end
    end
    for i in 1:length(instr.arguments)
        let attrs = instr.argument_attributes[i]
            @test eltype(attrs) == Attribute
            @test length(attrs) == 0
        end
    end

    let attrs = fn.return_attributes
        @test eltype(attrs) == Attribute
        @test length(attrs) == 0
    end
    let attrs = instr.return_attributes
        @test eltype(attrs) == Attribute
        @test length(attrs) == 0
    end
end

# memory effects
if LLVM.version() >= v"16"
    locations = LLVM.memory_locations()
    @test :argmem in locations && :inaccessiblemem in locations && :other in locations
    @test (:errnomem in locations) == (LLVM.version() >= v"21")

    # construction and querying
    let effects = MemoryEffects(:read; argmem=:readwrite)
        @test effects[:argmem] == :readwrite
        @test effects[:inaccessiblemem] == :read
        @test effects[:other] == :read
        @test effects.access == :readwrite
    end
    @test MemoryEffects() == MemoryEffects(:none)
    @test all(loc -> MemoryEffects(:write)[loc] == :write, locations)
    @test MemoryEffects(:none).access == :none
    @test MemoryEffects(argmem=:read, inaccessiblemem=:write).access == :readwrite
    @test MemoryEffects(argmem=:read) | MemoryEffects(argmem=:write, other=:read) ==
          MemoryEffects(argmem=:readwrite, other=:read)
    @test MemoryEffects(:read) & MemoryEffects(argmem=:readwrite) == MemoryEffects(argmem=:read)
    @test_throws ArgumentError MemoryEffects(:everything)
    @test_throws ArgumentError MemoryEffects(:everything; (loc => :none for loc in locations)...)
    @test_throws ArgumentError MemoryEffects(argmem=:everything)
    @test_throws ArgumentError MemoryEffects(globalmem=:read)
    @test_throws ArgumentError MemoryEffects(:read)[:globalmem]
    if LLVM.version() < v"21"
        # locations of later LLVM versions are rejected, not approximated
        @test_throws ArgumentError MemoryEffects(errnomem=:read)
        @test_throws ArgumentError MemoryEffects(:read)[:errnomem]
    end

    # printing, mirroring LLVM's syntax
    @test repr(MemoryEffects(:none)) == "MemoryEffects(:none)"
    @test repr(MemoryEffects(:readwrite)) == "MemoryEffects(:readwrite)"
    @test repr(MemoryEffects(argmem=:read)) == "MemoryEffects(argmem=:read)"
    @test repr(MemoryEffects(argmem=:read, inaccessiblemem=:write)) ==
          "MemoryEffects(argmem=:read, inaccessiblemem=:write)"
    @test repr(MemoryEffects(:read; argmem=:none)) == "MemoryEffects(:read; argmem=:none)"
    for effects in (MemoryEffects(:read; argmem=:none), MemoryEffects(inaccessiblemem=:write))
        @test eval(Meta.parse(repr(effects))) == effects
    end

    # compare against LLVM's textual representation, which catches encoding changes
    ir_kinds = Dict(:none => "none", :read => "read", :write => "write",
                    :readwrite => "readwrite")
    others = filter(!=(:other), locations)
    cases = Pair{String,MemoryEffects}[
        "memory(none)" => MemoryEffects(:none),
        "memory(readwrite)" => MemoryEffects(:readwrite),
        "memory(read, argmem: readwrite)" => MemoryEffects(:read; argmem=:readwrite),
        # only `other`
        "memory(write, $(join(("$loc: none" for loc in others), ", ")))" =>
            MemoryEffects(other=:write)
    ]
    # every other location on its own
    for loc in others, kind in (:read, :write)
        push!(cases, "memory($loc: $(ir_kinds[kind]))" => MemoryEffects(; loc => kind))
    end
    @dispose ctx=Context() begin
        ir = join(("declare void @f$i() $str" for (i, (str, _)) in enumerate(cases)), "\n")
        mod = parse(LLVM.Module, ir)
        for (i, (str, effects)) in enumerate(cases)
            f = mod.functions["f$i"]
            @test f.memory_effects == effects
            @test MemoryEffects(only(collect(f.function_attributes))) == effects

            # the attribute we create is printed like the one LLVM parsed
            g = LLVM.Function(mod, "g$i", f.function_type)
            g.memory_effects = effects
            @test occursin(str, string(g))
        end
        @test verify(mod) === nothing
        dispose(mod)
    end

    # functions and call sites
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") builder=IRBuilder() begin
        ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
        fn = LLVM.Function(mod, "SomeFunction", ft)
        @test fn.memory_effects == MemoryEffects(:readwrite)

        fn.memory_effects = MemoryEffects(:read)
        @test fn.memory_effects == MemoryEffects(:read)
        # setting the memory effects again replaces the attribute
        fn.memory_effects = MemoryEffects(argmem=:read)
        @test fn.memory_effects == MemoryEffects(argmem=:read)
        @test length(fn.function_attributes) == 1

        # the property is a view of the function's memory effects
        effects = fn.memory_effects
        @test effects isa FunctionMemoryEffects
        @test MemoryEffects(effects) isa MemoryEffects
        @test MemoryEffects(effects) == effects == MemoryEffects(argmem=:read)
        @test hash(effects) == hash(MemoryEffects(argmem=:read))
        @test effects[:argmem] == :read && effects[:other] == :none
        @test effects.access == :read
        @test repr(effects) == "MemoryEffects(argmem=:read)"
        @test effects | MemoryEffects(other=:write) ==
              MemoryEffects(argmem=:read, other=:write)
        @test effects & MemoryEffects(:write) == MemoryEffects(:none)
        value = MemoryEffects(effects)

        # ... which can be modified in place
        effects[:inaccessiblemem] = :write
        @test fn.memory_effects == MemoryEffects(argmem=:read, inaccessiblemem=:write)
        @test effects.access == :readwrite
        @test occursin("memory(argmem: read, inaccessiblemem: write)", string(fn))
        @test value == MemoryEffects(argmem=:read)  # values don't change
        @test length(fn.function_attributes) == 1
        fn.memory_effects[:argmem] = :none
        @test fn.memory_effects == MemoryEffects(inaccessiblemem=:write)
        @test_throws ArgumentError effects[:globalmem] = :read
        @test_throws ArgumentError effects[:argmem] = :everything

        # ... or replaced wholesale, also with the effects of another function
        other = LLVM.Function(mod, "OtherFunction", ft)
        other.memory_effects[:other] = :read   # starts from `readwrite`
        @test other.memory_effects == MemoryEffects(:readwrite; other=:read)
        other.memory_effects = fn.memory_effects
        @test other.memory_effects == MemoryEffects(inaccessiblemem=:write)
        fn.memory_effects = MemoryEffects(argmem=:read)
        @test other.memory_effects == MemoryEffects(inaccessiblemem=:write)

        attr = EnumAttribute(MemoryEffects(:none))
        @test MemoryEffects(attr) == MemoryEffects(:none)
        @test_throws ArgumentError MemoryEffects(EnumAttribute("nounwind"))
        @test MemoryEffects(fn.function_attributes) == MemoryEffects(argmem=:read)
        @test_throws ArgumentError MemoryEffects(fn.parameter_attributes[1])
        @test_throws ArgumentError MemoryEffects(fn.return_attributes)

        caller = LLVM.Function(mod, "SomeCaller", ft)
        position!(builder, BasicBlock(caller, "entry"))
        call = call!(builder, ft, fn, [caller.parameters[1]])
        ret!(builder)

        # only the attributes of the call site are considered
        @test MemoryEffects(call.function_attributes) == MemoryEffects(:readwrite)
        push!(call.function_attributes, EnumAttribute(MemoryEffects(:read)))
        push!(call.function_attributes, EnumAttribute(MemoryEffects(:none)))
        @test MemoryEffects(call.function_attributes) == MemoryEffects(:none)
        @test length(call.function_attributes) == 1
        @test occursin("memory(none)", string(mod))     # nothing else has these effects
        @test_throws ArgumentError MemoryEffects(call.argument_attributes[1])

        @test verify(mod) === nothing
    end
else
    @test_throws ArgumentError MemoryEffects(:read)
end

# parameter iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    let params = fn.parameters
        @test eltype(params) == LLVM.Argument

        @test length(params) == 1

        intparam = params[1]
        @test first(params) == intparam
        @test last(params) == intparam

        for param in params
            @test param == intparam
        end

        @test collect(params) == [intparam]
    end
end

# basic block iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)
    @test isempty(fn.blocks)

    entrybb = BasicBlock(fn, "SomeBasicBlock")
    @test fn.entry == entrybb
    let bbs = fn.blocks
        @test eltype(bbs) == BasicBlock

        @test !isempty(bbs)
        @test length(bbs) == 1

        @test first(bbs) == entrybb
        @test last(bbs) == entrybb

        for bb in bbs
            @test bb == entrybb
        end

        @test collect(bbs) == [entrybb]
    end

    empty!(fn)
    @test isempty(fn.blocks)
end

end


@testset "basic blocks" begin

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    @test_throws BoundsError first(fn.blocks)
    @test_throws BoundsError last(fn.blocks)
    bb2 = BasicBlock(fn, "SomeOtherBasicBlock")
    @test bb2.parent == fn
    @test isempty(bb2.instructions)
    @test isempty(bb2.predecessors)
    @test_throws ArgumentError bb2.successors

    @test_throws BoundsError first(bb2.instructions)
    @test_throws BoundsError last(bb2.instructions)

    bb1 = BasicBlock(bb2, "SomeBasicBlock")
    @test bb2.parent == fn
    position!(builder, bb1)
    brinst = br!(builder, bb2)
    position!(builder, bb2)
    retinst = ret!(builder)
    @test !isempty(bb2.instructions)
    @test collect(bb2.predecessors) == [bb1]
    @test collect(bb1.successors) == [bb2]

    @test bb1.terminator == brinst
    @test bb2.terminator == retinst

    @test bb1.prev === nothing
    @test bb1.next == bb2
    @test bb2.prev == bb1
    @test bb2.next === nothing

    bb3 = BasicBlock("YetAnotherBasicBlock")
    @test bb3.parent == nothing
    @test bb3.terminator == nothing
    # XXX: can we insert this block into the function?

    # instruction iteration
    let insts = bb1.instructions
        @test eltype(insts) == Instruction

        @test first(insts) == brinst
        @test last(insts) == brinst

        for inst in insts
            @test inst == brinst
        end

        @test collect(insts) == [brinst]
    end

    erase!(brinst)    # we'll be deleting bb2, so remove uses of it

    # basic block iteration
    let bbs = fn.blocks
        @test collect(bbs) == [bb1, bb2]

        @test first(bbs) == bb1
        @test last(bbs) == bb2

        move_before(bb2, bb1)
        @test collect(bbs) == [bb2, bb1]

        move_after(bb2, bb1)
        @test collect(bbs) == [bb1, bb2]

        @test bb1 in bbs
        @test bb2 in bbs
        remove!(bb1)
        erase!(bb2)
        @test isempty(bbs)
    end
end

end


@testset "instructions" begin

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)
    @test isempty(fn.parameters)

    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int1Type(), LLVM.Int1Type()])
    fn = LLVM.Function(mod, "SomeOtherFunction", ft)
    @test !isempty(fn.parameters)

    bb1 = BasicBlock(fn, "entry")
    bb2 = BasicBlock(fn, "then")
    bb3 = BasicBlock(fn, "else")

    position!(builder, bb1)
    addinst = add!(builder, fn.parameters[1], fn.parameters[2])
    brinst = br!(builder, fn.parameters[1], bb2, bb3)
    @test brinst.opcode == LLVM.API.LLVMBr

    @test addinst.prev === nothing
    @test addinst.next == brinst
    @test brinst.prev == addinst
    @test brinst.next === nothing

    # walking the IR doesn't dispatch dynamically, only allocating a box for every value
    # whose concrete type is determined at run time
    let walk(bb) = (n = 0; for inst in bb.instructions, op in inst.operands
                               n += op isa LLVM.Argument
                           end; n)
        @test walk(bb1) == 3
        nvals = sum(inst -> 1 + length(inst.operands), bb1.instructions)
        @test @allocated(walk(bb1)) <= 4 * sizeof(Int) * nvals
    end

    position!(builder, bb2)
    retinst = ret!(builder)

    position!(builder, bb3)
    retinst = ret!(builder)

    # terminators

    @test isterminator(brinst)
    @test isconditional(brinst)
    @test brinst.condition == fn.parameters[1]
    brinst.condition = fn.parameters[2]
    @test brinst.condition == fn.parameters[2]

    let succ = bb1.terminator.successors
        @test eltype(succ) == BasicBlock

        @test length(succ) == 2

        @test succ[1] == bb2
        @test succ[2] == bb3
        @test_throws BoundsError succ[3]

        @test collect(succ) == [bb2, bb3]
        @test first(succ) == bb2
        @test last(succ) == bb3

        i = 1
        for bb in succ
            @test 1 <= i <= 2
            if i == 1
                @test bb == bb2
            elseif i == 2
                @test bb == bb3
            end
            i += 1
        end

        succ[2] = bb3
        @test succ[2] == bb3
    end

    # general stuff

    @test brinst.parent == bb1

    # metadata
    mdval = MDNode([MDString("whatever")])
    let md = brinst.metadata
        @test keytype(md) == LLVM.MDKind
        @test valtype(md) == Metadata

        @test isempty(md)
        @test !haskey(md, "dbg")

        md["dbg"] = mdval
        @test md["dbg"] == mdval

        @test !isempty(md)
        @test haskey(md, "dbg")

        @test !haskey(md, "tbaa")
        @test_throws KeyError md["tbaa"]

        delete!(md, "dbg")

        @test isempty(md)
        @test !haskey(md, "dbg")
    end

    @test retinst in bb3.instructions
    remove!(retinst)
    @test !(retinst in bb3.instructions)
    @test retinst.opcode == LLVM.API.LLVMRet   # make sure retinst is still alive

    @test brinst in bb1.instructions
    erase!(brinst)
    @test !(brinst in bb1.instructions)
end

# new freeze instruction (used in 1.7 with JuliaLang/julia#38977)
@dispose ctx=Context() begin
    mod = parse(LLVM.Module,  """
        define i64 @julia_f_246(i64 %0) {
        top:
            %1 = freeze i64 undef
            ret i64 %1
        }""")
    f = first(mod.functions)
    bb = first(f.blocks)
    inst = first(bb.instructions)
    @test inst isa LLVM.FreezeInst
    dispose(mod)
end

end


@testset "collection views" begin

# the operands of a metadata node are a mutable view
@dispose ctx=Context() begin
    a, b = MDString("a"), MDString("b")
    node = MDNode([a, nothing])
    ops = node.operands
    @test ops == [a, nothing]
    @test ops[2] === nothing
    @test_throws BoundsError ops[3]

    ops[2] = b
    @test node.operands == [a, b]
    ops[1] = nothing
    @test ops[1] === nothing
    @test collect(ops) == [nothing, b]
    @test_throws BoundsError ops[3] = a

    # LLVM keeps uniqued nodes unique, so a node that becomes identical to another one is
    # made distinct
    existing = MDNode([a])
    other = MDNode([b])
    other.operands[1] = a
    @test other.operands == [a]
    @test other != existing
    @test occursin("distinct", string(other))
end

# the parameters of a function type are a read-only view
@dispose ctx=Context() begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type(), LLVM.Int64Type()])
    params = ft.parameters
    @test params == [LLVM.Int32Type(), LLVM.Int64Type()]
    @test params[2] == LLVM.Int64Type()
    @test length(params) == 2
    @test_throws BoundsError params[3]
    @test_throws CanonicalIndexError params[1] = LLVM.Int8Type()
    @test collect(params) isa Vector{LLVMType}
    @test isempty(LLVM.FunctionType(LLVM.VoidType()).parameters)

    # views can be used to construct new objects
    @test LLVM.FunctionType(LLVM.VoidType(), params) == ft
    @test LLVM.StructType(params).elements == params
    node = MDNode([MDString("a"), nothing])
    @test MDNode(node.operands) == node
end

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type(), LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)
    x, y = fn.parameters

    # the blocks of a function reflect blocks that are added or removed later
    bbs = fn.blocks
    @test isempty(bbs)
    entry = BasicBlock(fn, "entry")
    exit = BasicBlock(fn, "exit")
    @test bbs[2] == exit
    middle = BasicBlock(exit, "middle")
    @test bbs == [entry, middle, exit]
    @test bbs[2] == middle
    @test bbs[3] == exit
    @test_throws BoundsError bbs[4]
    @test_throws CanonicalIndexError bbs[1] = exit
    erase!(middle)
    @test bbs[2] == exit
    @test length(bbs) == 2
    @test_throws BoundsError bbs[3]

    # the predecessors of a block are a read-only view, derived from its uses
    preds = exit.predecessors
    @test isempty(preds)
    position!(builder, entry)
    call = call!(builder, ft, fn, [x, y])
    br = br!(builder, exit)
    @test collect(preds) == [entry]
    @test length(preds) == 1
    @test_throws MethodError push!(preds, entry)
    position!(builder, exit)
    ret!(builder)

    # the arguments of a call are a mutable view of its operands
    args = call.arguments
    @test args == [x, y]
    args[1] = y
    @test call.operands[1] == y
    @test call.arguments == [y, y]
    @test call!(builder, ft, fn, args).arguments == [y, y]
    @test_throws BoundsError args[3]
    @test_throws BoundsError args[3] = x

    # the operands of instructions can be replaced, but not those of constants
    call.operands[2] = x
    @test replace!(call.operands, x => y) == call.operands
    @test call.arguments == [y, y]
    ce = const_inttoptr(ConstantInt(Int64(42)), LLVM.PointerType(LLVM.Int32Type()))
    @test_throws ArgumentError ce.operands[1] = ConstantInt(Int64(0))

    # the successors of a terminator are a mutable view
    succs = br.successors
    @test succs == [exit]
    @test_throws BoundsError succs[2] = entry

    erase!(br)
    @test isempty(preds)

    # attribute sets can be iterated and appended to
    attrs = fn.function_attributes
    append!(attrs, [EnumAttribute("nounwind"), StringAttribute("foo", "bar")])
    @test length(attrs) == 2
    @test Set(attr.kind for attr in attrs) ==
          Set([EnumAttribute("nounwind").kind, "foo"])
    call_attrs = call.function_attributes
    append!(call_attrs, [EnumAttribute("nounwind")])
    @test [attr.kind for attr in call_attrs] == [EnumAttribute("nounwind").kind]
end

# the elements of a structure type are a read-only view
@dispose ctx=Context() begin
    st = LLVM.StructType("SomeStruct")
    elems = st.elements
    @test isempty(elems)
    elements!(st, [LLVM.Int32Type(), LLVM.Int8Type()]; packed=true)
    @test elems == [LLVM.Int32Type(), LLVM.Int8Type()]
    @test elems[2] == LLVM.Int8Type()
    @test ispacked(st)
    @test_throws BoundsError elems[3]
    @test_throws CanonicalIndexError elems[1] = LLVM.Int64Type()
    @test ctx.types["SomeStruct"] == st
end

# objects in a list can navigate to their siblings
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    a = GlobalVariable(mod, LLVM.Int32Type(), "a")
    b = GlobalVariable(mod, LLVM.Int32Type(), "b")
    @test a.prev === nothing
    @test a.next == b
    @test b.prev == a
    @test b.next === nothing
    @test_throws "read-only" a.next = b

    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type(), LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)
    x, y = fn.parameters
    @test x.prev === nothing
    @test x.next == y
    @test y.prev == x
    @test y.next === nothing

    foo, bar = mod.metadata["foo"], mod.metadata["bar"]
    @test foo.prev === nothing
    @test foo.next == bar
    @test bar.prev == foo
    @test bar.next === nothing

    # objects that are not part of a list have no siblings
    position!(builder, BasicBlock(fn, "entry"))
    inst = ret!(builder)
    remove!(inst)
    @test inst.next === nothing
    @test inst.prev === nothing
    bb = BasicBlock("detached")
    @test bb.next === nothing
    @test bb.prev === nothing

    # only objects in a list have siblings
    @test !hasproperty(ConstantInt(Int32(1)), :next)
    @test !hasproperty(ft, :next)
end

# instruction metadata and module flags can be iterated
@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)
    position!(builder, BasicBlock(fn, "entry"))
    inst = ret!(builder)

    md = inst.metadata
    @test isempty(md)
    @test length(md) == 0
    node = MDNode([MDString("foo")])
    md["foo"] = node
    @test collect(md) == [MDKind("foo") => node]
    @test length(md) == 1

    flags = mod.flags
    @test isempty(flags)
    val = Metadata(ConstantInt(Int32(42)))
    flags["foo", LLVM.API.LLVMModuleFlagBehaviorError] = val
    @test collect(flags) == ["foo" => val]
    @test length(flags) == 1
end

end

end

@testset "renamed functions" begin
    for name in (:is_opaque, :is_atomic, :available,
                 :set_transform!, :linkinglayercreator!, :targetmachinebuilder!,
                 :debuglocation, :debuglocation!, :threadlocalmode, :threadlocalmode!)
        @test !isdefined(LLVM, name)
    end
end
