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

    @test width(LLVM.IntType(8)) == 8

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
    @test return_type(ft) == x
    @test parameters(ft) == y
    @test_throws BoundsError parameters(ft)[3]
end

# sequential
@dispose ctx=Context() begin
    eltyp = LLVM.Int32Type()

    ptrtyp = LLVM.PointerType(eltyp)
    if supports_typed_pointers(ctx)
        @test eltype(ptrtyp) == eltyp
    end

    @test context(ptrtyp) == context(eltyp)

    @test addrspace(ptrtyp) == 0

    ptrtyp = LLVM.PointerType(eltyp, 1)
    @test addrspace(ptrtyp) == 1
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
        @test name(st) === nothing

        let elem_it = elements(st)
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
        @test name(st) == "foo"
        @test isopaque(st)
        elements!(st, elem)
        @test collect(elements(st)) == elem
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

    let ts = types(ctx)
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
    @test name(entry) == "entry"

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

    show(devnull, val)

    @test value_type(val) == LLVM.PointerType(typ)
    @test_throws ErrorException sizeof(val)
    @test name(val) == "foo"
    @test !isconstant(val)
    @test !isundef(val)

    name!(val, "bar")
    @test name(val) == "bar"
end

# usage

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, entry)

    valueinst1 = add!(builder, parameters(fn)[1],
                      ConstantInt(Int32(1)))
    @test !isterminator(valueinst1)

    userinst = add!(builder, valueinst1,
                    ConstantInt(Int32(1)))

    # use iteration
    let usepairs = uses(valueinst1)
        @test eltype(usepairs) == Use

        usepair = first(usepairs)
        @test value(usepair) == valueinst1
        @test user(usepair) == userinst

        for _usepair in usepairs
            @test usepair == _usepair
        end

        @test value.(collect(usepairs)) == [valueinst1]
        @test user.(collect(usepairs)) == [userinst]
    end

    valueinst2 = add!(builder, parameters(fn)[1],
                    ConstantInt(Int32(2)))

    replace_uses!(valueinst1, valueinst2)
    @test user.(collect(uses(valueinst2))) == [userinst]
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
    fun = functions(mod)["fun1"]

    for (i, instr) in enumerate(instructions(first(blocks(fun))))
        ops = operands(instr)
        @test eltype(ops) == Value
        if i == 1
            @test length(ops) == 2
            @test ops[1] == first(parameters(fun))
            @test ops[2] == ConstantInt(LLVM.Int32Type(), 1)
            @test_throws BoundsError ops[3]
        elseif i == 2
            @test length(ops) == 0
            @test collect(ops) == []
        end
    end

    fun = functions(mod)["fun2"]

    @test_throws BoundsError first(blocks(fun))
    @test_throws BoundsError last(blocks(fun))
    @test_throws BoundsError blocks(fun)[1]

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
        @test value_type(constval) == LLVM.Int1Type()
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
        # TODO: how to construct full-width constants?
        c = ConstantFP(typ, 1.1)
        @test convert(Float64, c) == 1.1
    end
    for T in [LLVM.FP128Type, LLVM.PPCFP128Type]
        typ = T()
        # TODO: how to construct full-width constants?
        c = ConstantFP(typ, 1.1)
        @test convert(Float64, c) == 1.1
    end
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

    end

    @testset "struct aggregate constants" begin

    # from Julia values
    let
        test_struct = TestStruct(true, -99, 1.5)
        constant_struct = ConstantStruct(test_struct, anonymous=true)
        constant_struct_type = value_type(constant_struct)

        @test constant_struct_type isa LLVM.StructType
        @test context(constant_struct) == ctx
        @test !ispacked(constant_struct_type)
        @test !isopaque(constant_struct_type)

        @test collect(elements(constant_struct_type)) ==
            [LLVM.Int1Type(), LLVM.Int64Type(), LLVM.HalfType()]

        expected_operands = [
            ConstantInt(LLVM.Int1Type(), Int(true)),
            ConstantInt(LLVM.Int64Type(), -99),
            ConstantFP(LLVM.HalfType(), 1.5)
        ]
        @test collect(operands(constant_struct)) == expected_operands
    end
    let
        test_struct = TestStruct(false, 52, -2.5)
        constant_struct = ConstantStruct(test_struct)
        constant_struct_type = value_type(constant_struct)

        @test constant_struct_type isa LLVM.StructType

        expected_operands = [
            ConstantInt(LLVM.Int1Type(), Int(false)),
            ConstantInt(LLVM.Int64Type(), 52),
            ConstantFP(LLVM.HalfType(), -2.5)
        ]
        @test collect(operands(constant_struct)) == expected_operands

        # re-creating the same type shouldn't fail
        ConstantStruct(TestStruct(true, 42, 0))
        # unless it's a conflicting type
        @test_throws ArgumentError ConstantStruct(AnotherTestStruct(1), "TestStruct")

    end
    let
        test_struct = TestSingleton()
        constant_struct = ConstantStruct(test_struct)
        constant_struct_type = value_type(constant_struct)

        @test isempty(operands(constant_struct))
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
        @test value_type(cda) == LLVM.ArrayType(eltyp, 4)
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
        vec = T[1,2,3,4]
        cda = ConstantDataArray(vec)
        @test cda isa ConstantDataArray
        @test size(vec) == size(cda)
        @test collect(cda) == ConstantFP.(vec)
    end

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

        for f = [const_neg, const_nswneg, const_nuwneg]
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

        for f in [const_mul, const_nswmul, const_nuwmul]
            ce = f(val, other_val)::LLVM.Constant
            @check_ir ce "i32 84"
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

        ce = const_inttoptr(ce, value_type(ptr))::LLVM.Constant
        if supports_typed_pointers(ctx)
            @check_ir ce "i32* null"
        else
            @check_ir ce "ptr null"
        end
        @test isempty(uses(ptr))
        for f in [const_addrspacecast, const_pointercast]
            ce = f(ptr, LLVM.PointerType(LLVM.Int32Type(), 1))::LLVM.Constant
            if supports_typed_pointers(ctx)
                @check_ir ce "i32 addrspace(1)* addrspacecast (i32* null to i32 addrspace(1)*)"
            else
                @check_ir ce "ptr addrspace(1) addrspacecast (ptr null to ptr addrspace(1))"
            end
            # deletion of a constant
            @test !isempty(uses(ptr))
            LLVM.unsafe_destroy!(ce)
            @test isempty(uses(ptr))
        end
    end

    # gep, inbounds_gep, select, extractelement, insertelement, shufflevector, exactvalue, insertvalue

    end
end

# global values
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    st = LLVM.StructType("SomeType")
    ft = LLVM.FunctionType(st, [st])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    @test isdeclaration(fn)
    @test linkage(fn) == LLVM.API.LLVMExternalLinkage
    linkage!(fn, LLVM.API.LLVMAvailableExternallyLinkage)
    @test linkage(fn) == LLVM.API.LLVMAvailableExternallyLinkage

    @test section(fn) == ""
    section!(fn, "SomeSection")
    @test section(fn) == "SomeSection"

    @test visibility(fn) == LLVM.API.LLVMDefaultVisibility
    visibility!(fn, LLVM.API.LLVMHiddenVisibility)
    @test visibility(fn) == LLVM.API.LLVMHiddenVisibility

    @test dllstorage(fn) == LLVM.API.LLVMDefaultStorageClass
    dllstorage!(fn, LLVM.API.LLVMDLLImportStorageClass)
    @test dllstorage(fn) == LLVM.API.LLVMDLLImportStorageClass

    @test !unnamed_addr(fn)
    @test !local_unnamed_addr(fn)
    unnamed_addr!(fn, true)
    @test unnamed_addr(fn)
    @test !local_unnamed_addr(fn)
    local_unnamed_addr!(fn, true)
    @test !unnamed_addr(fn)
    @test local_unnamed_addr(fn)

    str = MDString("bar")
    md = MDNode([str])
    @test isempty(metadata(fn))
    @test !haskey(metadata(fn), "foo")
    @test_throws KeyError metadata(fn)["foo"]
    metadata(fn)["foo"] = md
    @test !isempty(metadata(fn))
    @test haskey(metadata(fn), "foo")
    @test metadata(fn)["foo"] == md
    @test collect(values(metadata(fn))) == [md]
    delete!(metadata(fn), "foo")
    @test isempty(metadata(fn))
    metadata(fn)["foo"] = md
    @test !isempty(metadata(fn))
    empty!(metadata(fn))
    @test isempty(metadata(fn))
end

# global variables
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    @test isempty(globals(mod))
    gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")
    @test !isempty(globals(mod))

    show(devnull, gv)

    @test initializer(gv) === nothing
    init = ConstantInt(Int32(0))
    initializer!(gv, init)
    @test initializer(gv) == init
    initializer!(gv, nothing)
    @test initializer(gv) === nothing

    @test !isthreadlocal(gv)
    threadlocal!(gv, true)
    @test isthreadlocal(gv)

    @test !isconstant(gv)
    constant!(gv, true)
    @test isconstant(gv)

    @test !isextinit(gv)
    extinit!(gv, true)
    @test isextinit(gv)

    @test alignment(gv) == 0
    alignment!(gv, 4)
    @test alignment(gv) == 4

    @test threadlocalmode(gv) == LLVM.API.LLVMGeneralDynamicTLSModel
    threadlocalmode!(gv, LLVM.API.LLVMNotThreadLocal)
    @test threadlocalmode(gv) == LLVM.API.LLVMNotThreadLocal

    @test !haskey(globals(mod), "llvm.used")
    set_used!(mod, gv)
    @test haskey(globals(mod), "llvm.used")
    erase!(globals(mod)["llvm.used"])

    @test !haskey(globals(mod), "llvm.compiler.used")
    set_compiler_used!(mod, gv)
    @test haskey(globals(mod), "llvm.compiler.used")
    erase!(globals(mod)["llvm.compiler.used"])

    let gvars = globals(mod)
        @test gv in gvars
        erase!(gv)
        @test isempty(gvars)
    end
end

@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    st = LLVM.StructType("SomeType")
    gv = GlobalVariable(mod, st, "SomeGlobal")

    init = null(st)
    initializer!(gv, init)
    @test initializer(gv) == init
end

@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal", 1)

    @test value_type(gv) isa LLVM.PointerType
    @test addrspace(value_type(gv)) == 1

    @test global_value_type(gv) == LLVM.Int32Type()
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

    push!(metadata(mod)["function"], MDNode([f1]))
    @test Value(operands(operands(metadata(mod)["function"])[1])[1]) == f1

    f2 = LLVM.Function(mod, "f2", ft)
    replace_metadata_uses!(f1, f2)
    @test Value(operands(operands(metadata(mod)["function"])[1])[1]) == f2
end

# different type; requires a hack
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft1 = LLVM.FunctionType(LLVM.VoidType())
    f1 = LLVM.Function(mod, "f1", ft1)

    push!(metadata(mod)["function"], MDNode([f1]))
    @test Value(operands(operands(metadata(mod)["function"])[1])[1]) == f1

    ft2 = LLVM.FunctionType(LLVM.Int32Type())
    f2 = LLVM.Function(mod, "f2", ft2)
    replace_metadata_uses!(f1, f2)
    @test Value(operands(operands(metadata(mod)["function"])[1])[1]) == f2
end

@dispose ctx=Context() begin
    str = MDString("foo")
    node = MDNode([str])
    ops = operands(node)
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

    foo_md = operands(metadata(mod)["foo"])[1]
    @test operands(foo_md)[1] !== nothing
    @test operands(foo_md)[2] === nothing
    @test operands(foo_md)[3] !== nothing

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

    fun = functions(mod)["test"]
    bb = first(collect(blocks(fun)))
    inst = first(collect(instructions(bb)))

    @test haskey(metadata(inst), "dbg")
    loc = metadata(inst)["dbg"]

    @test loc isa DILocation
    @test LLVM.line(loc) == 94
    @test LLVM.column(loc) == 0

    scope = LLVM.scope(loc)
    @test scope isa DISubProgram
    @test LLVM.line(scope) == 0
    @test LLVM.name(scope) == "Float64;"

    file = LLVM.file(scope)
    @test file isa DIFile
    @test LLVM.filename(file) == "float.jl"
    @test LLVM.directory(file) == "."
    @test LLVM.source(file) == ""

    loc = LLVM.inlined_at(loc)
    @test loc isa DILocation
    @test LLVM.line(loc) == 7

    loc = LLVM.inlined_at(loc)
    @test loc isa DILocation

    loc = LLVM.inlined_at(loc)
    @test loc isa DILocation

    loc = LLVM.inlined_at(loc)
    @test loc isa DILocation

    loc = LLVM.inlined_at(loc)
    @test loc === nothing

    dispose(mod)
end

end

end


@testset "module" begin

@dispose ctx=Context() begin
    @dispose mod=LLVM.Module("SomeModule") begin
        @test context(mod) == ctx

        @test name(mod) == "SomeModule"
        name!(mod, "SomeOtherName")
        @test name(mod) == "SomeOtherName"
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

    inline_asm!(mod, "nop")
    @test split(inline_asm(mod)) == ["nop"]
    inline_asm!(mod, "nop")
    @test split(inline_asm(mod)) == ["nop", "nop"]
    inline_asm!(mod, "nop"; overwrite=true)
    @test split(inline_asm(mod)) == ["nop"]

    dummyTriple = "SomeTriple"
    triple!(mod, dummyTriple)
    @test triple(mod) == dummyTriple

    dummyLayout = "e-p:64:64:64"
    datalayout!(mod, dummyLayout)
    @test string(datalayout(mod)) == dummyLayout

    md = Metadata(ConstantInt(42))

    mod_flags = flags(mod)
    mod_flags["foobar", LLVM.API.LLVMModuleFlagBehaviorError] = md

    @test occursin("!llvm.module.flags = !{!0}", string(mod))
    @test occursin(r"!0 = !\{i\d+ 1, !\"foobar\", i\d+ 42\}", string(mod))

    @test mod_flags["foobar"] == md
    @test_throws KeyError mod_flags["foobaz"]

    @test sdk_version(mod) === nothing
    sdk_version!(mod, v"1.2.3")
    @test sdk_version(mod) == v"1.2.3"
end

# metadata iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    node = MDNode([MDString("SomeMDString")])

    let mds = metadata(mod)
        @test keytype(mds) == String
        @test valtype(mds) == NamedMDNode

        @test !haskey(mds, "SomeMDNode")
        @test !(node in operands(mds["SomeMDNode"]))
        @test haskey(mds, "SomeMDNode") # getindex is mutating

        push!(mds["SomeMDNode"], node)
        @test node in operands(mds["SomeMDNode"])
    end
end

# global variable iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    dummygv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")

    let gvs = globals(mod)
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

# function iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    st = LLVM.StructType("SomeType")
    ft = LLVM.FunctionType(st, [st])
    @test isempty(functions(mod))

    @test_throws BoundsError first(functions(mod))
    @test_throws BoundsError last(functions(mod))

    dummyfn = LLVM.Function(mod, "SomeFunction", ft)
    let fns = functions(mod)
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
    @test prevfun(dummyfn) === nothing
    @test nextfun(dummyfn) == anotherfn
    @test prevfun(anotherfn) == dummyfn
    @test nextfun(anotherfn) === nothing
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
        @test haskey(functions(mod), "SomeFunction")
        dispose(mod)
    end
end

# binary bitcode
@dispose ctx=Context() builder=IRBuilder() source_mod=LLVM.Module("SomeModule") begin
    invalid_bitcode = "invalid"
    @test_throws LLVMException parse(LLVM.Module, unsafe_wrap(Vector{UInt8}, invalid_bitcode))

    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(source_mod, "SomeFunction", ft)

    entry = BasicBlock(fn, "entry")
    position!(builder, entry)

    ret!(builder)

    verify(source_mod)


    @dispose bitcode_buf = convert(MemoryBuffer, source_mod) begin
        @dispose mod=parse(LLVM.Module, bitcode_buf) begin
            verify(mod)
            @test haskey(functions(mod), "SomeFunction")
        end
    end


    let bitcode = convert(Vector{UInt8}, source_mod)
        @dispose mod = parse(LLVM.Module, bitcode) begin
            verify(mod)
            @test haskey(functions(mod), "SomeFunction")
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

@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    show(devnull, fn)

    @test personality(fn) === nothing
    pers_ft = LLVM.FunctionType(LLVM.Int32Type(); vararg=true)
    pers_fn = LLVM.Function(mod, "PersonalityFunction", ft)
    personality!(fn, pers_fn)
    @test personality(fn) == pers_fn
    personality!(fn, nothing)
    @test personality(fn) === nothing
    erase!(pers_fn)

    @test !isintrinsic(fn)

    @test callconv(fn) == LLVM.API.LLVMCCallConv
    callconv!(fn, LLVM.API.LLVMFastCallConv)
    @test callconv(fn) == LLVM.API.LLVMFastCallConv

    @test LLVM.gc(fn) == ""
    gc!(fn, "SomeGC")
    @test LLVM.gc(fn) == "SomeGC"

    let fns = functions(mod)
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

    @test name(intr) == "llvm.trap"

    ft = LLVM.FunctionType(intr)
    @test ft isa LLVM.FunctionType
    @test return_type(ft) == LLVM.VoidType()

    fn = LLVM.Function(mod, intr)
    @test fn isa LLVM.Function

    if supports_typed_pointers(ctx)
        @test eltype(value_type(fn)) == ft
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

    @test name(intr, [LLVM.DoubleType()]) == "llvm.sin.f64"

    ft = LLVM.FunctionType(intr, [LLVM.DoubleType()])
    @test ft isa LLVM.FunctionType
    @test return_type(ft) == LLVM.DoubleType()

    fn = LLVM.Function(mod, intr, [LLVM.DoubleType()])
    @test fn isa LLVM.Function
    if supports_typed_pointers(ctx)
        @test eltype(value_type(fn)) == ft
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
    instr = call!(builder, ft, fn, LLVM.Value[ parameters(fn)... ])

    let attrs = function_attributes(fn), instr_attrs = function_attributes(instr)
        @test eltype(attrs) == Attribute
        @test eltype(instr_attrs) == Attribute

        @test length(attrs) == 0
        @test length(instr_attrs) == 0

        let attr = EnumAttribute("sspreq", 0)
            @test kind(attr) != 0
            @test value(attr) == 0
            push!(attrs, attr)
            @test collect(attrs) == [attr]

            delete!(attrs, attr)
            @test length(attrs) == 0
        end
        let instr_attr = EnumAttribute("sspreq", 0)
            @test kind(instr_attr) != 0
            @test value(instr_attr) == 0
            push!(instr_attrs, instr_attr)
            @test collect(instr_attrs) == [instr_attr]

            delete!(instr_attrs, instr_attr)
            @test length(instr_attrs) == 0
        end

        let attr = StringAttribute("nounwind", "")
            @test kind(attr) == "nounwind"
            @test value(attr) == ""
            push!(attrs, attr)
            @test collect(attrs) == [attr]

            delete!(attrs, attr)
            @test length(attrs) == 0
        end
        let instr_attr = StringAttribute("nounwind", "")
            @test kind(instr_attr) == "nounwind"
            @test value(instr_attr) == ""
            push!(instr_attrs, instr_attr)
            @test collect(instr_attrs) == [instr_attr]

            delete!(instr_attrs, instr_attr)
            @test length(instr_attrs) == 0
        end

        let attr = TypeAttribute("sret", LLVM.Int32Type())
            @test kind(attr) != 0
            @test value(attr) ==  LLVM.Int32Type()

            push!(attrs, attr)
            @test collect(attrs) == [attr]

            delete!(attrs, attr)
            @test length(attrs) == 0
        end
        let instr_attr = TypeAttribute("sret", LLVM.Int32Type())
            @test kind(instr_attr) != 0
            @test value(instr_attr) ==  LLVM.Int32Type()

            push!(instr_attrs, instr_attr)
            @test collect(instr_attrs) == [instr_attr]

            delete!(instr_attrs, instr_attr)
            @test length(instr_attrs) == 0
        end
    end

    for i in 1:length(parameters(fn))
        let attrs = parameter_attributes(fn, i)
            @test eltype(attrs) == Attribute
            @test length(attrs) == 0
        end
    end
    for i in 1:length(arguments(instr))
        let attrs = argument_attributes(instr, i)
            @test eltype(attrs) == Attribute
            @test length(attrs) == 0
        end
    end

    let attrs = return_attributes(fn)
        @test eltype(attrs) == Attribute
        @test length(attrs) == 0
    end
    let attrs = return_attributes(instr)
        @test eltype(attrs) == Attribute
        @test length(attrs) == 0
    end
end

# parameter iteration
@dispose ctx=Context() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    let params = parameters(fn)
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
    @test isempty(blocks(fn))

    entrybb = BasicBlock(fn, "SomeBasicBlock")
    @test entry(fn) == entrybb
    let bbs = blocks(fn)
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
    @test isempty(blocks(fn))
end

end


@testset "basic blocks" begin

@dispose ctx=Context() builder=IRBuilder() mod=LLVM.Module("SomeModule") begin
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    @test_throws BoundsError first(blocks(fn))
    @test_throws BoundsError last(blocks(fn))
    bb2 = BasicBlock(fn, "SomeOtherBasicBlock")
    @test LLVM.parent(bb2) == fn
    @test isempty(instructions(bb2))
    @test isempty(predecessors(bb2))
    @test_throws ArgumentError successors(bb2)

    @test_throws BoundsError first(instructions(bb2))
    @test_throws BoundsError last(instructions(bb2))

    bb1 = BasicBlock(bb2, "SomeBasicBlock")
    @test LLVM.parent(bb2) == fn
    position!(builder, bb1)
    brinst = br!(builder, bb2)
    position!(builder, bb2)
    retinst = ret!(builder)
    @test !isempty(instructions(bb2))
    @test collect(predecessors(bb2)) == [bb1]
    @test collect(successors(bb1)) == [bb2]

    @test terminator(bb1) == brinst
    @test terminator(bb2) == retinst

    @test prevblock(bb1) === nothing
    @test nextblock(bb1) == bb2
    @test prevblock(bb2) == bb1
    @test nextblock(bb2) === nothing

    bb3 = BasicBlock("YetAnotherBasicBlock")
    @test LLVM.parent(bb3) == nothing
    @test terminator(bb3) == nothing
    # XXX: can we insert this block into the function?

    # instruction iteration
    let insts = instructions(bb1)
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
    let bbs = blocks(fn)
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
    @test isempty(parameters(fn))

    ft = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int1Type(), LLVM.Int1Type()])
    fn = LLVM.Function(mod, "SomeOtherFunction", ft)
    @test !isempty(parameters(fn))

    bb1 = BasicBlock(fn, "entry")
    bb2 = BasicBlock(fn, "then")
    bb3 = BasicBlock(fn, "else")

    position!(builder, bb1)
    addinst = add!(builder, parameters(fn)[1], parameters(fn)[2])
    brinst = br!(builder, parameters(fn)[1], bb2, bb3)
    @test opcode(brinst) == LLVM.API.LLVMBr

    @test previnst(addinst) === nothing
    @test nextinst(addinst) == brinst
    @test previnst(brinst) == addinst
    @test nextinst(brinst) === nothing

    position!(builder, bb2)
    retinst = ret!(builder)

    position!(builder, bb3)
    retinst = ret!(builder)

    # terminators

    @test isterminator(brinst)
    @test isconditional(brinst)
    @test condition(brinst) == parameters(fn)[1]
    condition!(brinst, parameters(fn)[2])
    @test condition(brinst) == parameters(fn)[2]

    let succ = successors(terminator(bb1))
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

    @test LLVM.parent(brinst) == bb1

    # metadata
    mdval = MDNode([MDString("whatever")])
    let md = metadata(brinst)
        @test keytype(md) == LLVM.MDKind
        @test valtype(md) == LLVM.MetadataAsValue

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

    @test retinst in instructions(bb3)
    remove!(retinst)
    @test !(retinst in instructions(bb3))
    @test opcode(retinst) == LLVM.API.LLVMRet   # make sure retinst is still alive

    @test brinst in instructions(bb1)
    erase!(brinst)
    @test !(brinst in instructions(bb1))
end

# new freeze instruction (used in 1.7 with JuliaLang/julia#38977)
@dispose ctx=Context() begin
    mod = parse(LLVM.Module,  """
        define i64 @julia_f_246(i64 %0) {
        top:
            %1 = freeze i64 undef
            ret i64 %1
        }""")
    f = first(functions(mod))
    bb = first(blocks(f))
    inst = first(instructions(bb))
    @test inst isa LLVM.FreezeInst
    dispose(mod)
end

end

end
