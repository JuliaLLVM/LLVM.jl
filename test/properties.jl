@testset "properties" begin

@dispose ctx=Context() mod=LLVM.Module("SomeModule") builder=IRBuilder() begin
    ft = LLVM.FunctionType(LLVM.Int32Type(), [LLVM.PointerType(LLVM.Int32Type())])
    fn = LLVM.Function(mod, "SomeFunction", ft)

    @test fn.name == "SomeFunction"
    fn.name = "OtherFunction"
    @test fn.name == "OtherFunction"
    fn.name = "SomeFunction"

    # assignments evaluate to the assigned value
    @test (fn.callconv = LLVM.API.LLVMFastCallConv) == LLVM.API.LLVMFastCallConv
    @test fn.callconv == LLVM.API.LLVMFastCallConv

    # properties are inherited, but fields are private
    names = propertynames(fn)
    @test allunique(names)
    @test :name in names          # Value
    @test :linkage in names       # GlobalValue
    @test :personality in names   # Function
    @test !(:ref in names)
    @test :ref in propertynames(fn, true)
    @test hasproperty(fn, :linkage)
    @test !hasproperty(ft, :linkage)
    @test fn.ref === getfield(fn, :ref)

    # accessing properties is type stable
    @test @inferred((f -> f.linkage)(fn)) == LLVM.API.LLVMExternalLinkage
    @test @inferred((f -> f.ref)(fn)) isa LLVM.API.LLVMValueRef

    # read-only and unknown properties
    @test_throws "property `value_type` of LLVM.Function is read-only" fn.value_type = ft
    @test_throws "has no property `foo`; available properties are: value_type, name" fn.foo
    @test_throws "has no property `foo`" fn.foo = 1

    # properties are only defined for the objects that support them
    entrybb = BasicBlock(fn, "entry")
    position!(builder, entrybb)
    ld = load!(builder, LLVM.Int32Type(), parameters(fn)[1])
    ld.alignment = 4
    @test ld.alignment == 4
    val = add!(builder, ld, ld)
    @test !hasproperty(val, :alignment)
    @test_throws "no property `alignment`" val.alignment
    cmpxchg = atomic_cmpxchg!(builder, parameters(fn)[1], val, val,
                              LLVM.API.LLVMAtomicOrderingSequentiallyConsistent,
                              LLVM.API.LLVMAtomicOrderingMonotonic, false)
    @test !hasproperty(cmpxchg, :ordering)
    @test cmpxchg.success_ordering == LLVM.API.LLVMAtomicOrderingSequentiallyConsistent

    # fast-math flags can only be added to, so they are a read-only property
    flt = uitofp!(builder, val, LLVM.FloatType())
    fp = fadd!(builder, flt, flt)
    fast_math!(fp; nnan=true)
    fast_math!(fp; ninf=true)
    @test fp.fast_math.nnan && fp.fast_math.ninf && !fp.fast_math.nsz
    @test_throws "read-only" fp.fast_math = (; nsz=true)

    # debug locations can be cleared by assigning `nothing`
    LLVM.DIBuilder(mod) do dib
        difile = LLVM.file!(dib, "test.jl", "/tmp")
        LLVM.compile_unit!(dib, LLVM.API.LLVMDWARFSourceLanguageJulia,
                           difile, "LLVM.jl Tests")
        sp = LLVM.subprogram!(dib, difile, "SomeFunction", difile, 1,
                              LLVM.subroutine_type!(dib, difile, nothing))
        loc = DILocation(42, 1, sp)
        @test loc.line == 42
        @test loc.scope == sp
        @test loc.scope.file.filename == "test.jl"

        builder.debug_location = loc
        @test builder.debug_location == loc
        builder.debug_location = nothing
        @test builder.debug_location === nothing

        ld.debug_location = loc
        @test ld.debug_location == loc
        ld.debug_location = nothing
        @test ld.debug_location === nothing

        # copying the builder's location to an instruction is an assignment to the latter
        builder.debug_location = loc
        ld.debug_location = builder.debug_location
        @test ld.debug_location == loc
        @test_throws "cannot set property `debug_location` of IRBuilder" builder.debug_location = ld
        builder.debug_location = nothing
    end

    # the section of an alias is that of its aliasee, and cannot be set
    gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal")
    gv.section = "SomeSection"
    ga = GlobalAlias(mod, gv, "SomeAlias")
    @test ga.section == "SomeSection"
    @test_throws "property `section` of GlobalAlias is read-only" ga.section = "OtherSection"

    # module-level inline assembly is replaced, not appended to
    mod.inline_asm = "nop"
    mod.inline_asm = "ret"
    @test split(mod.inline_asm) == ["ret"]

    # relationships
    @test ld.parent == entrybb
    @test ld.parent.parent == fn
    @test fn.parent == mod
    @test parameters(fn)[1].parent == fn
    @test fn.entry == entrybb
    @test entrybb.terminator === nothing
    retinst = ret!(builder, val)
    @test entrybb.terminator == retinst
    @test_throws "read-only" ld.parent = entrybb

    # ... and their absence
    decl = LLVM.Function(mod, "SomeDeclaration", ft)
    @test decl.entry === nothing
    detached_bb = BasicBlock("detached")
    @test detached_bb.parent === nothing
    detached = unreachable!(builder)
    remove!(detached)
    @test detached.parent === nothing
end

# properties are the only public spelling: the accessor functions that back them are
# internal, except for `context`, which also provides the task-local context
@static if VERSION >= v"1.11"
    # functions that are named like a property setter, but that do something else
    unrelated = (:context!, :fast_math!, :binop!, :expression!, :file!, :subprogram!)
    for name in unique(last.(LLVM.property_registry))
        name === :context && continue
        @test !Base.ispublic(LLVM, name)
        setter = Symbol(name, :!)
        setter in unrelated && continue
        @test !Base.ispublic(LLVM, setter)
    end
    @test Base.ispublic(LLVM, :context)
end

end

@testset "abstractly typed values" begin
    # properties of values whose concrete type is only known at run time shouldn't dispatch
    @dispose ctx=Context() mod=LLVM.Module("SomeModule") builder=IRBuilder() begin
        ft = LLVM.FunctionType(LLVM.Int32Type(), [LLVM.Int32Type()])
        fn = LLVM.Function(mod, "SomeFunction", ft)
        position!(builder, BasicBlock(fn, "entry"))
        inst = add!(builder, parameters(fn)[1], ConstantInt(Int32(1)), "sum")
        ret!(builder, inst)

        vals = Value[inst, parameters(fn)[1], ConstantInt(Int32(42)), fn]
        @test all(v -> v.ref === Base.unsafe_convert(LLVM.API.LLVMValueRef, v), vals)
        @test all(v -> v.name == LLVM.name(v), vals)

        # measure behind a function barrier, as `@allocated` on a global allocates itself
        sum_refs(vals) = sum(v -> UInt(v.ref), vals)
        measure(f, x) = (f(x); @allocated f(x))
        @test measure(sum_refs, vals) == 0
    end
end

@testset "vocabularies" begin
    # `using LLVM` only exports `@dispose`
    @test filter(n -> Base.isexported(LLVM, n), names(LLVM)) == [Symbol("@dispose"), :LLVM]

    # the vocabularies re-export LLVM's bindings
    # including the instruction types, and the groups of instructions that have properties
    @test LLVM.IR.CallInst === LLVM.CallInst
    for name in (:CallBase, :AtomicInst, :MemAccessInst, :AlignedInst, :NoWrapInst, :ExactInst, :NonNegInst)
        @test Base.isexported(LLVM.IR, name)
    end
    @test LLVM.IR.functions === LLVM.functions
    @test LLVM.Build.add! === LLVM.Passes.add! === LLVM.ORC.add! === LLVM.add!
    @test !Base.isexported(LLVM, :IR)
    @static if VERSION >= v"1.11"
        @test Base.ispublic(LLVM, :IR)
    end

    # accessors that back properties are not part of any vocabulary
    for vocab in (LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC), name in (:name, :parent, :entry)
        @test !Base.isexported(vocab, name)
    end
end
