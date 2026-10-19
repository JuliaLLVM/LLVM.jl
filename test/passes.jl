
@testset "newpm" begin

using LLVM.Interop
using IOCapture

function test_module()
    mod = LLVM.Module("test")
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    @dispose builder=IRBuilder() begin
        entry = BasicBlock(fn, "entry")
        position!(builder, LLVM.at_end(entry))

        ret!(builder)
    end

    return mod
end

@testset "pass builder" begin
    # invalid options are rejected (without leaking the options)
    @test_throws ArgumentError PassBuilder(; invalid_option=true)

    @test PassBuilder(pb -> pb isa PassBuilder; verify_each=true)

    # adding and registering passes returns the pass builder or manager
    @dispose pb=PassBuilder() begin
        @test add!(pb, NoOpModulePass()) === pb
        @test add!(pb, ModulePassManager()) do mpm
            @test add!(mpm, NoOpModulePass()) === mpm
        end === pb
        @test add!(_ -> nothing, pb, FunctionPassManager()) === pb
        @test add!(pb, AAManager()) do aam
            add!(aam, BasicAA())
        end === pb
        @test register!(pb, ModulePass("some-pass", mod -> false)) === pb
    end

    @dispose ctx=Context() begin
        # single pass
        @dispose mod=test_module() begin
            fun = only(mod.functions)

            # by string
            @test run!("no-op-module", mod) === nothing
            @test run!("no-op-function", fun) === nothing

            # by object
            @test run!(NoOpModulePass(), mod) === nothing
            @test run!(NoOpFunctionPass(), fun) === nothing

            # by object with options
            @test run!(LoopExtractorPass(single=true), mod) === nothing
            @test run!(EarlyCSEPass(memssa=true), fun) === nothing
        end

        # default pipelines
        @dispose mod=test_module() begin
            # by string
            @test run!("default<O3>", mod) === nothing

            # by object
            @test run!(DefaultPipeline(), mod) === nothing

            # by object with options
            if LLVM.version() >= v"23"
                @test_throws ArgumentError DefaultPipeline(opt_level='s')
                @test_throws ArgumentError DefaultPipeline(opt_level='z')
            else
                @test run!(DefaultPipeline(opt_level='s'), mod) === nothing
            end
        end

        # custom pipelines
        @dispose pb=PassBuilder() mod=test_module() begin
            # by string
            add!(pb, "no-op-module")

            # by object
            add!(pb, NoOpModulePass())

            # by object with options
            add!(pb, LoopExtractorPass(single=true))

            @test string(pb) == "no-op-module,no-op-module,loop-extract<single>"

            @test run!(pb, mod) === nothing
        end

        # options
        @dispose mod=test_module() begin
            @dispose pb=PassBuilder(verify_each=true) begin
                add!(pb, "no-op-module")
                @test run!(pb, mod) === nothing
            end

            run!("no-op-module", mod; verify_each=true)
        end

        # target machines
        host_triple = LLVM.default_triple()
        host_t = LLVM.Target(triple=host_triple)
        @dispose tm=LLVM.TargetMachine(host_t, host_triple) mod=test_module() begin
            @test run!(NoOpModulePass(), mod, tm) === nothing
            @test run!("no-op-module", mod, tm) === nothing
        end
    end
end

@testset "pass manager" begin
    @dispose ctx=Context() begin
        # pass manager interface
        @dispose pb=PassBuilder() mod=test_module() begin
            add!(pb, "no-op-module")
            add!(pb, ModulePassManager()) do mpm
                # by string
                add!(mpm, "no-op-module")

                # by object
                add!(mpm, NoOpModulePass())

                # by object with options
                add!(mpm, LoopExtractorPass(single=true))
                add!(mpm, SimplifyCFGPass(keep_loops=false))

                @test string(mpm) == "module(no-op-module,no-op-module,loop-extract<single>,simplifycfg<no-keep-loops>)"
            end
            add!(pb, NoOpModulePass())
            @test string(pb) == "no-op-module,module(no-op-module,no-op-module,loop-extract<single>,simplifycfg<no-keep-loops>),no-op-module"

            @test run!(pb, mod) === nothing
        end

        # nested pass managers
        @dispose pb=PassBuilder() mod=test_module() begin
            add!(pb, ModulePassManager()) do mpm
                add!(mpm, "no-op-module")
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, "no-op-function")
                    add!(fpm, LoopPassManager()) do lpm
                        add!(lpm, "no-op-loop")
                    end
                end
            end
            @test string(pb) == "module(no-op-module,function(no-op-function,loop(no-op-loop)))"

            @test run!(pb, mod) === nothing
        end

        # pass managers with options
        @dispose pb=PassBuilder(debug_logging=true) mod=test_module() begin
            add!(pb, FunctionPassManager(; eager_invalidate=true)) do fpm
                add!(fpm, "require<domtree>")
                add!(fpm, LoopPassManager(; use_memory_ssa=true)) do lpm
                    add!(lpm, "no-op-loop")
                end
            end
            @test string(pb) ==
                  "function<eager-inv>(require<domtree>,loop-mssa(no-op-loop))"
            @test sprint(print_pipeline, pb) == string(pb)

            # the dominator tree is invalidated after the function pass manager
            io = IOCapture.capture() do
                run!(pb, mod)
            end
            @test contains(io.output, "Invalidating analysis: DominatorTreeAnalysis")
        end
    end
end

@testset "pipeline printing" begin
    # passes are printed by their name, in the pass managers that LLVM inserts
    @dispose pb=PassBuilder() begin
        add!(pb, "globaldce,instsimplify")
        @test sprint(print_pipeline, pb) == "globaldce,function(instsimplify)"
        @test IOCapture.capture(() -> print_pipeline(pb)).output ==
              "globaldce,function(instsimplify)"
    end

    # analyses, parameterized passes and the adaptors that LLVM inserts, by their name
    for (pipeline, printed) in [
            "require<domtree>,invalidate<all>" => "function(require<domtree>,invalidate<all>)",
            "require<globals-aa>,invalidate<all>" => "require<globals-aa>,invalidate<all>",
            "cgscc(inline)" => "cgscc(inline)",
            "function(loop(licm),loop-mssa(licm))" =>
                "function(loop(licm<allowspeculation>),loop-mssa(licm<allowspeculation>))"]
        @dispose pb=PassBuilder() begin
            add!(pb, pipeline)
            @test sprint(print_pipeline, pb) == printed
        end
    end
    @dispose pb=PassBuilder() begin
        add!(pb, "function(loop-unroll<O2>,simplifycfg<bonus-inst-threshold=2>)")
        @test startswith(sprint(print_pipeline, pb),
                         "function(loop-unroll<O2>,simplifycfg<bonus-inst-threshold=2;")
    end

    # a default pipeline, which LLVM builds from passes that it does not parse
    @dispose pb=PassBuilder() begin
        add!(pb, "default<O2>")
        printed = sprint(print_pipeline, pb)
        @test occursin("function<eager-inv>(", printed)
        @test occursin(r"cgscc\(.*inline", printed)
        @test !occursin(r"[A-Z][A-Za-z]+(Pass|Analysis)\b", printed)
    end

    # custom passes are printed by their name and options, without running them
    calls = Ref(0)
    custom_passes = [FunctionPass("custom-pass", f -> (calls[] += 1; false)),
                     ModulePass("custom-pass-with-options", (mod, opts) -> (calls[] += 1; false);
                                parse_options=identity)]
    pipeline = "custom-pass-with-options<some;option=1>,function(custom-pass)"
    @dispose pb=PassBuilder() begin
        foreach(pass -> register!(pb, pass), custom_passes)
        add!(pb, pipeline)
        @test sprint(print_pipeline, pb) == pipeline
        @test calls[] == 0
    end

    # Julia's passes and pipeline, which only exist in Julia's LLVM
    if LLVM.julia_llvm
        @dispose pb=PassBuilder() begin
            add!(pb, JuliaPipeline(opt_level=2))
            printed = sprint(print_pipeline, pb)
            @test occursin("AllocOpt", printed)
            @test !occursin("julia<", printed)
        end
    end

    # with the passes of a target machine
    host_triple = LLVM.default_triple()
    host_t = LLVM.Target(triple=host_triple)
    @dispose tm=LLVM.TargetMachine(host_t, host_triple) pb=PassBuilder() begin
        add!(pb, "instsimplify")
        @test sprint(print_pipeline, pb, tm) == "function(instsimplify)"
    end

    # errors
    @dispose pb=PassBuilder() begin
        add!(pb, "unknown-pass")
        @test_throws LLVM.LLVMException print_pipeline(devnull, pb)
    end
    @dispose pb=PassBuilder() begin
        register!(pb, ModulePass("bad-options", (mod, opts) -> false;
                                 parse_options=str -> error("bad options")))
        add!(pb, "bad-options<x>")
        @test_throws LLVM.PassException print_pipeline(devnull, pb)
    end
end

@testset "passes" begin
    function test_passes(typ, passes, skips=String[])
        @dispose ctx=Context() begin
            for pass in passes
                if startswith(pass, "print") || startswith(pass, "dot") ||
                   startswith(pass, "view") || pass in skips
                    continue
                end

                # the first pass determines the pass manager's type, so add the pass and
                # then a no-op one that would trigger an error in case of type mismatches.
                # use a fresh module per run so instrumentation passes (tsan/asan/hwasan)
                # don't trip their redundant-instrumentation check on the second invocation.
                @dispose pb=PassBuilder() mod=test_module() begin
                    add!(pb, pass)
                    add!(pb, "no-op-$typ")
                    @test run!(pb, mod) === nothing
                end

                # same, but to catch type mismatches in the other direction
                @dispose pb=PassBuilder() mod=test_module() begin
                    add!(pb, "no-op-$typ")
                    add!(pb, pass)
                    @test run!(pb, mod) === nothing
                end

                # XXX: add predicate functions to make these tests simpler?
            end
        end
    end

    @testset "module" begin
        bad_passes = [
            # requires additional set-up
            "function-import",
            "pgo-instr-use",
            "sample-profile",
            "inliner-ml-advisor-release",

            # unsupported
            "dfsan",
            "msan",

            # does bad things
            "trigger-crash",
            "trigger-crash-module",
            "check-debugify",
            "debugify",
        ]
        test_passes("module", LLVM.module_passes(), bad_passes)
    end

    @testset "cgscc" begin
        test_passes("cgscc", LLVM.cgscc_passes())
    end

    @testset "function" begin
        bad_passes = [
            # unsupported
            "msan",

            # does bad things
            "trigger-crash-function",
            "aa-eval",
            "chr",
            "helloworld"
        ]
        test_passes("function", LLVM.function_passes(), bad_passes)
    end

    @testset "loop" begin
        test_passes("loop", LLVM.loop_passes())
    end
end

@testset "custom passes" begin
    module_pass_calls = 0
    function custom_module_pass!(mod::LLVM.Module)
        module_pass_calls += 1
        return false
    end

    function_pass_calls = 0
    function custom_function_pass!(f::LLVM.Function)
        function_pass_calls += 1
        return false
    end
    CustomModulePass() = ModulePass("custom_module_pass", custom_module_pass!)
    CustomFunctionPass() = FunctionPass("custom_function_pass", custom_function_pass!)

    @dispose ctx=Context() mod=test_module() pb=PassBuilder() begin
        register!(pb, CustomModulePass())
        register!(pb, CustomFunctionPass())

        add!(pb, CustomModulePass())
        add!(pb, FunctionPassManager()) do fpm
            add!(fpm, CustomFunctionPass())
        end
        add!(pb, CustomModulePass())

        @test run!(pb, mod) === nothing
        @test module_pass_calls == 2
        @test function_pass_calls == 1
    end

    # custom passes are known to LLVM by their own name, e.g. when it logs the passes it runs
    @dispose ctx=Context() mod=test_module() pb=PassBuilder(debug_logging=true) begin
        register!(pb, CustomModulePass())
        register!(pb, CustomFunctionPass())
        register!(pb, FunctionPass("custom_pass_with_options", (f, am, opts) -> false;
                                   analyses=true, parse_options=identity))
        add!(pb, CustomModulePass())
        add!(pb, FunctionPassManager()) do fpm
            add!(fpm, CustomFunctionPass())
            add!(fpm, "custom_pass_with_options<some-option>")
        end

        io = IOCapture.capture() do
            run!(pb, mod)
        end
        @test contains(io.output, "Running pass: custom_module_pass on [module]")
        @test contains(io.output, "Running pass: custom_function_pass on SomeFunction")
        @test contains(io.output, "Running pass: custom_pass_with_options on SomeFunction")
        @test !contains(io.output, "JuliaCustom")
    end

    # running a single custom pass
    @dispose ctx=Context() mod=test_module() begin
        module_pass_calls = 0
        function_pass_calls = 0
        @test run!(CustomModulePass(), mod) === nothing
        @test module_pass_calls == 1
        # function passes run on every function of a module
        @test run!(CustomFunctionPass(), mod; verify_each=true) === nothing
        @test function_pass_calls == 1
        fn = mod.functions["SomeFunction"]
        @test run!(CustomFunctionPass(), fn) === nothing
        @test function_pass_calls == 2
        # but module passes can't run on a function
        @test_throws ArgumentError run!(CustomModulePass(), fn)
        @test module_pass_calls == 1
        # exceptions are rethrown after LLVM returns
        @test_throws LLVM.PassException run!(ModulePass("throwing_pass",
                                                        mod -> error("oops")), mod)
        @test_throws ArgumentError run!(CustomModulePass(), mod; invalid_option=true)
    end

    @dispose ctx=Context() mod=parse(LLVM.Module,
        "define void @f() noinline optnone { ret void }") begin
        fn = mod.functions["f"]
        optional_calls = Ref(0)
        required_calls = Ref(0)
        optional = FunctionPass("probe", _ -> (optional_calls[] += 1; false))
        required = FunctionPass("probe", _ -> (required_calls[] += 1; false);
                                required=true)
        run!(optional, fn)
        run!(required, fn)
        @test optional_calls[] == 0
        @test required_calls[] == 1
        run!(required, mod)
        @test required_calls[] == 2

        @dispose pb=PassBuilder() begin
            seen = String[]
            register!(pb, FunctionPass("clean", _ -> (push!(seen, "clean"); false)))
            register!(pb, FunctionPass("clean-extra", _ -> (push!(seen, "clean-extra"); false);
                                       required=true))
            @test_throws ArgumentError register!(pb, FunctionPass("clean", _ -> false))
            add!(pb, "function(clean-extra)")
            run!(pb, mod)
            @test seen == ["clean-extra"]
        end
        @dispose pb=PassBuilder() begin
            register!(pb, FunctionPass("clean", _ -> false))
            add!(pb, "function(clean<unknown>)")
            @test_throws LLVM.LLVMException run!(pb, mod)
        end
    end
end

@testset "registration callbacks" begin
    # native callbacks are called with the PassBuilder, for every run
    calls = @eval begin
        const registration_calls = Ref(0)
        function count_registration(pb::Ptr{Cvoid})
            pb == C_NULL || (registration_calls[] += 1)
            return
        end
        registration_calls
    end
    @dispose ctx=Context() mod=LLVM.Module("test") pb=PassBuilder() begin
        callback = @eval @cfunction(count_registration, Cvoid, (Ptr{Cvoid},))
        @test register_callbacks!(pb, callback) === pb
        @test_throws ArgumentError register_callbacks!(pb, C_NULL)
        add!(pb, NoOpModulePass())
        calls[] = 0
        run!(pb, mod)
        @test calls[] == 1
        run!(pb, mod)
        @test calls[] == 2
    end
end

@testset "expand reductions" begin
    # the pass is only registered by LLVM since LLVM 21, but works on all versions
    ir = """
        define i32 @f(<4 x i32> %v) {
          %r = call i32 @llvm.vector.reduce.add.v4i32(<4 x i32> %v)
          ret i32 %r
        }
        declare i32 @llvm.vector.reduce.add.v4i32(<4 x i32>)"""
    @dispose ctx=Context() begin
        # on a function, and as part of a module pipeline (using the generic TTI, which
        # asks for reductions to be expanded)
        @dispose mod=parse(LLVM.Module, ir) begin
            @test run!(ExpandReductionsPass(), mod.functions["f"]) === nothing
            @test !occursin("vector.reduce", string(mod.functions["f"]))
            verify(mod)
        end
        @dispose mod=parse(LLVM.Module, ir) begin
            @dispose pb=PassBuilder() begin
                add!(pb, FunctionPassManager()) do fpm
                    add!(fpm, ExpandReductionsPass())
                end
                run!(pb, mod)
            end
            @test !occursin("vector.reduce", string(mod.functions["f"]))
            verify(mod)
        end
    end
end

@testset "early vector combine" begin
    @test VectorCombinePass() == "vector-combine"
    if LLVM.version() < v"16"
        @test_throws ArgumentError VectorCombinePass(; early=true)
    else
        @test VectorCombinePass(; early=true) == "vector-combine<early>"

        # unlike LLVM, which prints both as `vector-combine`, the parameter is printed
        @dispose pb=PassBuilder() begin
            add!(pb, "function(vector-combine<early>,vector-combine<no-early>,vector-combine)")
            @test sprint(print_pipeline, pb) ==
                  "function(vector-combine<early>,vector-combine,vector-combine)"
        end

        # the early folds don't include scalarizing extracted operands
        if :X86 in LLVM.backends()
            LLVM.InitializeX86Target()
            LLVM.InitializeX86TargetInfo()
            LLVM.InitializeX86TargetMC()
            triple = "x86_64-unknown-linux-gnu"
            ir = """
                target triple = "$triple"
                define i32 @f(<4 x i32> %x, <4 x i32> %y) {
                  %a = extractelement <4 x i32> %x, i32 0
                  %b = extractelement <4 x i32> %y, i32 0
                  %r = add i32 %a, %b
                  ret i32 %r
                }"""
            @dispose tm=LLVM.TargetMachine(LLVM.Target(; triple), triple) begin
                for (early, folded) in ((false, true), (true, false))
                    @dispose ctx=Context() mod=parse(LLVM.Module, ir) begin
                        run!(VectorCombinePass(; early), mod, tm)
                        @test occursin("add <4 x i32>", string(mod)) == folded
                    end
                end
            end
        end

        @dispose ctx=Context() mod=test_module() begin
            @test_throws LLVM.LLVMException run!("vector-combine<invalid>", mod)
        end
    end
end

@testset "internalize" begin
    # preserving global values requires LLVM 19 (or Julia's LLVM 18), but works on all versions
    ir = """
        @g = global i32 0
        define void @f() {
          ret void
        }
        define void @h() {
          ret void
        }"""
    @dispose ctx=Context() mod=parse(LLVM.Module, ir) begin
        run!(InternalizePass(; preserved_gvs=["f", "g"]), mod)
        @test mod.functions["f"].linkage == LLVM.Linkage.External
        @test mod.globals["g"].linkage == LLVM.Linkage.External
        @test mod.functions["h"].linkage == LLVM.Linkage.Internal
        verify(mod)
    end
end

@testset "custom TTI" begin
    # IR with an `addrspacecast` from AS 2 to the generic AS. With no TTI
    # attached, `InferAddressSpacesPass` has no flat AS to infer against and
    # bails. With a TTI that reports AS 0 as flat and declares the cast a
    # noop, the pass folds it away and the load moves to AS 2.
    function make_mod()
        ir = if supports_typed_pointers()
            """
            @g = internal addrspace(2) constant i32 42
            define i32 @f() {
              %p = addrspacecast i32 addrspace(2)* @g to i32*
              %v = load i32, i32* %p
              ret i32 %v
            }
            """
        else
            """
            @g = internal addrspace(2) constant i32 42
            define i32 @f() {
              %p = addrspacecast ptr addrspace(2) @g to ptr
              %v = load i32, ptr %p
              ret i32 %v
            }
            """
        end
        return parse(LLVM.Module, ir)
    end
    has_addrspacecast(mod) = occursin("addrspacecast", string(mod.functions["f"]))

    # A do-nothing subtype: exercises the abstract defaults, which must match
    # LLVM's baseline well enough that InferAddressSpaces can't find a flat AS
    # and therefore folds nothing — same observable behavior as no TTI at all.
    struct BaselineTTI <: LLVM.AbstractTargetTransformInfo end
    struct PartialValueTTI <: LLVM.AbstractTargetTransformInfo end
    LLVM.get_assumed_addr_space(::PartialValueTTI, ::LLVM.GlobalVariable) = 1
    @test_throws ArgumentError LLVM.build_custom_tti_options(PartialValueTTI())

    @dispose ctx=Context() mod=make_mod() begin
        @dispose pb=PassBuilder() begin
            target_transform_info!(pb, BaselineTTI())
            add!(pb, FunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
            end
            run!(pb, mod)
        end
        @test has_addrspacecast(mod)
    end

    # With an overriding subtype: cast gets folded.
    struct FlatZeroTTI <: LLVM.AbstractTargetTransformInfo end
    LLVM.flat_address_space(::FlatZeroTTI) = UInt(0)
    LLVM.is_noop_addr_space_cast(::FlatZeroTTI, from::Unsigned, to::Unsigned) =
        from == 0 || to == 0

    @dispose ctx=Context() mod=make_mod() begin
        @dispose pb=PassBuilder() begin
            target_transform_info!(pb, FlatZeroTTI())
            add!(pb, FunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
            end
            run!(pb, mod)
        end
        @test !has_addrspacecast(mod)
    end

    # Operand lists that don't fit the C API's buffer are reported, not truncated.
    struct ManyOperandsTTI <: LLVM.AbstractTargetTransformInfo end
    LLVM.collect_flat_address_operands(::ManyOperandsTTI, ::UInt) = collect(1:41)
    let state = LLVM.CustomTTIState(ManyOperandsTTI())
        ops = Vector{Cint}(undef, 32)
        count = Ref{Cuint}()
        GC.@preserve state ops count begin
            ret = LLVM.custom_tti_collect_flat_address_operands_callback(
                Cuint(0), pointer(ops), Cuint(32),
                Base.unsafe_convert(Ptr{Cuint}, count), pointer_from_objref(state))
        end
        @test !Bool(ret)
        @test count[] == 0
        @test state.exception !== nothing
        @test first(state.exception) isa ArgumentError
    end

    # Operand positions are 1-based, and are checked.
    struct OperandsTTI <: LLVM.AbstractTargetTransformInfo
        ops::Vector{Int}
    end
    LLVM.collect_flat_address_operands(tti::OperandsTTI, ::UInt) = tti.ops
    function collect_operands(tti)
        state = LLVM.CustomTTIState(tti)
        ops = zeros(Cint, 32)
        count = Ref{Cuint}()
        GC.@preserve state ops count begin
            LLVM.custom_tti_collect_flat_address_operands_callback(
                Cuint(0), pointer(ops), Cuint(32),
                Base.unsafe_convert(Ptr{Cuint}, count), pointer_from_objref(state))
        end
        return ops[1:count[]], state.exception
    end
    @test collect_operands(OperandsTTI([1, 3])) == (Cint[0, 2], nothing)
    let (ops, exception) = collect_operands(OperandsTTI([0]))
        @test isempty(ops)
        @test first(exception) isa ArgumentError
    end

    # Address spaces are integers, or `nothing` for none.
    struct AddrSpaceTTI <: LLVM.AbstractTargetTransformInfo
        as::Any
    end
    LLVM.get_assumed_addr_space(tti::AddrSpaceTTI, ::LLVM.Value) = tti.as
    LLVM.get_predicated_addr_space(tti::AddrSpaceTTI, v::LLVM.Value) =
        tti.as === nothing ? nothing : (v, tti.as)
    @dispose ctx=Context() begin
        val = LLVM.ConstantInt(Int32(0))
        ref = Base.unsafe_convert(LLVM.API.LLVMValueRef, val)
        function query(tti)
            state = LLVM.CustomTTIState(tti)
            pred = Ref{LLVM.API.LLVMValueRef}(ref)
            GC.@preserve state pred begin
                assumed = LLVM.custom_tti_get_assumed_address_space_callback(
                    ref, pointer_from_objref(state))
                predicated = LLVM.custom_tti_get_predicated_address_space_callback(
                    ref, Base.unsafe_convert(Ptr{LLVM.API.LLVMValueRef}, pred),
                    pointer_from_objref(state))
            end
            return assumed, predicated, pred[], state.exception
        end
        @test query(AddrSpaceTTI(3)) == (3, 3, ref, nothing)
        @test query(AddrSpaceTTI(nothing)) == (typemax(Cuint), typemax(Cuint), C_NULL, nothing)
        # the old sentinel is rejected, as are address spaces that pointers can't have
        for as in (typemax(UInt), 2^24, -1)
            let (assumed, predicated, pred, exception) = query(AddrSpaceTTI(as))
                @test assumed == typemax(Cuint)
                @test first(exception) isa ArgumentError
            end
        end
        @test query(AddrSpaceTTI(2^24 - 1))[1] == 2^24 - 1
    end

    # Assumptions can imply the address space of a pointer.
    struct AssumeTTI <: LLVM.AbstractTargetTransformInfo end
    LLVM.flat_address_space(::AssumeTTI) = UInt(0)
    LLVM.is_noop_addr_space_cast(::AssumeTTI, from::Unsigned, to::Unsigned) =
        from == 0 || to == 0
    function LLVM.get_predicated_addr_space(::AssumeTTI, cond::LLVM.Value)
        # `llvm.assume(is_shared(ptr))` implies that `ptr` is in address space 3
        cond isa LLVM.CallInst || return nothing
        f = cond.called_function
        f !== nothing && f.name == "is_shared" || return nothing
        return (cond.arguments[1], 3)
    end
    @dispose ctx=Context() begin
        # `ptr` is `i32*` with typed pointers
        ptr = supports_typed_pointers() ? "i32*" : "ptr"
        mod = parse(LLVM.Module, """
            declare i1 @is_shared($ptr)
            declare void @llvm.assume(i1)

            define i32 @f($ptr %p) {
              %c = call i1 @is_shared($ptr %p)
              call void @llvm.assume(i1 %c)
              %q = getelementptr i32, $ptr %p, i64 1
              %v = load i32, $ptr %q
              ret i32 %v
            }""")
        @dispose pb=PassBuilder() begin
            target_transform_info!(pb, AssumeTTI())
            add!(pb, FunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
            end
            run!(pb, mod)
        end
        @test occursin(supports_typed_pointers() ? "load i32, i32 addrspace(3)*" :
                                                   "load i32, ptr addrspace(3)", string(mod))
        dispose(mod)
    end

    # Overrides are found when they are specialized on the argument types that the
    # callbacks pass (they used to be ignored unless they accepted any `Unsigned`).
    struct FlatZeroUIntTTI <: LLVM.AbstractTargetTransformInfo end
    LLVM.flat_address_space(::FlatZeroUIntTTI) = UInt(0)
    LLVM.is_noop_addr_space_cast(::FlatZeroUIntTTI, from::UInt, to::UInt) =
        from == 0 || to == 0

    @dispose ctx=Context() mod=make_mod() begin
        @dispose pb=PassBuilder() begin
            target_transform_info!(pb, FlatZeroUIntTTI())
            add!(pb, FunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
            end
            run!(pb, mod)
        end
        @test !has_addrspacecast(mod)
    end

    # Callbacks fire: observe the counter getting incremented.
    # `get_assumed_addr_space` is queried unconditionally per pointer Value
    # during inference, making it a reliable observable; the other callbacks
    # fire only on paths InferAddressSpaces may or may not take for a given
    # module.
    let calls = Ref(0)
        # Subtype-local field lets the method see per-instance state.
        struct CountingTTI <: LLVM.AbstractTargetTransformInfo
            calls::Base.RefValue{Int}
        end
        LLVM.flat_address_space(::CountingTTI) = UInt(0)
        LLVM.is_noop_addr_space_cast(::CountingTTI, from::Unsigned, to::Unsigned) =
            from == 0 || to == 0
        LLVM.get_assumed_addr_space(t::CountingTTI, ::LLVM.Value) =
            (t.calls[] += 1; nothing)

        @dispose ctx=Context() mod=make_mod() begin
            @dispose pb=PassBuilder() begin
                target_transform_info!(pb, CountingTTI(calls))
                add!(pb, FunctionPassManager()) do fpm
                    add!(fpm, InferAddressSpacesPass())
                end
                run!(pb, mod)
            end
        end
        @test calls[] > 0
    end

    # A target without a flat address space doesn't fold the cast.
    struct NoFlatTTI <: LLVM.AbstractTargetTransformInfo end
    LLVM.flat_address_space(::NoFlatTTI) = nothing
    LLVM.is_noop_addr_space_cast(::NoFlatTTI, from::Unsigned, to::Unsigned) = true
    @dispose ctx=Context() mod=make_mod() begin
        @dispose pb=PassBuilder() begin
            target_transform_info!(pb, NoFlatTTI())
            add!(pb, FunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
            end
            run!(pb, mod)
        end
        @test has_addrspacecast(mod)
    end

    # `target_transform_info!(pb, nothing)` reverts to LLVM's native TTI.
    @dispose ctx=Context() mod=make_mod() begin
        @dispose pb=PassBuilder() begin
            target_transform_info!(pb, FlatZeroTTI())
            target_transform_info!(pb, nothing)
            add!(pb, FunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
            end
            run!(pb, mod)
        end
        @test has_addrspacecast(mod)
    end

    # Exceptions in TTI callbacks are caught and rethrown as PassException.
    struct BoomTTI <: LLVM.AbstractTargetTransformInfo
        calls::Base.RefValue{Int}
    end
    LLVM.flat_address_space(::BoomTTI) = UInt(0)
    function LLVM.get_assumed_addr_space(tti::BoomTTI, ::LLVM.Value)
        tti.calls[] += 1
        error("TTI callback boom")
    end

    @dispose ctx=Context() mod=make_mod() begin
        calls = Ref(0)
        @dispose pb=PassBuilder() begin
            target_transform_info!(pb, BoomTTI(calls))
            add!(pb, FunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
                add!(fpm, InferAddressSpacesPass())
            end
            @test_throws LLVM.PassException run!(pb, mod)
        end
        @test calls[] == 1
    end
end

@testset "custom passes with analyses" begin
    diamond = """
        define i32 @f(i1 %c, i32 %x) {
        entry:
          br i1 %c, label %then, label %join
        then:
          %y = add i32 %x, 1
          br label %join
        join:
          %r = phi i32 [ %y, %then ], [ %x, %entry ]
          ret i32 %r
        }"""

    # querying analyses
    @dispose ctx=Context() mod=parse(LLVM.Module, diamond) begin
        results = Dict{Symbol,Any}()
        function query!(f, am)
            @test am isa FunctionAnalysisManager
            entry, then, join = f.blocks
            results[:cached_before] = get(am, DomTree, nothing)
            domtree = am[DomTree]
            results[:dominates] = dominates(domtree, entry.terminator, then.terminator)
            results[:not_dominates] = dominates(domtree, then.terminator, join.terminator)
            results[:cached_after] = get(am, DomTree, nothing) isa DomTree
            postdomtree = am[PostDomTree]
            results[:postdominates] = dominates(postdomtree, join.terminator, then.terminator)
            invalidate!(am)
            results[:cached_invalidated] = get(am, DomTree, nothing)
            @test_throws ArgumentError am[Int]
            return false
        end
        run!(FunctionPass("query", query!; analyses=true), mod)
        @test results[:cached_before] === nothing
        @test results[:dominates]
        @test !results[:not_dominates]
        @test results[:cached_after]
        @test results[:postdominates]
        @test results[:cached_invalidated] === nothing
    end

    # preserving analyses
    @dispose ctx=Context() mod=parse(LLVM.Module, diamond) begin
        for (preserved, expected) in [false => true, true => false,
                                      PreservedAnalyses() => false,
                                      PreservedAnalyses(AllAnalyses) => true,
                                      PreservedAnalyses(CFGAnalyses) => true,
                                      PreservedAnalyses(DomTree) => true,
                                      PreservedAnalyses(PostDomTree) => false]
            cached = Ref{Bool}()
            @dispose pb=PassBuilder() begin
                register!(pb, FunctionPass("compute", (f, am) -> (am[DomTree]; preserved);
                                           analyses=true))
                register!(pb, FunctionPass("check", (f, am) -> begin
                    cached[] = get(am, DomTree, nothing) !== nothing
                    return false
                end; analyses=true))
                add!(pb, "function(compute,check)")
                run!(pb, mod)
            end
            @test cached[] == expected
        end
    end

    # invalidating only what isn't preserved
    @dispose ctx=Context() mod=parse(LLVM.Module, diamond) begin
        cached = Bool[]
        run!(FunctionPass("partial", (f, am) -> begin
            am[DomTree]
            am[PostDomTree]
            invalidate!(am, PreservedAnalyses(DomTree))
            push!(cached, get(am, DomTree, nothing) !== nothing)
            push!(cached, get(am, PostDomTree, nothing) !== nothing)
            return false
        end; analyses=true), mod)
        @test cached == [true, false]
    end

    # the description of preserved analyses
    @test PreservedAnalyses() == PreservedAnalyses()
    @test PreservedAnalyses(CFGAnalyses, DomTree) == PreservedAnalyses(DomTree, CFGAnalyses)
    @test PreservedAnalyses(DomTree) != PreservedAnalyses(PostDomTree)
    @test convert(PreservedAnalyses, false) == PreservedAnalyses(AllAnalyses)
    @test convert(PreservedAnalyses, true) == PreservedAnalyses()
    @test sprint(show, PreservedAnalyses(CFGAnalyses, DomTree)) ==
          "PreservedAnalyses($(CFGAnalyses), $(DomTree))"
    @test_throws ArgumentError PreservedAnalyses(Int)

    # invalid results and exceptions are rethrown after LLVM returns
    @dispose ctx=Context() mod=parse(LLVM.Module, diamond) begin
        @test_throws LLVM.PassException run!(FunctionPass("bad", (f, am) -> 42;
                                                          analyses=true), mod)
        @test_throws LLVM.PassException run!(FunctionPass("throws", (f, am) -> error("oops");
                                                          analyses=true), mod)
        # a pass without analyses=true is called with only the function
        @test_throws LLVM.PassException run!(FunctionPass("arity", (f, am) -> false), mod)
    end
end

@testset "custom module passes with analyses" begin
    two_functions = """
        define i32 @f(i32 %x) {
        entry:
          %c = icmp ult i32 %x, 10
          br i1 %c, label %then, label %exit
        then:
          %y = add i32 %x, 1
          ret i32 %y
        exit:
          ret i32 0
        }

        define i32 @g(i32 %x) {
        entry:
          %c = icmp ult i32 %x, 100
          br i1 %c, label %then, label %exit
        then:
          %y = add i32 %x, 1
          ret i32 %y
        exit:
          ret i32 0
        }"""

    # x's range in the `then` block, according to LVI
    function then_range(am, f)
        entry, then, exit = f.blocks
        x = f.parameters[1]
        return ConstantRange(am[f][LazyValueInfo], x; at=then.terminator)
    end

    # querying the analyses of two functions, changing one, and invalidating its results
    @dispose ctx=Context() mod=parse(LLVM.Module, two_functions) begin
        results = Dict{Symbol,Any}()
        function ipo!(mod, am)
            @test am isa ModuleAnalysisManager
            f, g = mod.functions["f"], mod.functions["g"]
            for fun in (f, g)
                entry, then, exit = fun.blocks
                @test dominates(am[fun][DomTree], entry.terminator, then.terminator)
            end
            results[:f_before] = then_range(am, f)
            results[:g_before] = then_range(am, g)

            # change f's condition, which LVI doesn't notice until f is invalidated
            cmp = first(f.entry.instructions)
            cmp.operands[2] = ConstantInt(Int32(5))
            results[:f_stale] = then_range(am, f)
            invalidate!(am[f])
            results[:f_domtree] = get(am[f], DomTree, nothing)
            results[:g_domtree] = get(am[g], DomTree, nothing) !== nothing
            results[:f_after] = then_range(am, f)
            return PreservedAnalyses(FunctionAnalyses)
        end
        cached = Dict{String,Bool}()
        @dispose pb=PassBuilder() begin
            register!(pb, ModulePass("ipo", ipo!; analyses=true))
            register!(pb, FunctionPass("check", (f, am) -> begin
                cached[f.name] = get(am, DomTree, nothing) !== nothing
                return false
            end; analyses=true))
            add!(pb, "ipo,function(check)")
            run!(pb, mod)
        end
        @test results[:f_before] == ConstantRange(32, 0, 10)
        @test results[:g_before] == ConstantRange(32, 0, 100)
        @test results[:f_stale] == ConstantRange(32, 0, 10)
        @test results[:f_domtree] === nothing
        @test results[:g_domtree]
        @test results[:f_after] == ConstantRange(32, 0, 5)
        # g's dominator tree was kept, f's wasn't recomputed
        @test cached == Dict("f" => false, "g" => true)
    end

    # preserving analyses
    @dispose ctx=Context() mod=parse(LLVM.Module, two_functions) begin
        for (preserved, expected) in [false => true, true => false,
                                      PreservedAnalyses() => false,
                                      PreservedAnalyses(AllAnalyses) => true,
                                      PreservedAnalyses(FunctionAnalyses) => true,
                                      PreservedAnalyses(CFGAnalyses) => true,
                                      PreservedAnalyses(DomTree) => true,
                                      PreservedAnalyses(PostDomTree) => false]
            cached = Bool[]
            @dispose pb=PassBuilder() begin
                register!(pb, ModulePass("compute", (mod, am) -> begin
                    foreach(f -> am[f][DomTree], mod.functions)
                    return preserved
                end; analyses=true))
                register!(pb, FunctionPass("check", (f, am) -> begin
                    push!(cached, get(am, DomTree, nothing) !== nothing)
                    return false
                end; analyses=true))
                add!(pb, "compute,function(check)")
                run!(pb, mod)
            end
            @test cached == [expected, expected]
        end
    end

    # clearing results, e.g., before deleting a function
    @dispose ctx=Context() mod=parse(LLVM.Module, two_functions) begin
        cached = Bool[]
        run!(ModulePass("delete", (mod, am) -> begin
            f = mod.functions["f"]
            am[f][DomTree]
            am[f][AssumptionCache]
            # the assumption cache survives invalidation
            invalidate!(am[f])
            push!(cached, get(am[f], DomTree, nothing) !== nothing)
            push!(cached, get(am[f], AssumptionCache, nothing) !== nothing)
            empty!(am[f])
            push!(cached, get(am[f], AssumptionCache, nothing) !== nothing)
            erase!(f)
            return PreservedAnalyses(FunctionAnalyses)
        end; analyses=true), mod)
        @test cached == [false, true, false]
        @test !haskey(mod.functions, "f")
    end

    # invalid uses
    @dispose ctx=Context() mod=parse(LLVM.Module, two_functions) other=parse(LLVM.Module, two_functions) begin
        decl = LLVM.Function(mod, "decl", LLVM.FunctionType(LLVM.VoidType()))
        errors = Any[]
        run!(ModulePass("invalid", (mod, am) -> begin
            push!(errors, try am[other.functions["f"]] catch err err end)
            push!(errors, try am[DomTree] catch err err end)
            # declarations have no analyses
            push!(errors, try am[decl][DomTree] catch err err end)
            push!(errors, get(am[decl], DomTree, nothing))
            invalidate!(am[decl])
            empty!(am[decl])
            return false
        end; analyses=true), mod)
        @test errors[1] isa ArgumentError
        @test errors[2] isa ArgumentError
        @test errors[3] isa ArgumentError
        @test errors[4] === nothing
        @test sprint(show, PreservedAnalyses(FunctionAnalyses, DomTree)) ==
              "PreservedAnalyses($(FunctionAnalyses), $(DomTree))"

        # function passes can preserve all function analyses too
        @test run!(FunctionPass("all-functions",
            (f, am) -> PreservedAnalyses(FunctionAnalyses); analyses=true), mod) === nothing
        @test_throws LLVM.PassException run!(ModulePass("bad", (mod, am) -> 42;
                                                        analyses=true), mod)
        @test_throws LLVM.PassException run!(ModulePass("throws", (mod, am) -> error("oops");
                                                        analyses=true), mod)
    end
end

@testset "running passes from a custom pass" begin
    # what is preserved after two passes
    let all = PreservedAnalyses(AllAnalyses), none = PreservedAnalyses()
        @test intersect(all, all) == all
        @test intersect(all, none) == none
        @test intersect(PreservedAnalyses(DomTree), all) == PreservedAnalyses(DomTree)
        @test intersect(PreservedAnalyses(CFGAnalyses, DomTree, LoopInfo),
                        PreservedAnalyses(CFGAnalyses, DomTree)) ==
              PreservedAnalyses(CFGAnalyses, DomTree)
        # like LLVM, a set of analyses does not imply the analyses in it
        @test intersect(PreservedAnalyses(FunctionAnalyses),
                        PreservedAnalyses(CFGAnalyses, DomTree)) == none
    end

    ir = """
        define i32 @f(i32 %x) {
        entry:
          %y = add i32 %x, 0
          br label %exit
        exit:
          ret i32 %y
        }"""
    @dispose ctx=Context() mod=parse(LLVM.Module, ir) begin
        results = []
        function nested!(f, am)
            domtree = am[DomTree]
            # the passes use the analyses of the pipeline, and update them
            push!(results, run!(InstSimplifyPass(), f, am))
            push!(results, get(am, DomTree, nothing) === domtree)
            push!(results, run!(InstSimplifyPass(), f, am))
            push!(results, run!(SimplifyCFGPass(), f, am))
            push!(results, get(am, DomTree, nothing))
            return PreservedAnalyses(FunctionAnalyses)
        end
        @dispose pb=PassBuilder(debug_logging=true) begin
            register!(pb, FunctionPass("nested", nested!; analyses=true))
            add!(pb, "function(nested)")
            io = IOCapture.capture() do
                run!(pb, mod)
            end
            # the passes are part of the pipeline's instrumentation
            @test contains(io.output, "Running pass: InstSimplifyPass on f")
            @test count("Running analysis: DominatorTreeAnalysis", io.output) == 1
        end
        # instsimplify changed the function, but not its control flow
        @test results[1] == PreservedAnalyses(FunctionAnalyses, CFGAnalyses)
        @test results[2]
        # there was nothing left to simplify
        @test results[3] == PreservedAnalyses(AllAnalyses)
        # simplifycfg merged the blocks
        @test !results[4].all && results[4].functions && !results[4].cfg
        @test results[5] === nothing
        @test length(mod.functions["f"].blocks) == 1
    end

    # passes registered with the pass builder can run, including custom passes
    @dispose ctx=Context() mod=parse(LLVM.Module, ir) begin
        calls = Ref(0)
        inner = FunctionPass("inner", f -> (calls[] += 1; false))
        result = Ref{Any}()
        run!(FunctionPass("outer", (f, am) -> begin
            @dispose pb=PassBuilder() begin
                register!(pb, inner)
                add!(pb, inner)
                add!(pb, "inner")
                result[] = run!(pb, f, am)
            end
            return result[]
        end; analyses=true), mod)
        @test calls[] == 2
        @test result[] == PreservedAnalyses(AllAnalyses)
    end

    # errors are thrown from the outer pass
    @dispose ctx=Context() mod=parse(LLVM.Module, ir) begin
        f = mod.functions["f"]
        function outer_exception(body)
            try
                run!(FunctionPass("outer", (f, am) -> (body(f, am); false); analyses=true),
                     mod)
            catch err
                err isa LLVM.PassException || rethrow()
                return err.ex
            end
            return nothing
        end
        @test outer_exception((f, am) -> run!("unknown-pass", f, am)) isa LLVM.LLVMException
        let err = outer_exception((f, am) -> run!(FunctionPass("inner", f -> error("oops")),
                                                  f, am))
            @test err isa LLVM.PassException
            @test err.ex == ErrorException("oops")
        end
        @test outer_exception((f, am) -> run!(ModulePass("inner", mod -> false), f, am)) isa
              ArgumentError
        @test outer_exception((f, am) -> run!("instsimplify", LLVM.Function(mod, "g",
                                              LLVM.FunctionType(LLVM.VoidType())), am)) isa
              ArgumentError
        @test outer_exception((f, am) -> @dispose(pb=PassBuilder(), begin
                                  add!(pb, AAManager())
                                  add!(pb, "instsimplify")
                                  run!(pb, f, am)
                              end)) isa ArgumentError
    end
end

@testset "custom passes with options" begin
    # every occurrence of a pass gets its own options
    @dispose ctx=Context() mod=test_module() begin
        seen = Tuple{String,Any}[]
        parsed = String[]
        parse = str -> (push!(parsed, str); isempty(str) ? :default : Symbol(str))
        @dispose pb=PassBuilder() begin
            register!(pb, FunctionPass("fopts", (f, opts) -> begin
                push!(seen, (f.name, opts))
                return false
            end; parse_options=parse))
            register!(pb, ModulePass("mopts", (mod, am, opts) -> begin
                @test am isa ModuleAnalysisManager
                push!(seen, ("module", opts))
                return false
            end; analyses=true, parse_options=parse))
            add!(pb, "mopts<first>,function(fopts<a;b=1>,fopts,fopts<>),mopts")
            run!(pb, mod)
        end
        @test seen == [("module", :first),
                       ("SomeFunction", Symbol("a;b=1")), ("SomeFunction", :default),
                       ("SomeFunction", :default), ("module", :default)]
        # the parser can be called more than once per occurrence
        @test issubset(["first", "a;b=1", ""], parsed)
    end

    # options are kept alive while the pipeline runs
    @dispose ctx=Context() mod=test_module() begin
        values = Int[]
        pass = FunctionPass("rooted", (f, opts) -> begin
            GC.gc()
            push!(values, opts.value[])
            return false
        end; parse_options=str -> (; value=Ref(parse(Int, str))))
        @dispose pb=PassBuilder() begin
            register!(pb, pass)
            add!(pb, "function(rooted<1>,rooted<2>)")
            GC.gc()
            run!(pb, mod)
        end
        @test values == [1, 2]
    end

    # passes with options and analyses
    @dispose ctx=Context() mod=test_module() begin
        limits = Int[]
        pass = FunctionPass("limited", (f, am, limit) -> begin
            am[DomTree]
            push!(limits, limit)
            return PreservedAnalyses(AllAnalyses)
        end; analyses=true, parse_options=str -> isempty(str) ? 10 : parse(Int, str))
        @dispose pb=PassBuilder() begin
            register!(pb, pass)
            add!(pb, "function(limited<42>)")
            run!(pb, mod)
        end
        run!(pass, mod)
        @test limits == [42, 10]
    end

    # exceptions in the parser abort parsing, and take precedence over LLVM's error
    @dispose ctx=Context() mod=test_module() begin
        ran = Ref(0)
        pass = FunctionPass("checked", (f, n) -> (ran[] += 1; false);
                            parse_options=str -> parse(Int, str))
        @dispose pb=PassBuilder() begin
            register!(pb, pass)
            add!(pb, "function(checked<1>,checked<oops>)")
            err = try run!(pb, mod) catch err err end
            @test err isa LLVM.PassException
            @test err.ex isa ArgumentError
        end
        @test ran[] == 0

        # the pass can be used again after a failure
        @dispose pb=PassBuilder() begin
            register!(pb, pass)
            add!(pb, "function(checked<1>)")
            run!(pb, mod)
        end
        @test ran[] == 1

        # names that don't match, and options for passes that don't take them
        @dispose pb=PassBuilder() begin
            register!(pb, pass)
            add!(pb, "function(checked<1)")
            @test_throws LLVMException run!(pb, mod)
        end
        @dispose pb=PassBuilder() begin
            register!(pb, FunctionPass("plain", f -> false))
            add!(pb, "function(plain<1>)")
            @test_throws LLVMException run!(pb, mod)
        end
    end
end

@testset "optimization remarks" begin
    ir = """
        define i32 @f(i32 %x) {
          %y = add i32 %x, 1
          ret i32 %y
        }

        define i32 @g(i32 %x) {
          ret i32 %x
        }"""

    # remarks from function and module passes are collected
    @dispose ctx=Context() mod=parse(LLVM.Module, ir) begin
        remarks = Remark[]
        @dispose pb=PassBuilder(; remarks=r -> push!(remarks, r)) begin
            register!(pb, FunctionPass("remarking", (f, am) -> begin
                inst = first(f.entry.instructions)
                remark!(am, :passed, "remarking", "Visited", inst, "visited $(f.name)")
                return false
            end; analyses=true))
            register!(pb, ModulePass("module-remarking", (mod, am) -> begin
                f = mod.functions["f"]
                remark!(am[f], :missed, "module-remarking", "Skipped",
                        first(f.entry.instructions), "skipped the add")
                remark!(am[f], :analysis, "module-remarking", "Range", f.entry.terminator,
                        "returns a value")
                return false
            end; analyses=true))
            add!(pb, "module-remarking,function(remarking)")
            run!(pb, mod)
        end
        @test length(remarks) == 4
        @test remarks[1].kind === :missed
        @test remarks[1].pass_name == "module-remarking"
        @test remarks[1].name == "Skipped"
        @test remarks[1].function_name == "f"
        @test remarks[1].message == "skipped the add"
        @test remarks[1].file == ""
        @test remarks[2].kind === :analysis
        @test [(r.kind, r.function_name, r.message) for r in remarks[3:4]] ==
              [(:passed, "f", "visited f"), (:passed, "g", "visited g")]
        @test occursin("skipped the add", sprint(show, remarks[1]))

        # without a callback, remarks are logged as debug messages
        pass = FunctionPass("remarking", (f, am) -> begin
            remark!(am, :passed, "remarking", "Visited", f.entry.terminator, "visited")
            return false
        end; analyses=true)
        run!(pass, mod)

        # the callback is only used during the run
        remarks = Remark[]
        run!(pass, mod; remarks=r -> push!(remarks, r))
        @test length(remarks) == 2
        run!(pass, mod)
        @test length(remarks) == 2

        # nested runs collect their own remarks, and restore those of the outer run
        outer = Remark[]
        inner_error = Ref{Any}(nothing)
        nesting = FunctionPass("nesting", (f, am) -> begin
            remark!(am, :passed, "nesting", "Before", f.entry.terminator, "before")
            @dispose inner_mod=parse(LLVM.Module, ir) begin
                inner_error[] = try
                    run!(pass, inner_mod; remarks=r -> error("inner"))
                catch err
                    err
                end
            end
            remark!(am, :passed, "nesting", "After", f.entry.terminator, "after")
            return false
        end; analyses=true)
        run!(nesting, mod.functions["f"]; remarks=r -> push!(outer, r))
        @test [r.name for r in outer] == ["Before", "After"]
        @test inner_error[] isa LLVM.PassException

        # exceptions are rethrown after LLVM returns
        err = try
            run!(pass, mod; remarks=r -> error("oops"))
        catch err
            err
        end
        @test err isa LLVM.PassException
        @test err.ex isa ErrorException
        @test_throws LLVM.PassException run!(FunctionPass("bad-kind", (f, am) -> begin
            remark!(am, :bogus, "bad-kind", "Bad", f.entry.terminator, "")
            return false
        end; analyses=true), mod)
        @test_throws LLVM.PassException run!(FunctionPass("other-function", (f, am) -> begin
            other = mod.functions[f.name == "f" ? "g" : "f"]
            remark!(am, :passed, "other-function", "Bad", other.entry.terminator, "")
            return false
        end; analyses=true), mod)
    end

    # messages made of arguments, which LLVM renders like those of its own passes
    ir_dbg = """
        declare void @llvm.assume(i1)
        declare void @julia.foo()

        define i32 @f(i32 %x, i1 %c) !dbg !3 {
          call void @llvm.assume(i1 %c)
          call void @julia.foo()
          %y = add i32 %x, 1, !dbg !6
          ret i32 %y
        }

        !llvm.module.flags = !{!0}
        !llvm.dbg.cu = !{!1}
        !0 = !{i32 2, !"Debug Info Version", i32 3}
        !1 = distinct !DICompileUnit(language: DW_LANG_C, file: !2, emissionKind: FullDebug)
        !2 = !DIFile(filename: "file.jl", directory: "/dir")
        !3 = distinct !DISubprogram(name: "f", scope: !2, file: !2, line: 1, scopeLine: 2,
                                    type: !4, unit: !1, spFlags: DISPFlagDefinition)
        !4 = !DISubroutineType(types: !5)
        !5 = !{}
        !6 = !DILocation(line: 3, column: 7, scope: !3)"""
    @dispose ctx=Context() mod=parse(LLVM.Module, ir_dbg) begin
        f = mod.functions["f"]
        intrinsic, call, add, ret = collect(f.entry.instructions)
        x, c = LLVM.parameters(f)
        built = Ref(0)
        pass = FunctionPass("arguments", (f, am) -> begin
            remark!(am, :passed, "arguments", "Values", add) do
                built[] += 1
                ("values: ", "Intrinsic" => intrinsic, ", ", "Call" => call, ", ",
                 "Inst" => add, ", ", "Arg" => x, ", ", "Const" => ConstantInt(Int32(42)),
                 ", ", "Func" => f)
            end
            remark!(am, :missed, "arguments", "Others", ret) do
                built[] += 1
                ("Type" => LLVM.value_type(add), "Int" => -1, "UInt" => UInt8(2),
                 "Bool" => true, "Str" => "text")
            end
            remark!(am, :analysis, "arguments", "Single", ret) do
                built[] += 1
                "a single string"
            end
            remark!(am, :passed, "arguments", "Eager", ret, "eager ", "Inst" => ret)
            return false
        end; analyses=true)

        # lazily built remarks are only built when they are wanted
        @test !remark_enabled(ctx, :passed, "arguments")
        run!(pass, mod)
        @test built[] == 0

        # a logger that wants LLVM.jl's debug messages is a consumer too
        built[] = 0
        logs, _ = Test.collect_test_logs(; min_level=Base.CoreLogging.Debug) do
            @test remark_enabled(ctx, :passed, "arguments")
            run!(pass, mod)
        end
        @test built[] == 3
        @test any(log -> occursin("a single string", string(log.message)), logs)
        built[] = 0

        remarks = Remark[]
        run!(pass, mod; remarks=r -> push!(remarks, r))
        @test built[] == 3
        @test [r.name for r in remarks] == ["Values", "Others", "Single", "Eager"]
        @test [r.kind for r in remarks] == [:passed, :missed, :analysis, :passed]

        # since LLVM 21, intrinsic calls are rendered with the name of the intrinsic
        intrinsic_str = LLVM.version() >= v"21" ? "call llvm.assume" : "call"
        @test remarks[1].message ==
              "values: $intrinsic_str, call, add, x, 42, f"
        @test remarks[1].arguments == ["String" => "values: ", "Intrinsic" => intrinsic_str,
                                       "String" => ", ", "Call" => "call",
                                       "String" => ", ", "Inst" => "add",
                                       "String" => ", ", "Arg" => "x",
                                       "String" => ", ", "Const" => "42",
                                       "String" => ", ", "Func" => "f"]
        @test remarks[2].file == ""
        @test remarks[2].arguments == ["Type" => "i32", "Int" => "-1", "UInt" => "2",
                                       "Bool" => "true", "Str" => "text"]
        @test remarks[2].message == "i32-12truetext"
        @test remarks[3].arguments == ["String" => "a single string"]
        @test remarks[4].arguments == ["String" => "eager ", "Inst" => "ret"]

        # the remark of an instruction with a debug location has that location
        run!(FunctionPass("located", (f, am) -> begin
            remark!(() -> "located", am, :passed, "located", "Located", add)
            return false
        end; analyses=true), mod; remarks=r -> push!(remarks, r))
        @test remarks[end].file == joinpath("/dir", "file.jl")
        @test remarks[end].line == 3
        @test remarks[end].column == 7

        # invalid parts are rejected before building the remark
        @test_throws LLVM.PassException run!(FunctionPass("bad-part", (f, am) -> begin
            remark!(() -> ("ok", 42), am, :passed, "bad-part", "Bad", ret)
            return false
        end; analyses=true), mod; remarks=r -> nothing)
        @test_throws LLVM.PassException run!(FunctionPass("bad-value", (f, am) -> begin
            remark!(am, :passed, "bad-value", "Bad", ret, "Float" => 1.5)
            return false
        end; analyses=true), mod)
        @test_throws ArgumentError remark_enabled(ctx, :bogus, "arguments")

        # remarks written to a file, with the arguments and their debug locations
        function parse_remarks(path, format)
            data = read(path)
            str(ref) = unsafe_string(Ptr{UInt8}(LLVM.API.LLVMRemarkStringGetData(ref)),
                                     LLVM.API.LLVMRemarkStringGetLen(ref))
            parsed = Pair{String,Vector{Pair{String,String}}}[]
            GC.@preserve data begin
                parser = format == :yaml ?
                    LLVM.API.LLVMRemarkParserCreateYAML(data, length(data)) :
                    LLVM.API.LLVMRemarkParserCreateBitstream(data, length(data))
                while (entry = LLVM.API.LLVMRemarkParserGetNext(parser)) != C_NULL
                    args = Pair{String,String}[]
                    arg = LLVM.API.LLVMRemarkEntryGetFirstArg(entry)
                    while arg != C_NULL
                        push!(args, str(LLVM.API.LLVMRemarkArgGetKey(arg)) =>
                                    str(LLVM.API.LLVMRemarkArgGetValue(arg)))
                        arg = LLVM.API.LLVMRemarkEntryGetNextArg(arg, entry)
                    end
                    push!(parsed, str(LLVM.API.LLVMRemarkEntryGetRemarkName(entry)) => args)
                    LLVM.API.LLVMRemarkEntryDispose(entry)
                end
                @test !Bool(LLVM.API.LLVMRemarkParserHasError(parser))
                LLVM.API.LLVMRemarkParserDispose(parser)
            end
            return parsed
        end
        mktempdir() do dir
            path = joinpath(dir, "remarks.yaml")
            # an invalid filter leaves the context as it was
            @test_throws LLVMException RemarkFile(path; passes="[")
            RemarkFile(path; passes="^arguments\$") do file
                # the context doesn't dispose of the file, so it should be reported as
                # leaked if it isn't disposed of
                LLVM.memcheck_enabled && @test LLVM.tracked_objects[file].owner === nothing
                @test remark_enabled(ctx, :passed, "arguments")
                @test remark_enabled(ctx, :analysis, "arguments")
                @test !remark_enabled(ctx, :passed, "other")
                @test_throws LLVMException RemarkFile(joinpath(dir, "other.yaml"))
                built[] = 0
                run!(pass, mod)
                @test built[] == 3
            end
            # remarks emitted after the file was disposed of are not written to it
            run!(pass, mod)
            @test built[] == 3
            # written as text, so with CRLF line endings on Windows
            yaml = replace(read(path, String), "\r\n" => "\n")
            @test count("--- !", yaml) == 4
            yaml = replace(yaml, r" +" => " ")
            @test occursin("--- !Missed\nPass: arguments\nName: Others", yaml)
            @test occursin("--- !Analysis\nPass: arguments\nName: Single", yaml)
            @test occursin("- Inst: add\n DebugLoc: { File: file.jl, Line: 3, Column: 7 }", yaml)
            @test occursin("- Func: f\n DebugLoc: { File: file.jl, Line: 2, Column: 0 }", yaml)
            @test occursin("- Type: i32\n", yaml)

            @test parse_remarks(path, :yaml) == [r.name => r.arguments for r in remarks[1:4]]

            # other formats, which older versions of LLVM write for a string table that is
            # stored elsewhere
            path = joinpath(dir, "remarks.bitstream")
            if LLVM.version() >= v"22"
                RemarkFile(path; format="bitstream") do file
                    run!(pass, mod)
                end
                @test parse_remarks(path, :bitstream) == [r.name => r.arguments for r in remarks[1:4]]
            else
                @test_throws LLVMException RemarkFile(path; format="bitstream")
            end
            @test_throws LLVMException RemarkFile(joinpath(dir, "remarks.txt");
                                                  format="bogus")
        end
    end

    # remarks enabled using LLVM's command-line options
    let (; out, err, success) = execute_code("""
            ir = "define void @f() { ret void }"
            @dispose ctx=Context() mod=parse(LLVM.Module, ir) begin
                LLVM.clopts("-pass-remarks-missed=^cl-missed\\\$",
                            "-pass-remarks-analysis=cl-")
                println("missed: ", remark_enabled(ctx, :missed, "cl-missed"), " ",
                        remark_enabled(ctx, :passed, "cl-missed"), " ",
                        remark_enabled(ctx, :missed, "cl-missed-other"))
                println("analysis: ", remark_enabled(ctx, :analysis, "cl-other"), " ",
                        remark_enabled(ctx, :analysis, "other"))
                built = String[]
                run!(FunctionPass("cl-missed", (f, am) -> begin
                    for (kind, pass) in [(:missed, "cl-missed"), (:passed, "cl-missed"),
                                         (:analysis, "cl-other"), (:missed, "cl-other")]
                        remark!(() -> push!(built, "\$kind \$pass")[end], am, kind, pass,
                                "Name", f.entry.terminator)
                    end
                    return false
                end; analyses=true), mod)
                println("built: ", join(built, ", "))
            end""")
        @test success
        @test occursin("missed: true false false", out)
        @test occursin("analysis: true false", out)
        @test occursin("built: missed cl-missed, analysis cl-other", out)
    end
end

@testset "custom pass exceptions" begin
    # Module pass exceptions are rethrown after LLVM returns.
    @dispose ctx=Context() mod=test_module() begin
        function throwing_pass!(mod::LLVM.Module)
            error("test error from pass")
        end

        @dispose pb=PassBuilder() begin
            pass = ModulePass("throwing-pass", throwing_pass!)
            register!(pb, pass)
            add!(pb, pass)

            @test_throws LLVM.PassException run!(pb, mod)
        end

        # The module remains usable after the deferred exception.
        @test verify(mod) === nothing
    end

    # Do not invoke a failed callback again in the same pipeline.
    @dispose ctx=Context() mod=test_module() begin
        attempts = 0
        function throwing_fn_pass!(f::LLVM.Function)
            attempts += 1
            error("function pass error")
        end

        @dispose pb=PassBuilder() begin
            pass = FunctionPass("throwing-fn-pass", throwing_fn_pass!)
            register!(pb, pass)
            add!(pb, FunctionPassManager()) do fpm
                add!(fpm, pass)
                add!(fpm, pass)
            end

            @test_throws LLVM.PassException run!(pb, mod)
            @test attempts == 1
        end
    end

    # Preserve the original exception and backtrace.
    @dispose ctx=Context() mod=test_module() begin
        function pass_with_message!(mod)
            throw(ArgumentError("specific error message"))
        end

        @dispose pb=PassBuilder() begin
            register!(pb, ModulePass("msg-pass", pass_with_message!))
            add!(pb, "msg-pass")

            try
                run!(pb, mod)
                @test false
            catch e
                @test e isa LLVM.PassException
                @test e.ex isa ArgumentError
                @test occursin("specific error message", string(e.ex))
                @test !isempty(e.processed_bt)
            end
        end
    end

    # Passes preceding the failure still run normally.
    @dispose ctx=Context() mod=test_module() begin
        success_count = 0
        function success_pass!(mod)
            success_count += 1
            return false
        end
        function throwing_pass!(mod)
            error("later error")
        end

        @dispose pb=PassBuilder() begin
            register!(pb, ModulePass("success-pass", success_pass!))
            register!(pb, ModulePass("throw-pass", throwing_pass!))
            add!(pb, "success-pass")
            add!(pb, "throw-pass")

            @test_throws LLVM.PassException run!(pb, mod)
            @test success_count == 1
        end
    end

    # a failure skips the rest of the pipeline, as it may have left the IR inconsistent:
    # LLVM's passes, and custom passes even if they are required
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define i32 @f() {
          %a = add i32 1, 2
          ret i32 %a
        }""") begin
        ran = String[]
        @dispose pb=PassBuilder() begin
            register!(pb, FunctionPass("fail", f -> error("failed")))
            register!(pb, FunctionPass("after", f -> (push!(ran, "after"); false);
                                       required=true))
            register!(pb, ModulePass("later", m -> (push!(ran, "later"); false)))
            add!(pb, "function(fail,instcombine,after),later")
            @test_throws LLVM.PassException run!(pb, mod)
        end
        @test isempty(ran)
        @test occursin("add i32 1, 2", string(mod))

        # the next run is not affected
        @dispose pb=PassBuilder() begin
            register!(pb, FunctionPass("after", f -> (push!(ran, "after"); false)))
            add!(pb, "function(instcombine,after)")
            run!(pb, mod)
        end
        @test ran == ["after"]
        @test !occursin("add i32 1, 2", string(mod))
    end

    # also when the failing pass runs from another pass, which then fails too
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define i32 @f() {
          %a = add i32 1, 2
          ret i32 %a
        }""") begin
        ran = String[]
        function nested!(f, am)
            @dispose pb=PassBuilder() begin
                register!(pb, FunctionPass("fail", f -> error("failed")))
                add!(pb, "fail,instcombine")
                return run!(pb, f, am)
            end
        end
        @dispose pb=PassBuilder() begin
            register!(pb, FunctionPass("nested", nested!; analyses=true))
            register!(pb, ModulePass("later", m -> (push!(ran, "later"); false)))
            add!(pb, "function(nested,instcombine),later")
            @test_throws LLVM.PassException run!(pb, mod)
        end
        @test isempty(ran)
        @test occursin("add i32 1, 2", string(mod))

        # unless it catches the exception, after which its pipeline continues
        function catching!(f, am)
            try
                nested!(f, am)
            catch err
                err isa LLVM.PassException && push!(ran, "caught")
            end
            return PreservedAnalyses()
        end
        @dispose pb=PassBuilder() begin
            register!(pb, FunctionPass("catching", catching!; analyses=true))
            register!(pb, ModulePass("later", m -> (push!(ran, "later"); false)))
            add!(pb, "function(catching),later")
            run!(pb, mod)
        end
        @test ran == ["caught", "later"]
        @test occursin("add i32 1, 2", string(mod))
    end

    # a pass builder can be run multiple times, with the callbacks of each run using
    # their own state (they used to refer to the state of the first run)
    @dispose ctx=Context() mod=LLVM.Module("test") begin
        runs = Ref(0)
        function counting_pass!(mod)
            runs[] += 1
            runs[] == 3 && error("third run")
            return false
        end

        @dispose pb=PassBuilder() begin
            register!(pb, ModulePass("counting-pass", counting_pass!))
            add!(pb, "counting-pass")
            run!(pb, mod)
            GC.gc(true)
            run!(pb, mod)
            @test runs[] == 2
            @test_throws LLVM.PassException run!(pb, mod)
            @test runs[] == 3
        end
    end

    # This deliberately violates the raw callback's no-throw contract. Run it
    # in a child process so native state abandoned by longjmp cannot affect the
    # rest of the test suite.
    script = joinpath(@__DIR__, "raw_newpm_longjmp.jl")
    project = dirname(Base.active_project())
    @test success(`$(Base.julia_cmd()) --startup-file=no --project=$project $script`)
end

@testset "custom loop passes" begin
    # a loop with invariant computations (%x and %s), which a pass can hoist to its
    # preheader (the entry block), and a store that MemorySSA describes
    function loop_ir(; attrs="", ptr=supports_typed_pointers() ? "i64*" : "ptr")
        """
        define void @f($ptr %p, i64 %n, i64 %a, i64 %b) $attrs {
        entry:
          br label %loop
        loop:
          %i = phi i64 [ 0, %entry ], [ %i.next, %loop ]
          %x = mul i64 %a, %b
          %s = add i64 %a, %b
          %y = add i64 %x, %s
          %v = add i64 %y, %i
          %q = getelementptr i64, $ptr %p, i64 %i
          store i64 %v, $ptr %q
          %i.next = add i64 %i, 1
          %c = icmp ult i64 %i.next, %n
          br i1 %c, label %loop, label %exit
        exit:
          ret void
        }"""
    end

    # hoist invariant instructions with the given opcode, by recreating them in the
    # preheader; this does not move memory operations, so MemorySSA remains valid
    function hoist!(loop::Loop, ar::LoopAnalysisResults, opcode=LLVM.API.LLVMMul)
        se = ar[ScalarEvolution]
        @test ar[DomTree] isa DomTree
        @test ar[LoopInfo] isa LoopInfo
        @test ar[AssumptionCache] isa AssumptionCache
        candidates = Instruction[]
        for bb in loop.blocks, inst in bb.instructions
            LLVM.opcode(inst) == opcode || continue
            all(op -> is_loop_invariant(loop, op), inst.operands) || continue
            is_safe_to_speculatively_execute(inst) || continue
            push!(candidates, inst)
        end
        isempty(candidates) && return PreservedAnalyses(AllAnalyses)
        @dispose builder=IRBuilder() begin
            position!(builder, LLVM.before(loop.preheader.terminator))
            for inst in candidates
                lhs, rhs = inst.operands
                hoisted = binop!(builder, opcode, lhs, rhs, "hoisted")
                forget!(se, inst)
                replace_uses!(inst, hoisted)
                erase!(inst)
                remark!(ar, :passed, "hoist", "Hoisted", hoisted, "hoisted $(hoisted.name)")
            end
        end
        mssa = get(ar, MemorySSA, nothing)
        mssa === nothing || verify(mssa)
        forget_loop_dispositions!(se)
        return PreservedAnalyses(LoopAnalyses, CFGAnalyses, MemorySSA)
    end

    hoisted_opcodes(f) =
        [LLVM.opcode(inst) for inst in f.entry.instructions if inst != f.entry.terminator]

    # in loop and loop-mssa pipelines, and on its own
    for pipeline in ["function(loop(hoist))", "function(loop-mssa(hoist))", "loop(hoist)",
                     "loop-mssa(hoist)", "hoist", "hoist,hoist", "function(loop-mssa(hoist,licm))"]
        @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir()) begin
            mssa_seen = Ref{Any}()
            @dispose pb=PassBuilder() begin
                register!(pb, LoopPass("hoist", (loop, ar) -> begin
                    mssa_seen[] = get(ar, MemorySSA, nothing)
                    hoist!(loop, ar)
                end))
                add!(pb, pipeline)
                run!(pb, mod)
            end
            f = mod.functions["f"]
            @test LLVM.API.LLVMMul in hoisted_opcodes(f)
            @test (mssa_seen[] isa MemorySSA) == occursin("mssa", pipeline)
            @test verify(mod) === nothing
        end
    end

    # running a loop pass directly
    @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir()) begin
        pass = LoopPass("hoist", hoist!)
        run!(pass, mod)
        @test hoisted_opcodes(mod.functions["f"]) == [LLVM.API.LLVMMul]
    end
    @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir()) begin
        run!(LoopPass("hoist", hoist!), mod.functions["f"])
        @test hoisted_opcodes(mod.functions["f"]) == [LLVM.API.LLVMMul]
    end

    # options
    @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir()) begin
        parse_opcode(str) = isempty(str) ? LLVM.API.LLVMMul :
                            str == "add" ? LLVM.API.LLVMAdd : throw(ArgumentError(str))
        @dispose pb=PassBuilder() begin
            register!(pb, LoopPass("hoist", hoist!; parse_options=parse_opcode))
            add!(pb, "hoist<add>")
            run!(pb, mod)
        end
        @test hoisted_opcodes(mod.functions["f"]) == [LLVM.API.LLVMAdd]
        @dispose pb=PassBuilder() begin
            register!(pb, LoopPass("hoist", hoist!; parse_options=parse_opcode))
            add!(pb, "function(loop-mssa(hoist,hoist<add>))")
            run!(pb, mod)
        end
        # the add of the hoisted values is now invariant too
        @test hoisted_opcodes(mod.functions["f"]) ==
              [LLVM.API.LLVMAdd, LLVM.API.LLVMMul, LLVM.API.LLVMAdd]
        @dispose pb=PassBuilder() begin
            register!(pb, LoopPass("hoist", hoist!; parse_options=parse_opcode))
            add!(pb, "function(loop(hoist<sub>))")
            err = try run!(pb, mod) catch err err end
            @test err isa LLVM.PassException
            @test err.ex isa ArgumentError
        end
    end

    # loop passes are known to LLVM by their own name, like other custom passes
    @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir()) begin
        pb = PassBuilder(debug_logging=true)
        register!(pb, LoopPass("count", (loop, ar) -> PreservedAnalyses(AllAnalyses)))
        register!(pb, LoopPass("count-with", (loop, ar, opts) -> PreservedAnalyses(AllAnalyses);
                               parse_options=identity))
        add!(pb, "function(loop-mssa(count,count-with<blocks>))")
        @test sprint(print_pipeline, pb) ==
              "function(loop-mssa(count,count-with<blocks>))"
        io = IOCapture.capture() do
            run!(pb, mod)
        end
        @test contains(io.output, "Running pass: count on ")
        @test contains(io.output, "Running pass: count-with on ")
        @test !contains(io.output, "JuliaCustom")
        dispose(pb)
    end

    # passes without analyses, and the order in which loops are visited
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        define void @nested(i64 %n) {
        entry:
          br label %outer
        outer:
          %i = phi i64 [ 0, %entry ], [ %i.next, %outer.latch ]
          br label %inner
        inner:
          %j = phi i64 [ 0, %outer ], [ %j.next, %inner ]
          %j.next = add i64 %j, 1
          %cj = icmp ult i64 %j.next, %n
          br i1 %cj, label %inner, label %outer.latch
        outer.latch:
          %i.next = add i64 %i, 1
          %ci = icmp ult i64 %i.next, %n
          br i1 %ci, label %outer, label %exit
        exit:
          ret void
        }""") begin
        visited = String[]
        run!(LoopPass("visit", loop -> (push!(visited, loop.header.name); false);
                      analyses=false), mod)
        @test visited == ["inner", "outer"]
    end

    # scalar evolution caches what it knows, which a pass that changes the IR forgets
    @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir()) begin
        results = Dict{Symbol,String}()
        run!(LoopPass("forget", (loop, ar) -> begin
            se = ar[ScalarEvolution]
            x = first(inst for inst in loop.header.instructions
                       if LLVM.opcode(inst) == LLVM.API.LLVMMul)
            results[:before] = string(se[x])
            x.operands[2] = ConstantInt(Int64(3))
            results[:stale] = string(se[x])
            forget!(se, x)
            results[:value] = string(se[x])
            count = string(backedge_taken_count(se, loop))
            cmp = first(loop.latch.terminator.operands)
            cmp.operands[2] = ConstantInt(Int64(10))
            forget!(se, loop)
            results[:count] = count
            results[:new_count] = string(backedge_taken_count(se, loop))
            return PreservedAnalyses(LoopAnalyses, CFGAnalyses)
        end), mod)
        @test results[:before] == results[:stale]
        @test results[:value] != results[:before]
        @test occursin("3", results[:value])
        @test occursin("%n", results[:count])
        @test !occursin("%n", results[:new_count])
    end

    # remarks
    @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir()) begin
        remarks = Remark[]
        @dispose pb=PassBuilder(; remarks=r -> push!(remarks, r)) begin
            register!(pb, LoopPass("hoist", hoist!))
            add!(pb, "function(loop-mssa(hoist))")
            run!(pb, mod)
        end
        @test length(remarks) == 1
        @test remarks[1].kind === :passed
        @test remarks[1].pass_name == "hoist"
        @test remarks[1].function_name == "f"
        @test remarks[1].message == "hoisted hoisted"
    end
    @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir()) begin
        built = Ref(0)
        pass = LoopPass("lazy", (loop, ar) -> begin
            remark!(ar, :analysis, "lazy", "Header", loop.header.terminator) do
                built[] += 1
                "header ends in ", "Inst" => loop.header.terminator
            end
            return false
        end)
        remarks = Remark[]
        @dispose pb=PassBuilder() begin
            register!(pb, pass)
            add!(pb, "function(loop(lazy))")
            run!(pb, mod)
        end
        @test built[] == 0
        @dispose pb=PassBuilder(; remarks=r -> push!(remarks, r)) begin
            register!(pb, pass)
            add!(pb, "function(loop(lazy))")
            run!(pb, mod)
        end
        @test built[] == 1
        @test only(remarks).message == "header ends in br"
        @test only(remarks).arguments == ["String" => "header ends in ", "Inst" => "br"]
    end

    # what a loop pass can return
    @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir()) begin
        for (preserved, (ok_loop, ok_mssa)) in
                [false => (true, true), true => (true, false),
                 PreservedAnalyses(AllAnalyses) => (true, true),
                 PreservedAnalyses(LoopAnalyses) => (true, false),
                 PreservedAnalyses(LoopAnalyses, MemorySSA) => (true, true),
                 PreservedAnalyses(LoopAnalyses, CFGAnalyses, MemorySSA) => (true, true),
                 PreservedAnalyses(DomTree) => (false, false),
                 PreservedAnalyses(FunctionAnalyses) => (false, false),
                 42 => (false, false)]
            for (pipeline, ok) in [("function(loop(ret))", ok_loop),
                                   ("function(loop-mssa(ret))", ok_mssa)]
                @dispose pb=PassBuilder() begin
                    register!(pb, LoopPass("ret", (loop, ar) -> preserved))
                    add!(pb, pipeline)
                    if ok
                        @test run!(pb, mod) === nothing
                    else
                        @test_throws LLVM.PassException run!(pb, mod)
                    end
                end
            end
        end
        @test PreservedAnalyses(LoopAnalyses, MemorySSA) ==
              PreservedAnalyses(MemorySSA, LoopAnalyses)
        @test sprint(show, PreservedAnalyses(LoopAnalyses, CFGAnalyses)) ==
              "PreservedAnalyses($(LoopAnalyses), $(CFGAnalyses))"
        # loop analyses can't be preserved by other passes
        @test_throws LLVM.PassException run!(FunctionPass("fun",
            (f, am) -> PreservedAnalyses(LoopAnalyses); analyses=true), mod)
        @test_throws LLVM.PassException run!(ModulePass("mod",
            (m, am) -> PreservedAnalyses(LoopAnalyses); analyses=true), mod)

        # the results are only valid during the pass
        escaped = Ref{Any}()
        run!(LoopPass("escape", (loop, ar) -> (escaped[] = ar; false)), mod)
        @test_throws ArgumentError escaped[][DomTree]
        @test_throws ArgumentError remark!(escaped[], :passed, "escape", "Escaped",
                                           mod.functions["f"].entry.terminator, "")
        # remarks are about instructions of the loop's function
        err = try run!(LoopPass("detached", (loop, ar) -> begin
            n = LLVM.parent(loop.header).parameters[2]
            @dispose builder=IRBuilder() begin
                # an instruction that is not part of a block
                inst = add!(builder, n, n)
                try
                    remark!(ar, :passed, "detached", "Detached", inst, "")
                finally
                    LLVM.API.LLVMDeleteInstruction(inst)
                end
            end
        end), mod) catch err err end
        @test err isa LLVM.PassException && err.ex isa ArgumentError
        # unsupported analyses
        @test_throws LLVM.PassException run!(LoopPass("bad", (loop, ar) -> ar[PostDomTree]), mod)
        @test_throws LLVM.PassException run!(LoopPass("nomssa", (loop, ar) -> ar[MemorySSA]), mod)
    end

    # required passes run on optnone functions
    @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir(attrs="noinline optnone")) begin
        optional_calls = Ref(0)
        required_calls = Ref(0)
        run!(LoopPass("probe", (loop, ar) -> (optional_calls[] += 1; false)), mod)
        run!(LoopPass("probe", (loop, ar) -> (required_calls[] += 1; false);
                      required=true), mod)
        @test optional_calls[] == 0
        @test required_calls[] == 1
    end

    # a failing loop pass aborts the pipeline, even in loop-mssa where it cannot claim to
    # have preserved MemorySSA otherwise
    for pipeline in ["function(loop(fail),count),count", "function(loop-mssa(fail),count),count",
                     "function(loop-mssa(fail,count-loop),count)", "fail,count-loop"]
        @dispose ctx=Context() mod=parse(LLVM.Module, loop_ir() * replace(loop_ir(), "@f" => "@g")) begin
            attempts = Ref(0)
            counts = Ref(0)
            @dispose pb=PassBuilder() begin
                register!(pb, LoopPass("fail", (loop, ar) -> begin
                    attempts[] += 1
                    # leave the IR changed
                    hoist!(loop, ar)
                    error("loop pass failed")
                end))
                register!(pb, LoopPass("count-loop", (loop, ar) -> (counts[] += 1; false);
                                       required=true))
                register!(pb, FunctionPass("count", f -> (counts[] += 1; false);
                                           required=true))
                register!(pb, ModulePass("count", f -> (counts[] += 1; false)))
                add!(pb, pipeline)
                err = try run!(pb, mod) catch err err end
                @test err isa LLVM.PassException
                @test err.ex isa ErrorException
            end
            @test attempts[] == 1
            @test counts[] == 0
        end
    end

    # MemorySSA can be verified after every pass of a loop-mssa pipeline
    result = execute_code("""
        using LLVM.Analysis
        LLVM.clopts("-verify-memoryssa")
        @dispose ctx=Context() begin
            ir = supports_typed_pointers() ? $(repr(loop_ir(ptr="i64*"))) :
                                             $(repr(loop_ir(ptr="ptr")))
            mod = parse(LLVM.Module, ir)
            function hoist!(loop, ar)
                verify(ar[MemorySSA])
                @dispose builder=IRBuilder() begin
                    position!(builder, LLVM.before(loop.preheader.terminator))
                    for inst in collect(loop.header.instructions)
                        LLVM.opcode(inst) == LLVM.API.LLVMMul || continue
                        hoisted = mul!(builder, inst.operands[1], inst.operands[2])
                        forget!(ar[ScalarEvolution], inst)
                        replace_uses!(inst, hoisted)
                        erase!(inst)
                    end
                end
                verify(ar[MemorySSA])
                return PreservedAnalyses(LoopAnalyses, CFGAnalyses, MemorySSA)
            end
            @dispose pb=PassBuilder() begin
                register!(pb, LoopPass("hoist", hoist!))
                add!(pb, "function(loop-mssa(hoist,licm))")
                run!(pb, mod)
            end
            println(string(mod))
        end""")
    result.success || println(result.err)
    @test result.success
    @test occursin("mul i64 %a, %b", result.out)
end

@testset "MemorySSA" begin
    # describe MemorySSA by the instructions and blocks of its accesses, to compare it with
    # one that is computed for the same IR
    function access_name(access)
        access isa MemoryPhi && return "phi($(access.block.name))"
        inst = access.instruction
        return inst === nothing ? "liveOnEntry" : strip(string(inst))
    end
    function describe(mssa::MemorySSA, f::LLVM.Function)
        lines = String[]
        for bb in f.blocks, access in memory_accesses(mssa, bb)
            if access isa MemoryPhi
                incoming = ["$(access_name(a)) from $(pred.name)" for (a, pred) in access.incoming]
                push!(lines, "$(bb.name): phi [$(join(sort(incoming), ", "))]")
            else
                push!(lines, "$(bb.name): $(access_name(access)) <- $(access_name(access.defining_access))")
            end
        end
        return lines
    end
    # MemorySSA as computed for the current IR of a function
    function fresh_description(mod::LLVM.Module, name::String)
        result = Ref{Vector{String}}()
        run!(FunctionPass("fresh", (f, am) -> begin
            f.name == name && (result[] = describe(am[MemorySSA], f))
            return false
        end; analyses=true), mod)
        return result[]
    end

    ptr = Context(supports_typed_pointers) ? "i64*" : "ptr"

    # querying MemorySSA
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        declare void @g()
        define i64 @f($ptr %p, $ptr %q, i1 %c) {
        entry:
          %x = load i64, $ptr %q
          br i1 %c, label %then, label %join
        then:
          store i64 %x, $ptr %p
          br label %join
        join:
          %y = add i64 %x, 1
          call void @g()
          %z = load i64, $ptr %p
          ret i64 %z
        }""") begin
        checked = Ref(false)
        run!(FunctionPass("query", (f, am) -> begin
            @test get(am, MemorySSA, nothing) === nothing
            mssa = am[MemorySSA]
            @test get(am, MemorySSA, nothing) == mssa
            verify(mssa)
            entry, then, join = collect(f.blocks)
            x = first(entry.instructions)
            store = first(then.instructions)
            y, call, z = collect(join.instructions)[1:3]

            live = live_on_entry(mssa)
            @test live isa MemoryDef
            @test live.instruction === nothing
            @test live.defining_access === nothing
            @test live.block == entry

            ax = memory_access(mssa, x)
            @test ax isa MemoryUse && ax isa MemoryUseOrDef && ax isa MemoryAccess
            @test ax.instruction == x
            @test ax.block == entry
            @test ax.defining_access == live
            @test memory_access(mssa, y) === nothing
            @test memory_access(mssa, entry) === nothing

            astore = memory_access(mssa, store)
            @test astore isa MemoryDef
            @test astore.defining_access == live
            phi = memory_access(mssa, join)
            @test phi isa MemoryPhi
            @test phi.block == join
            @test issetequal(phi.incoming, [(live, entry), (astore, then)])
            acall = memory_access(mssa, call)
            @test acall isa MemoryDef
            @test acall.defining_access == phi
            az = memory_access(mssa, z)
            @test az.defining_access == acall
            @test memory_accesses(mssa, join) == [phi, acall, az]
            @test memory_accesses(mssa, entry) == [ax]
            @test occursin("MemoryUse", sprint(show, ax))
            @test occursin("MemoryPhi", string(phi))

            # phis cannot be removed, as LLVM can't remove one that merges several states
            MemorySSAUpdater(mssa) do u
                @test_throws MethodError remove_access!(u, phi)
            end
            @test memory_access(mssa, join) == phi

            checked[] = true
            return PreservedAnalyses(AllAnalyses)
        end; analyses=true), mod)
        @test checked[]

        # preserving MemorySSA, which also requires preserving the dominator tree
        for (preserved, expected) in [PreservedAnalyses() => false,
                                      PreservedAnalyses(AllAnalyses) => true,
                                      PreservedAnalyses(CFGAnalyses) => false,
                                      PreservedAnalyses(MemorySSA) => false,
                                      PreservedAnalyses(MemorySSA, DomTree) => true,
                                      PreservedAnalyses(MemorySSA, CFGAnalyses) => true]
            # by function passes
            cached = Ref{Bool}()
            @dispose pb=PassBuilder() begin
                register!(pb, FunctionPass("compute", (f, am) -> (am[MemorySSA]; preserved);
                                           analyses=true))
                register!(pb, FunctionPass("check", (f, am) -> begin
                    cached[] = get(am, MemorySSA, nothing) !== nothing
                    return false
                end; analyses=true))
                add!(pb, "function(compute,check)")
                run!(pb, mod)
            end
            @test cached[] == expected

            # and by module passes
            cached[] = !expected
            @dispose pb=PassBuilder() begin
                register!(pb, ModulePass("compute", (mod, am) -> begin
                    am[mod.functions["f"]][MemorySSA]
                    return preserved
                end; analyses=true))
                register!(pb, ModulePass("check", (mod, am) -> begin
                    cached[] = get(am[mod.functions["f"]], MemorySSA, nothing) !== nothing
                    return false
                end; analyses=true))
                add!(pb, "compute,check")
                run!(pb, mod)
            end
            @test cached[] == expected
        end

        # and reported by the passes that a function pass runs
        reported = Ref{Any}()
        run!(FunctionPass("outer", (f, am) -> begin
            reported[] = run!(FunctionPass("keep", (f, am) -> PreservedAnalyses(MemorySSA,
                                                                             DomTree);
                                           analyses=true), f, am)
            return reported[]
        end; analyses=true), mod)
        @test MemorySSA in reported[].analyses
        @test intersect(reported[], PreservedAnalyses(MemorySSA)) == PreservedAnalyses(MemorySSA)
    end

    # updating MemorySSA in a function pass, and comparing the result with MemorySSA as
    # computed for the changed IR
    @dispose ctx=Context() mod=parse(LLVM.Module, """
        declare void @g()
        define i64 @f($ptr %p, $ptr %q, i64 %v) {
        entry:
          store i64 %v, $ptr %p
          %a = load i64, $ptr %q
          %s = add i64 %a, 1
          br label %next
        next:
          call void @g()
          %b = load i64, $ptr %p
          store i64 %b, $ptr %q
          ret i64 %b
        }""") begin
        updated = Ref{Vector{String}}()
        run!(FunctionPass("update", (f, am) -> begin
            mssa = am[MemorySSA]
            entry, next = collect(f.blocks)
            store1, a, s = collect(entry.instructions)[1:3]
            call, b, store2 = collect(next.instructions)[1:3]
            MemorySSAUpdater(mssa) do u
                @test u isa MemorySSAUpdater

                # an instruction that does not access memory has no access
                @test create_access_in_block!(u, s, entry, :end) === nothing

                # a new store before the load of %p, inserted as a definition
                new_store = @dispose builder=IRBuilder() begin
                    position!(builder, LLVM.before(b))
                    store!(builder, s, f.parameters[1])
                end
                def = create_access_before!(u, new_store, memory_access(mssa, b))
                @test def isa MemoryDef
                insert_def!(u, def; rename=true)
                @test def.defining_access == memory_access(mssa, call)
                @test memory_access(mssa, b).defining_access == def

                # a new load at the end of the entry block, inserted as a use
                new_load = @dispose builder=IRBuilder() begin
                    position!(builder, LLVM.before(entry.terminator))
                    load!(builder, LLVM.Int64Type(), f.parameters[2])
                end
                use = create_access_in_block!(u, new_load, entry, :end)
                @test use isa MemoryUse
                insert_use!(u, use)
                @test use.defining_access == memory_access(mssa, store1)

                # a new load after the call, with a known defining access
                load2 = @dispose builder=IRBuilder() begin
                    position!(builder, LLVM.before(new_store))
                    load!(builder, LLVM.Int64Type(), f.parameters[2])
                end
                use2 = create_access_after!(u, load2, memory_access(mssa, call);
                                            definition=memory_access(mssa, call))
                @test use2.defining_access == memory_access(mssa, call)

                # moving the first store after the load of %q, and then to the next block
                move!(store1, LLVM.after(a))
                move_after!(u, memory_access(mssa, store1), memory_access(mssa, a))
                @test memory_accesses(mssa, entry)[1] == memory_access(mssa, a)
                move!(store1, LLVM.before(call))
                move_to_place!(u, memory_access(mssa, store1), next, :beginning)
                @test memory_accesses(mssa, next)[1] == memory_access(mssa, store1)
                @test memory_access(mssa, call).defining_access ==
                      memory_access(mssa, store1)
                move!(store1, LLVM.before(new_load))
                move_before!(u, memory_access(mssa, store1), use)
                @test memory_access(mssa, store1).block == entry
                @test use.defining_access == memory_access(mssa, store1)
                @test memory_access(mssa, call).defining_access ==
                      memory_access(mssa, store1)

                # removing the last store
                remove_access!(u, store2)
                erase!(store2)
                @test length(memory_accesses(mssa, next)) == 4
                # instructions without an access are ignored
                @test remove_access!(u, s) === u

                # invalid updates
                @test_throws ArgumentError move_to_place!(u, memory_access(mssa, b), entry, :end)
                @test_throws ArgumentError move_to_place!(u, memory_access(mssa, b), next, :middle)
                @test_throws ArgumentError create_access_in_block!(u, s, next, :end)
                @test_throws ArgumentError remove_access!(u, live_on_entry(mssa))
                @test_throws ArgumentError insert_def!(u, live_on_entry(mssa))
                @test_throws ArgumentError create_access_before!(u, new_load,
                                                                 memory_access(mssa, b))
                @test_throws MethodError insert_def!(u, use)
                @test_throws TypeError create_access_in_block!(u, new_load, entry, :end;
                                                               definition=use)
            end
            verify(mssa)
            updated[] = describe(mssa, f)
            return PreservedAnalyses(MemorySSA)
        end; analyses=true), mod)
        @test verify(mod) === nothing
        @test updated[] == fresh_description(mod, "f")
    end

    # a loop pass that changes memory operations the way Julia's JuliaLICM does: it hoists
    # a call, a load and a store, and an allocation, which it clears using a new memset,
    # and sinks a call to the exits of the loop, cloning it
    licm_ir = """
        declare void @begin($ptr)
        declare void @end($ptr)
        declare $ptr @alloc(i64)
        declare void @use($ptr)
        define void @f($ptr %p, $ptr %q, i64 %n, i1 %c) {
        entry:
          br label %loop
        loop:
          %i = phi i64 [ 0, %entry ], [ %i.next, %latch ]
          call void @begin($ptr %p)
          %v = load i64, $ptr %q
          store i64 %v, $ptr %p
          %a = call $ptr @alloc(i64 8)
          call void @use($ptr %a)
          call void @end($ptr %p)
          br i1 %c, label %exit1, label %latch
        latch:
          %i.next = add i64 %i, 1
          %cmp = icmp ult i64 %i.next, %n
          br i1 %cmp, label %loop, label %exit2
        exit1:
          ret void
        exit2:
          ret void
        }"""
    # the pass is also run in a separate process, so it is defined from its source code
    licm_code = raw"""
        function licm!(loop::Loop, ar::LoopAnalysisResults; update::Bool=true)
            mssa = get(ar, MemorySSA, nothing)
            se = ar[ScalarEvolution]
            preheader = loop.preheader
            preheader === nothing && return PreservedAnalyses(AllAnalyses)
            u = mssa === nothing ? nothing : MemorySSAUpdater(mssa)
            try
                function move_before(inst, dest, where=:before_terminator)
                    move!(inst, LLVM.before(dest))
                    if u !== nothing && update
                        access = memory_access(mssa, inst)
                        access === nothing || move_to_place!(u, access, LLVM.parent(dest), where)
                    end
                    forget!(se, inst)
                end
                function create_access(new, ref, where)
                    (u === nothing || memory_access(mssa, ref) === nothing) && return
                    access = create_access_in_block!(u, new, LLVM.parent(new), where)
                    if access isa MemoryDef
                        insert_def!(u, access; rename=true)
                    elseif access isa MemoryUse
                        insert_use!(u, access; rename=true)
                    end
                end
                invariant(inst) = all(op -> is_loop_invariant(loop, op), inst.operands)
                calls(inst, name) = inst isa CallInst &&
                                    inst.called_operand.name == name
                for bb in reverse_postorder(loop, ar[LoopInfo]), inst in collect(bb.instructions)
                    if (calls(inst, "begin") || inst isa LoadInst || inst isa StoreInst) &&
                       invariant(inst)
                        move_before(inst, preheader.terminator)
                    elseif calls(inst, "alloc") && invariant(inst)
                        move_before(inst, preheader.terminator)
                        memset = @dispose builder=IRBuilder() begin
                            position!(builder, LLVM.before(preheader.terminator))
                            memset!(builder, inst, ConstantInt(Int8(0)), inst.operands[1];
                                    align=8)
                        end
                        if u !== nothing
                            access = create_access_in_block!(u, memset, preheader,
                                                             :before_terminator)
                            insert_def!(u, access; rename=true)
                        end
                    elseif calls(inst, "end")
                        exits = collect(loop.exit_blocks)
                        move_before(inst, first(exits[1].instructions), :beginning)
                        for exit in exits[2:end]
                            clone = copy(inst)
                            move!(clone, LLVM.after_phis(exit))
                            create_access(clone, inst, :beginning)
                        end
                    end
                end
            finally
                u === nothing || dispose(u)
            end
            # MemorySSA is only consistent with the IR if it was updated
            (mssa === nothing || !update) || verify(mssa)
            forget_loop_dispositions!(se)
            return PreservedAnalyses(LoopAnalyses, CFGAnalyses, MemorySSA)
        end
        """
    licm! = include_string(@__MODULE__, licm_code)

    # MemorySSA after the pass is what it would be for the new IR, and it remains usable
    # by LLVM's loop passes that follow
    for pipeline in ["function(loop-mssa(jl-licm))", "function(loop-mssa(jl-licm,licm))",
                     "function(loop(jl-licm))"]
        @dispose ctx=Context() mod=parse(LLVM.Module, licm_ir) begin
            updated = Ref{Any}(nothing)
            @dispose pb=PassBuilder() begin
                register!(pb, LoopPass("jl-licm", (loop, ar) -> begin
                    preserved = licm!(loop, ar)
                    mssa = get(ar, MemorySSA, nothing)
                    mssa === nothing || (updated[] = describe(mssa, LLVM.parent(loop.header)))
                    return preserved
                end))
                add!(pb, pipeline)
                run!(pb, mod)
            end
            @test verify(mod) === nothing
            f = mod.functions["f"]
            entry, loop, latch, exit1, exit2 = collect(f.blocks)
            callees = [inst.called_operand.name for inst in entry.instructions
                       if inst isa CallInst]
            @test callees[1:2] == ["begin", "alloc"]
            @test startswith(callees[3], "llvm.memset")
            @test count(inst -> inst isa LoadInst || inst isa StoreInst,
                        collect(entry.instructions)) == 2
            @test occursin("call void @end", string(first(exit1.instructions)))
            @test occursin("call void @end", string(first(exit2.instructions)))
            @test !occursin("@end", string(loop))
            if pipeline == "function(loop-mssa(jl-licm))"
                @test updated[] == fresh_description(mod, "f")
            end
        end
    end

    # without the updates, MemorySSA does not match the IR anymore
    @dispose ctx=Context() mod=parse(LLVM.Module, licm_ir) begin
        updated = Ref{Any}(nothing)
        @dispose pb=PassBuilder() begin
            register!(pb, LoopPass("jl-licm", (loop, ar) -> begin
                preserved = licm!(loop, ar; update=false)
                updated[] = describe(ar[MemorySSA], LLVM.parent(loop.header))
                return preserved
            end))
            add!(pb, "function(loop-mssa(jl-licm))")
            run!(pb, mod)
        end
        @test updated[] != fresh_description(mod, "f")
    end

    # LLVM verifies MemorySSA after every pass of a loop-mssa pipeline, with assertions
    result = execute_code("""
        using LLVM.Analysis
        LLVM.clopts("-verify-memoryssa")
        @dispose ctx=Context() begin
            mod = parse(LLVM.Module, $(repr(licm_ir)))
            licm! = include_string(Main, $(repr(licm_code)))
            @dispose pb=PassBuilder() begin
                register!(pb, LoopPass("jl-licm", licm!))
                add!(pb, "function(loop-mssa(jl-licm,licm))")
                run!(pb, mod)
            end
            println(string(mod))
        end""")
    result.success || println(result.err)
    @test result.success
    @test occursin("memset", result.out)
end

if LLVM.julia_llvm
@testset "julia" begin
    @testset "passes" begin
        @dispose ctx=Context() pb=PassBuilder() begin
            basicSimplifyCFGOptions =
                (forward_switch_cond=true,
                   switch_range_to_icmp=true,
                   switch_to_lookup=true)
            aggressiveSimplifyCFGOptions =
                (forward_switch_cond=true,
                   switch_range_to_icmp=true,
                   switch_to_lookup=true,
                   hoist_common_insts=true)
            add!(pb, ModulePassManager()) do mpm
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, GCInvariantVerifierPass())
                end
                add!(mpm, VerifierPass())
                add!(mpm, ForceFunctionAttrsPass())
                add!(mpm, Annotation2MetadataPass())
                add!(mpm, ConstantMergePass())
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, LowerExpectIntrinsicPass())
                    add!(fpm, PropagateJuliaAddrspacesPass())
                    add!(fpm, SimplifyCFGPass(; basicSimplifyCFGOptions...))
                    add!(fpm, DCEPass())
                    add!(fpm, SROAPass())
                end
                add!(mpm, AlwaysInlinerPass())
                add!(mpm, CGSCCPassManager()) do cgpm
                    add!(cgpm, FunctionPassManager()) do fpm
                        add!(fpm, AllocOptPass())
                        add!(fpm, Float2IntPass())
                        add!(fpm, LowerConstantIntrinsicsPass())
                    end
                end
                add!(mpm, CPUFeaturesPass())
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, SROAPass())
                    add!(fpm, InstCombinePass())
                    add!(fpm, JumpThreadingPass())
                    add!(fpm, CorrelatedValuePropagationPass())
                    add!(fpm, ReassociatePass())
                    add!(fpm, EarlyCSEPass())
                    add!(fpm, AllocOptPass())
                end
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, LoopPassManager()) do lpm
                        add!(lpm, LowerSIMDLoopPass())
                        add!(lpm, LoopRotatePass())
                    end
                    add!(fpm, LoopPassManager(use_memory_ssa=true)) do lpm
                        add!(lpm, LICMPass())
                        add!(lpm, JuliaLICMPass())
                        add!(lpm, SimpleLoopUnswitchPass())
                        add!(lpm, LICMPass())
                        add!(lpm, JuliaLICMPass())
                    end
                    add!(fpm, IRCEPass())
                    add!(fpm, LoopPassManager()) do lpm
                        add!(lpm, LoopInstSimplifyPass())
                        add!(lpm, LoopIdiomRecognizePass())
                        add!(lpm, IndVarSimplifyPass())
                        add!(lpm, LoopDeletionPass())
                        add!(lpm, LoopFullUnrollPass())
                    end
                    add!(fpm, SROAPass())
                    add!(fpm, InstSimplifyPass())
                    add!(fpm, GVNPass())
                    add!(fpm, MemCpyOptPass())
                    add!(fpm, SCCPPass())
                    add!(fpm, CorrelatedValuePropagationPass())
                    add!(fpm, DCEPass())
                    add!(fpm, IRCEPass())
                    add!(fpm, InstCombinePass())
                    add!(fpm, JumpThreadingPass())
                    add!(fpm, GVNPass())
                    add!(fpm, DSEPass())
                    add!(fpm, SimplifyCFGPass(; aggressiveSimplifyCFGOptions...))
                    add!(fpm, AllocOptPass())
                    add!(fpm, LoopPassManager()) do lpm
                        add!(lpm, LoopDeletionPass())
                        add!(lpm, LoopInstSimplifyPass())
                    end
                    add!(fpm, LoopDistributePass())
                    add!(fpm, InjectTLIMappings())
                    add!(fpm, LoopVectorizePass())
                    add!(fpm, LoopLoadEliminationPass())
                    add!(fpm, InstCombinePass())
                    add!(fpm, SimplifyCFGPass(; aggressiveSimplifyCFGOptions...))
                    add!(fpm, SLPVectorizerPass())
                    add!(fpm, VectorCombinePass())
                    add!(fpm, ADCEPass())
                    add!(fpm, LoopUnrollPass())
                    add!(fpm, WarnMissedTransformationsPass())
                end
                add!(mpm, FunctionPassManager()) do fpm
                    if VERSION < v"1.13.0-DEV.36"
                        add!(fpm, LowerExcHandlersPass())
                    end
                    add!(fpm, GCInvariantVerifierPass())
                end
                add!(mpm, RemoveNIPass())
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, LateLowerGCPass())
                    if VERSION >= v"1.11.0-DEV.208"
                        add!(fpm, FinalLowerGCPass())
                    end
                    if VERSION >= v"1.13.0-DEV.321"
                        add!(fpm, ExpandAtomicModifyPass())
                    end
                end
                if VERSION < v"1.11.0-DEV.208"
                    add!(mpm, FinalLowerGCPass())
                end
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, GVNPass())
                    add!(fpm, SCCPPass())
                    add!(fpm, DCEPass())
                end
                add!(mpm, LowerPTLSPass())
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, InstCombinePass())
                    add!(fpm, SimplifyCFGPass(; aggressiveSimplifyCFGOptions...))
                end
                add!(mpm, FunctionPassManager()) do fpm
                    if VERSION < v"1.12.0-DEV.1390"
                        add!(fpm, CombineMulAddPass())
                    end
                    add!(fpm, DivRemPairsPass())
                end
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, AnnotationRemarksPass())
                end
                add!(mpm, FunctionPassManager()) do fpm
                    add!(fpm, DemoteFloat16Pass())
                    add!(fpm, GVNPass())
                end
            end

            @dispose mod=test_module() begin
                @test run!(pb, mod) === nothing
            end
        end
    end
end
end

# on Windows, instrumentation needs LLVM 20 to work across libraries, including
# Julia's own compiler (llvm/llvm-project#108051)
if !Sys.iswindows() || LLVM.version() >= v"20"
@testset "instrumentation" begin
    # LLVM's pass instrumentation is configured through global command-line options,
    # which are only read once, so test it in a fresh process configured at startup.
    function instrument(opts...)
        args = join([get(ENV, "JULIA_LLVM_ARGS", ""), opts...], " ")
        execute_code("""
            @dispose ctx=Context() mod=LLVM.Module("test") begin
                ft = LLVM.FunctionType(LLVM.VoidType())
                for name in ("SomeFunction", "dead_func")
                    fn = LLVM.Function(mod, name, ft)
                    @dispose builder=IRBuilder() begin
                        position!(builder, LLVM.at_end(BasicBlock(fn, "entry")))
                        ret!(builder)
                    end
                end
                mod.functions["dead_func"].linkage = LLVM.API.LLVMInternalLinkage

                custom_pass!(fn::LLVM.Function) = false
                CustomPass() = FunctionPass("custom-pass", custom_pass!)
                NestingPass() = FunctionPass("nesting-pass", (fn, am) ->
                    run!(FunctionPass("nested-pass", custom_pass!), fn, am); analyses=true)
                @dispose pb=PassBuilder() begin
                    register!(pb, CustomPass())
                    register!(pb, NestingPass())
                    add!(pb, NoOpModulePass())
                    add!(pb, FunctionPassManager()) do fpm
                        add!(fpm, NoOpFunctionPass())
                        add!(fpm, CustomPass())
                        add!(fpm, NestingPass())
                    end
                    add!(pb, GlobalDCEPass())
                    run!(pb, mod)
                end

                @dispose pb=PassBuilder() begin
                    add!(pb, EarlyCSEPass())
                    run!(pb, mod.functions["SomeFunction"])
                end
            end"""; env=("JULIA_LLVM_ARGS" => args,))
    end

    let (; err, success) = instrument("--print-after-all", "--filter-print-funcs=SomeFunction")
        @test success
        @test occursin("IR Dump After NoOpModulePass on [module]", err)
        @test occursin("IR Dump After NoOpFunctionPass on SomeFunction", err)
        # custom passes are reported by their own name
        @test occursin("IR Dump After custom-pass on SomeFunction", err)
        @test occursin("IR Dump After EarlyCSEPass on SomeFunction", err)
        @test !occursin("on dead_func", err)
    end

    # `-print-before` selects passes by their pipeline name, which requires registering the
    # mapping from class names; `-print-changed` compares the IR before and after each pass
    let (; err, success) = instrument("--print-before=globaldce", "--print-changed=quiet",
                                      "--filter-print-funcs=dead_func")
        @test success
        @test occursin("IR Dump Before GlobalDCEPass on [module]", err)
        @test occursin("IR Deleted After GlobalDCEPass on [module]", err)
        @test !occursin("NoOpModulePass", err)
    end

    # which also works for custom passes, including ones that a custom pass runs
    let (; err, success) = instrument("--print-before=custom-pass,nested-pass",
                                      "--filter-print-funcs=SomeFunction")
        @test success
        @test occursin("IR Dump Before custom-pass on SomeFunction", err)
        @test occursin("IR Dump Before nested-pass on SomeFunction", err)
        @test !occursin("NoOpFunctionPass", err)
    end
end
end

@testset "alias analyses" begin
    # default pipeline
    @dispose ctx=Context() mod=test_module() pb=PassBuilder(debug_logging=true) begin
        add!(pb, "aa-eval")

        io = IOCapture.capture() do
            run!(pb, mod)
        end

        @test contains(io.output, "Running analysis: BasicAA")
        @test contains(io.output, "Running analysis: TypeBasedAA")
        @test contains(io.output, "Running analysis: ScopedNoAliasAA")
    end

    # custom pipeline
    @dispose ctx=Context() mod=test_module() pb=PassBuilder(debug_logging=true) begin
        add!(pb, AAManager()) do aam
            # by string
            add!(aam, "basic-aa")

            # by object
            add!(aam, SCEVAA())
        end
        add!(pb, "aa-eval")

        io = IOCapture.capture() do
            run!(pb, mod)
        end

        @test contains(io.output, "Running analysis: BasicAA")
        @test contains(io.output, "Running analysis: SCEVAA")
        @test !contains(io.output, "Running analysis: TypeBasedAA")
        @test !contains(io.output, "Running analysis: ScopedNoAliasAA")
    end

    # an empty pipeline disables alias analysis
    for empty_aa! in (pb -> add!(pb, AAManager()), pb -> add!(aam -> nothing, pb, AAManager()))
        @dispose ctx=Context() mod=test_module() pb=PassBuilder(debug_logging=true) begin
            empty_aa!(pb)
            add!(pb, "aa-eval")

            io = IOCapture.capture() do
                run!(pb, mod)
            end

            @test contains(io.output, "Running analysis: AAManager")
            @test !contains(io.output, "Running analysis: BasicAA")
            @test !contains(io.output, "Running analysis: TypeBasedAA")
        end
    end

    # the target machine's alias analyses are part of the default pipeline, and can be
    # added to others
    if :AMDGPU in LLVM.backends()
        LLVM.InitializeAMDGPUTarget()
        LLVM.InitializeAMDGPUTargetInfo()
        LLVM.InitializeAMDGPUTargetMC()
        triple = "amdgcn-amd-amdhsa"
        @dispose tm=LLVM.TargetMachine(LLVM.Target(; triple), triple) begin
            function aa_log(aa_passes...; tm=tm)
                @dispose ctx=Context() mod=test_module() pb=PassBuilder(debug_logging=true) begin
                    if !isempty(aa_passes)
                        add!(pb, AAManager()) do aam
                            foreach(aa -> add!(aam, aa), aa_passes)
                        end
                    end
                    add!(pb, "aa-eval")
                    IOCapture.capture() do
                        run!(pb, mod, tm)
                    end.output
                end
            end
            @test contains(aa_log(), "Running analysis: AMDGPUAA")
            @test !contains(aa_log(BasicAA()), "Running analysis: AMDGPUAA")
            let log = aa_log(BasicAA(), TargetAA())
                @test contains(log, "Running analysis: BasicAA")
                @test contains(log, "Running analysis: AMDGPUAA")
            end
            @test !contains(aa_log(TargetAA()), "Running analysis: BasicAA")
            # without a target machine, there are no target alias analyses
            @test !contains(aa_log(TargetAA(); tm=nothing), "AMDGPUAA")
        end
    end
end

@static if LLVM.version() >= v"17"
@testset "callbacks" begin
    @dispose ctx=Context() begin
        # Each pseudo-pass pairs a (pipeline-context, pass-name, constructor)
        # where `constructor()` must return the same canonical string.
        module_cbs = [
            ("",       "pipeline-start-callbacks",                PipelineStartCallbacks),
            ("",       "pipeline-early-simplification-callbacks", PipelineEarlySimplificationCallbacks),
            ("",       "optimizer-early-callbacks",               OptimizerEarlyCallbacks),
            ("",       "optimizer-last-callbacks",                OptimizerLastCallbacks),
            ("cgscc",  "cgscc-optimizer-late-callbacks",          CGSCCOptimizerLateCallbacks),
            ("function", "peephole-callbacks",                    PeepholeCallbacks),
            ("function", "scalar-optimizer-late-callbacks",       ScalarOptimizerLateCallbacks),
            ("function", "vectorizer-start-callbacks",            VectorizerStartCallbacks),
            ("loop",   "late-loop-optimizations-callbacks",       LateLoopOptimizationsCallbacks),
            ("loop",   "loop-optimizer-end-callbacks",            LoopOptimizerEndCallbacks),
        ]
        if LLVM.version() >= v"21"
            push!(module_cbs,
                ("function", "vectorizer-end-callbacks", VectorizerEndCallbacks))
        end

        # The pseudo-passes parse and run without error, both as raw strings
        # and through their Julia constructors.
        for (wrap, name, ctor) in module_cbs
            pipeline = isempty(wrap) ? "$(name)<O0>" : "$(wrap)($(name)<O0>)"
            @dispose mod=test_module() begin
                @test run!(pipeline, mod) === nothing
            end
            @dispose mod=test_module() begin
                wrapped = isempty(wrap) ? ctor(opt_level=0) : "$(wrap)($(ctor(opt_level=0)))"
                @test run!(wrapped, mod) === nothing
            end
        end

        # End-to-end: a real TargetMachine whose `registerPassBuilderCallbacks`
        # hooks an EP should have its callback fan out when the matching
        # pseudo-pass runs. NVPTX registers `NVVMReflectPass` at PipelineStart,
        # which is observable through debug_logging.
        if :NVPTX in LLVM.backends()
            LLVM.InitializeNVPTXTarget()
            LLVM.InitializeNVPTXTargetInfo()
            LLVM.InitializeNVPTXTargetMC()
            triple = "nvptx64-nvidia-cuda"
            t = LLVM.Target(triple=triple)
            tm = LLVM.TargetMachine(t, triple; cpu="sm_80")
            try
                @dispose pb=PassBuilder(debug_logging=true) mod=test_module() begin
                    add!(pb, "pipeline-start-callbacks<O3>")
                    cap = IOCapture.capture() do
                        run!(pb, mod, tm)
                    end
                    @test occursin("NVVMReflectPass", cap.output)
                end
            finally
                dispose(tm)
            end
        end
    end
end
end # version() >= v"17"

end
