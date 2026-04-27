
@testset "newpm" begin

using LLVM.Interop
using IOCapture

function test_module()
    mod = LLVM.Module("test")
    ft = LLVM.FunctionType(LLVM.VoidType())
    fn = LLVM.Function(mod, "SomeFunction", ft)

    @dispose builder=IRBuilder() begin
        entry = BasicBlock(fn, "entry")
        position!(builder, entry)

        ret!(builder)
    end

    return mod
end

@testset "pass builder" begin
    @dispose ctx=Context() begin
        # single pass
        @dispose mod=test_module() begin
            fun = only(functions(mod))

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
            @test run!(DefaultPipeline(opt_level='s'), mod) === nothing
        end

        # custom pipelines
        @dispose pb=NewPMPassBuilder() mod=test_module() begin
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
            @dispose pb=NewPMPassBuilder(verify_each=true) begin
                add!(pb, "no-op-module")
                @test run!(pb, mod) === nothing
            end

            run!("no-op-module", mod; verify_each=true)
        end

        # target machines
        host_triple = triple()
        host_t = Target(triple=host_triple)
        @dispose tm=TargetMachine(host_t, host_triple) mod=test_module() begin
            @test run!(NoOpModulePass(), mod, tm) === nothing
            @test run!("no-op-module", mod, tm) === nothing
        end
    end
end

@testset "pass manager" begin
    @dispose ctx=Context() begin
        # pass manager interface
        @dispose pb=NewPMPassBuilder() mod=test_module() begin
            add!(pb, "no-op-module")
            add!(pb, NewPMModulePassManager()) do mpm
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
        @dispose pb=NewPMPassBuilder() mod=test_module() begin
            add!(pb, NewPMModulePassManager()) do mpm
                add!(mpm, "no-op-module")
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    add!(fpm, "no-op-function")
                    add!(fpm, NewPMLoopPassManager()) do lpm
                        add!(lpm, "no-op-loop")
                    end
                end
            end
            @test string(pb) == "module(no-op-module,function(no-op-function,loop(no-op-loop)))"

            @test run!(pb, mod) === nothing
        end
    end
end

@testset "passes" begin
    function test_passes(typ, passes, skips=String[])
        @dispose ctx=Context() mod=test_module() begin
            for pass in passes
                if startswith(pass, "print") || startswith(pass, "dot") ||
                   startswith(pass, "view") || pass in skips
                    continue
                end

                # the first pass determines the pass manager's type, so add the pass and
                # then a no-op one that would trigger an error in case of type mismatches.
                @dispose pb=NewPMPassBuilder() begin
                    add!(pb, pass)
                    add!(pb, "no-op-$typ")
                    @test run!(pb, mod) === nothing
                end

                # same, but to catch type mismatches in the other direction
                @dispose pb=NewPMPassBuilder() begin
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
            "check-debugify",
            "debugify",
        ]
        test_passes("module", LLVM.module_passes, bad_passes)
    end

    @testset "cgscc" begin
        test_passes("cgscc", LLVM.cgscc_passes)
    end

    @testset "function" begin
        bad_passes = [
            # unsupported
            "msan",

            # does bad things
            "aa-eval",
            "chr",
            "helloworld"
        ]
        test_passes("function", LLVM.function_passes, bad_passes)
    end

    @testset "loop" begin
        test_passes("loop", LLVM.loop_passes)
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
    CustomModulePass() = NewPMModulePass("custom_module_pass", custom_module_pass!)
    CustomFunctionPass() = NewPMFunctionPass("custom_function_pass", custom_function_pass!)

    @dispose ctx=Context() mod=test_module() pb=NewPMPassBuilder() begin
        register!(pb, CustomModulePass())
        register!(pb, CustomFunctionPass())

        add!(pb, CustomModulePass())
        add!(pb, NewPMFunctionPassManager()) do fpm
            add!(fpm, CustomFunctionPass())
        end
        add!(pb, CustomModulePass())

        @test run!(pb, mod) === nothing
        @test module_pass_calls == 2
        @test function_pass_calls == 1
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
    has_addrspacecast(mod) = occursin("addrspacecast", string(functions(mod)["f"]))

    # A do-nothing subtype: exercises the abstract defaults, which must match
    # LLVM's baseline well enough that InferAddressSpaces can't find a flat AS
    # and therefore folds nothing — same observable behavior as no TTI at all.
    struct BaselineTTI <: AbstractTargetTransformInfo end

    @dispose ctx=Context() mod=make_mod() begin
        @dispose pb=NewPMPassBuilder() begin
            target_transform_info!(pb, BaselineTTI())
            add!(pb, NewPMFunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
            end
            run!(pb, mod)
        end
        @test has_addrspacecast(mod)
    end

    # With an overriding subtype: cast gets folded.
    struct FlatZeroTTI <: AbstractTargetTransformInfo end
    LLVM.flat_address_space(::FlatZeroTTI) = UInt(0)
    LLVM.is_noop_addr_space_cast(::FlatZeroTTI, from::Unsigned, to::Unsigned) =
        from == 0 || to == 0

    @dispose ctx=Context() mod=make_mod() begin
        @dispose pb=NewPMPassBuilder() begin
            target_transform_info!(pb, FlatZeroTTI())
            add!(pb, NewPMFunctionPassManager()) do fpm
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
        struct CountingTTI <: AbstractTargetTransformInfo
            calls::Base.RefValue{Int}
        end
        LLVM.flat_address_space(::CountingTTI) = UInt(0)
        LLVM.is_noop_addr_space_cast(::CountingTTI, from::Unsigned, to::Unsigned) =
            from == 0 || to == 0
        LLVM.get_assumed_addr_space(t::CountingTTI, ::LLVM.Value) =
            (t.calls[] += 1; typemax(UInt))

        @dispose ctx=Context() mod=make_mod() begin
            @dispose pb=NewPMPassBuilder() begin
                target_transform_info!(pb, CountingTTI(calls))
                add!(pb, NewPMFunctionPassManager()) do fpm
                    add!(fpm, InferAddressSpacesPass())
                end
                run!(pb, mod)
            end
        end
        @test calls[] > 0
    end

    # `target_transform_info!(pb, nothing)` reverts to LLVM's native TTI.
    @dispose ctx=Context() mod=make_mod() begin
        @dispose pb=NewPMPassBuilder() begin
            target_transform_info!(pb, FlatZeroTTI())
            target_transform_info!(pb, nothing)
            add!(pb, NewPMFunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
            end
            run!(pb, mod)
        end
        @test has_addrspacecast(mod)
    end

    # Exceptions in TTI callbacks are caught and rethrown as PassException.
    struct BoomTTI <: AbstractTargetTransformInfo end
    LLVM.flat_address_space(::BoomTTI) = UInt(0)
    LLVM.get_assumed_addr_space(::BoomTTI, ::LLVM.Value) = error("TTI callback boom")

    @dispose ctx=Context() mod=make_mod() begin
        @dispose pb=NewPMPassBuilder() begin
            target_transform_info!(pb, BoomTTI())
            add!(pb, NewPMFunctionPassManager()) do fpm
                add!(fpm, InferAddressSpacesPass())
            end
            @test_throws LLVM.PassException run!(pb, mod)
        end
    end
end

@testset "custom pass exceptions" begin
    # Test that exceptions in module passes are caught and rethrown
    @dispose ctx=Context() mod=test_module() begin
        function throwing_pass!(mod::LLVM.Module)
            error("test error from pass")
        end

        @dispose pb=NewPMPassBuilder() begin
            pass = NewPMModulePass("throwing-pass", throwing_pass!)
            register!(pb, pass)
            add!(pb, pass)

            @test_throws LLVM.PassException run!(pb, mod)
        end

        # Test that module is still valid after exception
        @test verify(mod) === nothing
    end

    # Test function pass exception
    @dispose ctx=Context() mod=test_module() begin
        function throwing_fn_pass!(f::LLVM.Function)
            error("function pass error")
        end

        @dispose pb=NewPMPassBuilder() begin
            pass = NewPMFunctionPass("throwing-fn-pass", throwing_fn_pass!)
            register!(pb, pass)
            add!(pb, NewPMFunctionPassManager()) do fpm
                add!(fpm, pass)
            end

            @test_throws LLVM.PassException run!(pb, mod)
        end
    end

    # Test that exception message and backtrace are preserved
    @dispose ctx=Context() mod=test_module() begin
        function pass_with_message!(mod)
            throw(ArgumentError("specific error message"))
        end

        @dispose pb=NewPMPassBuilder() begin
            register!(pb, NewPMModulePass("msg-pass", pass_with_message!))
            add!(pb, "msg-pass")

            try
                run!(pb, mod)
                @test false  # Should not reach here
            catch e
                @test e isa LLVM.PassException
                @test e.ex isa ArgumentError
                @test occursin("specific error message", string(e.ex))
                @test !isempty(e.processed_bt)
            end
        end
    end

    # Test that a pass that succeeds before a throwing pass runs correctly
    @dispose ctx=Context() mod=test_module() begin
        success_count = 0
        function success_pass!(mod)
            success_count += 1
            return false
        end
        function throwing_pass!(mod)
            error("later error")
        end

        @dispose pb=NewPMPassBuilder() begin
            register!(pb, NewPMModulePass("success-pass", success_pass!))
            register!(pb, NewPMModulePass("throw-pass", throwing_pass!))
            add!(pb, "success-pass")
            add!(pb, "throw-pass")

            @test_throws LLVM.PassException run!(pb, mod)
            @test success_count == 1
        end
    end
end

@testset "julia" begin
    @testset "passes" begin
        @dispose ctx=Context() pb=NewPMPassBuilder() begin
            basicSimplifyCFGOptions =
                (forward_switch_cond=true,
                   switch_range_to_icmp=true,
                   switch_to_lookup=true)
            aggressiveSimplifyCFGOptions =
                (forward_switch_cond=true,
                   switch_range_to_icmp=true,
                   switch_to_lookup=true,
                   hoist_common_insts=true)
            add!(pb, NewPMModulePassManager()) do mpm
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    add!(fpm, GCInvariantVerifierPass())
                end
                add!(mpm, VerifierPass())
                add!(mpm, ForceFunctionAttrsPass())
                add!(mpm, Annotation2MetadataPass())
                add!(mpm, ConstantMergePass())
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    add!(fpm, LowerExpectIntrinsicPass())
                    add!(fpm, PropagateJuliaAddrspacesPass())
                    add!(fpm, SimplifyCFGPass(; basicSimplifyCFGOptions...))
                    add!(fpm, DCEPass())
                    add!(fpm, SROAPass())
                end
                add!(mpm, AlwaysInlinerPass())
                add!(mpm, NewPMCGSCCPassManager()) do cgpm
                    add!(cgpm, NewPMFunctionPassManager()) do fpm
                        add!(fpm, AllocOptPass())
                        add!(fpm, Float2IntPass())
                        add!(fpm, LowerConstantIntrinsicsPass())
                    end
                end
                add!(mpm, CPUFeaturesPass())
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    add!(fpm, SROAPass())
                    add!(fpm, InstCombinePass())
                    add!(fpm, JumpThreadingPass())
                    add!(fpm, CorrelatedValuePropagationPass())
                    add!(fpm, ReassociatePass())
                    add!(fpm, EarlyCSEPass())
                    add!(fpm, AllocOptPass())
                end
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    add!(fpm, NewPMLoopPassManager()) do lpm
                        add!(lpm, LowerSIMDLoopPass())
                        add!(lpm, LoopRotatePass())
                    end
                    add!(fpm, NewPMLoopPassManager(use_memory_ssa=true)) do lpm
                        add!(lpm, LICMPass())
                        add!(lpm, JuliaLICMPass())
                        add!(lpm, SimpleLoopUnswitchPass())
                        add!(lpm, LICMPass())
                        add!(lpm, JuliaLICMPass())
                    end
                    add!(fpm, IRCEPass())
                    add!(fpm, NewPMLoopPassManager()) do lpm
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
                    add!(fpm, NewPMLoopPassManager()) do lpm
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
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    if VERSION < v"1.13.0-DEV.36"
                        add!(fpm, LowerExcHandlersPass())
                    end
                    add!(fpm, GCInvariantVerifierPass())
                end
                add!(mpm, RemoveNIPass())
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    add!(fpm, LateLowerGCPass())
                    if VERSION >= v"1.11.0-DEV.208"
                        add!(fpm, FinalLowerGCPass())
                    end
                end
                if VERSION < v"1.11.0-DEV.208"
                    add!(mpm, FinalLowerGCPass())
                end
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    add!(fpm, GVNPass())
                    add!(fpm, SCCPPass())
                    add!(fpm, DCEPass())
                end
                add!(mpm, LowerPTLSPass())
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    add!(fpm, InstCombinePass())
                    add!(fpm, SimplifyCFGPass(; aggressiveSimplifyCFGOptions...))
                end
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    if VERSION < v"1.12.0-DEV.1390"
                        add!(fpm, CombineMulAddPass())
                    end
                    add!(fpm, DivRemPairsPass())
                end
                add!(mpm, NewPMFunctionPassManager()) do fpm
                    add!(fpm, AnnotationRemarksPass())
                end
                add!(mpm, NewPMFunctionPassManager()) do fpm
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

@testset "PassInstrumentation" begin
    # Verify that JULIA_LLVM_ARGS flags are propagated to the NewPM pipeline.
    # We run a script in a subprocess so that clopts() doesn't mutate
    # global LLVM state for the rest of the test suite.
    script = """
        using LLVM
        @dispose ctx=Context() begin
            mod = LLVM.Module("test")
            ft = LLVM.FunctionType(LLVM.VoidType())
            fn = LLVM.Function(mod, "SomeFunction", ft)

            @dispose builder=IRBuilder() begin
                entry = BasicBlock(fn, "entry")
                position!(builder, entry)
                ret!(builder)
            end

            dead_fn = LLVM.Function(mod, "dead_func", ft)
            linkage!(dead_fn, LLVM.API.LLVMInternalLinkage)
            @dispose builder=IRBuilder() begin
                entry = BasicBlock(dead_fn, "entry")
                position!(builder, entry)
                ret!(builder)
            end

            @dispose pb=NewPMPassBuilder() begin
                add!(pb, NewPMModulePassManager()) do mpm
                    add!(mpm, GlobalDCEPass())
                    add!(mpm, NewPMFunctionPassManager()) do fpm
                        add!(fpm, NoOpFunctionPass())
                    end
                end
                run!(pb, mod)
            end
        end
    """
    mktemp() do path, io
        write(io, script)
        flush(io)

        cmd = `$(Base.julia_cmd()) --project=$(Base.active_project()) $path`

        # Without flags: no IR dump output
        out = IOBuffer(); err = IOBuffer()
        @test success(pipeline(cmd, stdout=out, stderr=err))
        @test !occursin("IR Dump", String(take!(err)))

        # `JULIA_LLVM_ARGS="--print-after-all --filter-print-funcs=SomeFunction"`
        cmd_print = addenv(cmd, "JULIA_LLVM_ARGS" =>
            "--print-after-all --filter-print-funcs=SomeFunction")
        out = IOBuffer(); err = IOBuffer()
        @test success(pipeline(cmd_print, stdout=out, stderr=err))
        output = String(take!(err))
        @test occursin("IR Dump After NoOpFunctionPass on SomeFunction", output)

        # JULIA_LLVM_ARGS="--print-changed=diff-quiet --filter-passes=globaldce  --filter-print-funcs=dead_func"`
        cmd_changed = addenv(cmd, "JULIA_LLVM_ARGS" =>
            "--print-changed=diff-quiet --filter-passes=globaldce --filter-print-funcs=dead_func")
        out = IOBuffer(); err = IOBuffer()
        @test success(pipeline(cmd_changed, stdout=out, stderr=err))
        output = String(take!(err))
        @test occursin("IR Dump After GlobalDCEPass on [module]", output)
        @test occursin("-  ret void", output)
    end

    # Custom Julia function passes are registered under a user-chosen name and
    # are visible to pass instrumentation just like built-in passes.
    custom_script = """
        using LLVM
        @dispose ctx=Context() begin
            mod = LLVM.Module("test")
            ft = LLVM.FunctionType(LLVM.VoidType())
            fn = LLVM.Function(mod, "SomeFunction", ft)

            @dispose builder=IRBuilder() begin
                entry = BasicBlock(fn, "entry")
                position!(builder, entry)
                ret!(builder)
            end

            function my_pass!(f::LLVM.Function)
                return false  # no changes
            end
            MyPass() = NewPMFunctionPass("my-pass", my_pass!)

            @dispose pb=NewPMPassBuilder() begin
                register!(pb, MyPass())
                add!(pb, NewPMModulePassManager()) do mpm
                    add!(mpm, NewPMFunctionPassManager()) do fpm
                        add!(fpm, MyPass())
                    end
                end
                run!(pb, mod)
            end
        end
    """
    mktemp() do path, io
        write(io, custom_script)
        flush(io)

        cmd = `$(Base.julia_cmd()) --project=$(Base.active_project()) $path`

        # `JULIA_LLVM_ARGS="--print-after-all --filter-print-funcs=SomeFunction`
        cmd_custom = addenv(cmd, "JULIA_LLVM_ARGS" =>
            "--print-after-all --filter-print-funcs=SomeFunction")
        out = IOBuffer(); err = IOBuffer()
        @test success(pipeline(cmd_custom, stdout=out, stderr=err))
        output = String(take!(err))
        @test occursin("IR Dump After JuliaCustomFunctionPass on SomeFunction", output)
    end
end

@testset "alias analyses" begin
    # default pipeline
    @dispose ctx=Context() mod=test_module() pb=NewPMPassBuilder(debug_logging=true) begin
        add!(pb, "aa-eval")

        io = IOCapture.capture() do
            run!(pb, mod)
        end

        @test contains(io.output, "Running analysis: BasicAA")
        @test contains(io.output, "Running analysis: TypeBasedAA")
        @test contains(io.output, "Running analysis: ScopedNoAliasAA")
    end

    # custom pipeline
    @dispose ctx=Context() mod=test_module() pb=NewPMPassBuilder(debug_logging=true) begin
        add!(pb, NewPMAAManager()) do aam
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
            tm = LLVM.TargetMachine(t, triple, "sm_80")
            try
                @dispose pb=NewPMPassBuilder(debug_logging=true) mod=test_module() begin
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
