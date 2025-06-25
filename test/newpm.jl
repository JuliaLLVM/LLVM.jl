
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

end
