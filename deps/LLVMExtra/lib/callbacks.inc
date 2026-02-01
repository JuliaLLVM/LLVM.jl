#ifdef MODULE_CALLBACK
MODULE_CALLBACK("pipeline-start-callbacks", invokePipelineStartEPCallbacks)
#endif
#undef MODULE_CALLBACK

// There are some full lto specific ones that are ignored here for now
#ifdef MODULE_LTO_CALLBACK
MODULE_LTO_CALLBACK("pipeline-early-simplification-callbacks", invokePipelineEarlySimplificationEPCallbacks)
MODULE_LTO_CALLBACK("optimizer-early-callbacks", invokeOptimizerEarlyEPCallbacks)
MODULE_LTO_CALLBACK("optimizer-last-callbacks", invokeOptimizerLastEPCallbacks)
#endif
#undef MODULE_LTO_CALLBACK

#ifdef FUNCTION_CALLBACK
FUNCTION_CALLBACK("peephole-callbacks", invokePeepholeEPCallbacks)
FUNCTION_CALLBACK("scalar-optimizer-late-callbacks", invokeScalarOptimizerLateEPCallbacks)
FUNCTION_CALLBACK("vectorizer-start-callbacks", invokeVectorizerStartEPCallbacks)
#if LLVM_VERSION_MAJOR >= 21
FUNCTION_CALLBACK("vectorizer-end-callbacks", invokeVectorizerEndEPCallbacks)
#endif
#endif
#undef FUNCTION_CALLBACK

#ifdef CGSCC_CALLBACK
CGSCC_CALLBACK("cgscc-optimizer-late-callbacks", invokeCGSCCOptimizerLateEPCallbacks)
#endif
#undef CGSCC_CALLBACK

#ifdef LOOP_CALLBACK
LOOP_CALLBACK("late-loop-optimizations-callbacks", invokeLateLoopOptimizationsEPCallbacks)
LOOP_CALLBACK("loop-optimizer-end-callbacks", invokeLoopOptimizerEndEPCallbacks)
#endif
#undef LOOP_CALLBACK