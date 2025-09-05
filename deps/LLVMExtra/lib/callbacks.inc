#ifdef FUNCTION_CALLBACK
FUNCTION_CALLBACK("PeepholeCallbacks", invokePeepholeEPCallbacks)
FUNCTION_CALLBACK("ScalarOptimizerLateCallbacks", invokeScalarOptimizerLateEPCallbacks)
FUNCTION_CALLBACK("VectorizerStartCallbacks", invokeVectorizerStartEPCallbacks)
#endif

#ifdef LOOP_CALLBACK
LOOP_CALLBACK("LateLoopOptimizationsCallbacks", invokeLateLoopOptimizationsEPCallbacks)
LOOP_CALLBACK("LoopOptimizerEndCallbacks", invokeLoopOptimizerEndEPCallbacks)
#endif

#ifdef MODULE_CALLBACK
MODULE_CALLBACK("PipelineStartCallbacks", invokePipelineStartEPCallbacks)
#endif

// There are some full lto specific ones that are ignored here
#ifdef MODULE_LTO_CALLBACK
MODULE_LTO_CALLBACK("PipelineEarlySimplificationCallbacks", invokePipelineEarlySimplificationEPCallbacks)
MODULE_LTO_CALLBACK("OptimizerEarlyCallbacks", invokeOptimizerEarlyEPCallbacks)
MODULE_LTO_CALLBACK("OptimizerLastCallbacks", invokeOptimizerLastEPCallbacks)
#endif

#ifdef CGSCC_CALLBACK
CGSCC_CALLBACK("CGSCCOptimizerLateCallbacks", invokeCGSCCOptimizerLateEPCallbacks)
#endif