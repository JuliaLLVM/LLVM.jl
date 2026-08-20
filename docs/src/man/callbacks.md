# Callback exception safety

Julia exceptions use `setjmp`/`longjmp`. They must not escape a callback into
LLVM: doing so skips C++ destructors and can leave locks held, registrations
active, IR partly mutated, or process-global pointers referring to dead stack
storage.

The invariant for every high-level callback is therefore:

1. catch before returning through `@cfunction`;
2. preserve the exception and Julia backtrace in rooted state;
3. return a valid, conservative result through the foreign callback contract;
4. let LLVM finish its normal cleanup; and
5. throw only after control is back in a Julia-owned frame.

Raw function pointers passed through `LLVM.API` are outside LLVM.jl's control.
They have a strict no-throw contract unless their implementation supplies an
equivalent barrier.

## High-level inventory

The table records every callback constructed by LLVM.jl. “Retained” means LLVM
may invoke the callback after the API that registered it has returned.

| Julia boundary | Foreign caller and lifetime | Failure contract | Classification |
|---|---|---|---|
| `NewPMModulePass`, `NewPMFunctionPass` | LLVMExtra `JuliaCustom*Pass::run`; synchronous on the pass-run thread. Pass managers, analysis managers, instrumentation and partly-mutated IR are live. | The trampoline captures the first exception, reports “changed” to invalidate analyses, and `run!` throws `PassException` after `LLVMRunJuliaPasses` returns. | Safe exception barrier. |
| `AbstractTargetTransformInfo` query callbacks | LLVMExtra's `JuliaTTIImpl`; synchronous from NewPM analyses/transforms. | Predicates and query sentinels use conservative results; address spaces return “unknown”, aliases return “may alias”, divergence returns true, uniformity/no-op/validity return false, and rewrite/materialization queries return no result. `run!` then throws `PassException`. | Safe exception barrier. |
| `ModulePass`, `FunctionPass` | LLVMExtra `JuliaModulePass::runOnModule` / `JuliaFunctionPass::runOnFunction`, below `legacy::PassManager::run`; synchronous. Legacy analysis state and partly-mutated IR are live. | Persistent per-pass state captures the exception and reports “changed”. `run!` checks all rooted passes only after `LLVMRunPassManager` or `LLVMRunFunctionPassManager` returns. State is reset before the next run. | Safe exception barrier. |
| `clone_into!` `type_mapper` | LLVMExtra `ExternalTypeRemapper::remapType`, called from `CloneFunctionInto`; synchronous. `ValueToValueMapTy`, remappers, cloned returns and a partly-mutated destination function are live. | `Type *` has no failure sentinel. Identity is not always a safe fallback after earlier mappings changed types. | Needs API redesign; callbacks must not throw. |
| `clone_into!` `materializer` | LLVMExtra `ExternalValueMaterializer::materialize`, called from `ValueMapper`; synchronous during cloning. | Null means “not materialized”, not “abort cloning”; cloning continues and may already have modified the destination. | Needs API redesign; callbacks must not throw. |
| `ObjectLinkingLayerCreator` | LLVM C API's LLJIT builder closure; synchronous during `LLVMOrcCreateLLJIT`. LLJIT builder state and partially-constructed ORC components are live. | The C callback cannot return an error. On failure LLVM.jl supplies LLVM's valid default RTDyld layer, lets construction finish, disposes the temporary LLJIT, then throws `CallbackException`. The builder roots the creator. | Safe exception barrier. |
| `ThreadSafeModule(f)` | `LLVMOrcThreadSafeModuleWithModuleDo`; synchronous while the thread-safe module/context lock is held. | The callback returns `LLVMErrorRef`; errors are converted to a string error and `@check` throws after the lock is released. | Safe exception barrier. |
| `CustomMaterializationUnit.materialize` | Retained ORC callback, possibly on an ORC worker thread. Ownership of the materialization responsibility passes to the callback. | On exception it calls `LLVMOrcMaterializationResponsibilityFailMaterialization` and stores the original exception on the rooted unit. `check_callback_error` is the Julia-owned error channel. | Safe retained barrier with explicit error channel. |
| `CustomMaterializationUnit.discard` | Retained ORC callback; `void`, with no failure protocol. | The first exception is stored on the unit and can be surfaced with `check_callback_error`. | Safe retained barrier; recovery belongs to the caller. |
| `CustomMaterializationUnit` destroy trampoline | Retained ORC destruction callback. It only removes an internal GC root and invokes no user code. | No user failure path. | Safe by construction. |
| Context diagnostic handler | `LLVMContext::diagnose`; synchronous on whichever thread emits the diagnostic. The surrounding operation is otherwise unconstrained. | The `void` callback captures the first error in context-owned state and never throws, including if logging a warning fails. Bitcode parsing, linking and pass runners check that state after LLVM returns. | Safe barrier for integrated wrappers; raw APIs must inspect their own result contract. |
| Context yield callback | LLVM context yield hook. | Disabled: switching Julia tasks out of an active `ccall` has no established safety contract. | Safe by construction while disabled. |
| Fatal-error handling | `report_fatal_error` invokes a handler only before running interrupt handlers and unconditionally aborting or exiting. | LLVM.jl does not install a Julia callback. Fatal errors retain LLVM's default reporting and termination behavior. | Unrecoverable by contract. |
| `noop_register_frame` | Julia's frame-registration hook; synchronous and contains only a no-op. | Cannot execute user code or throw. | Safe by construction. |
| `jl_register_passbuilder_callbacks` | Native Julia runtime registration function obtained with `cglobal`; synchronous while configuring a pass builder. | This is not a Julia-language callback and does not enter arbitrary user code. | Safe by construction. |

The low-level overload `linkinglayercreator!(builder, callback, ctx)` and direct
`LLVMPassBuilderExtensionsRegister*Pass` entry points accept raw pointers. They
are escape hatches, not exception barriers.

## Raw LLVM C callback families

The generated bindings expose these families on every supported LLVM version
unless noted. LLVM.jl cannot validate an unknown pointer supplied by a caller.

| Raw family | Lifetime / thread | Foreign error channel | Required handling |
|---|---|---|---|
| Context diagnostic and yield handlers | Retained by `LLVMContext`; emitter thread | `void` | Never throw. Use context-owned state and an owning wrapper. |
| Fatal-error handler | Process-global; failing thread | None; LLVM terminates after it returns | Do not attempt recovery. |
| Disassembler operand-info and symbol-lookup callbacks | Retained by the disassembler; synchronous during disassembly | Integer/null sentinel | Catch and return “no symbolic information”. Keep returned strings rooted for the documented lifetime. |
| MCJIT allocate-code, allocate-data, finalize and destroy callbacks | Retained by the memory manager; compilation/destruction thread | Null for allocation, boolean plus error message for finalize, none for destroy | Needs a rooted memory-manager wrapper; destruction callbacks cannot report failure. |
| ORC execution-session error reporter | Retained; often an ORC worker thread | The callback owns an `LLVMErrorRef`, but returns `void` | Consume the error and store it in execution-session-owned state. |
| ORC materialize, discard and destroy callbacks | Retained; potentially asynchronous | Explicit fail-materialization only for materialize | Use the high-level materialization unit or an equivalent owned channel. |
| ORC C-API definition generator and disposer | Retained; lookup/worker thread | `LLVMErrorRef` from generate, none from dispose | Generate callbacks can translate exceptions to `LLVMErrorRef`; disposer callbacks must not fail. Suspended lookup state must always be resumed. |
| ORC generic module operation | Synchronous under the thread-safe-module lock | `LLVMErrorRef` | Use `ThreadSafeModule(f)` or return an LLVM error without throwing. |
| ORC IR and object transform callbacks | Retained; materialization thread | `LLVMErrorRef`; ownership is transferred through in/out pointers | Capture before returning, satisfy the documented input ownership on both success and failure, and return an LLVM error. |
| ORC execution-session lookup completion | Retained/asynchronous | Receives an owned `LLVMErrorRef`; returns `void` | Consume the error and signal an owned Julia state/channel. It cannot rethrow to the task that initiated lookup. |
| ORC symbol predicates | Synchronous from generator lookup | Boolean | Return a conservative false result after capture. |
| JITLink memory-manager create-context and notify-terminating callbacks | LLVM 16 and newer; retained by the RTDyld compatibility layer | Null for creation, none for termination | Needs an owned manager wrapper; termination cannot report failure. |
| LLVMExtra legacy/NewPM pass and TTI callbacks | Synchronous pass thread | Boolean/query-specific sentinel | Prefer the high-level barriers above. Raw pass callbacks must not throw. |
| LLVMExtra clone type-remapper and value-materializer callbacks | Synchronous clone thread | No abort/error result | No recoverable Julia exception contract exists today. |

The LLVM 15–22 C declarations for these families are stable except for the two
JITLink memory-manager callbacks added in LLVM 16. Internal ORC builder closure
signatures changed across releases, but the C object-layer-creator callback
remained a synchronous, non-error-returning function. The legacy pass-manager C
entry points and `LLVMContext::diagnose` shape are also unchanged across the
supported range.

## Prioritized remaining work

1. `clone_into!` is the only high-level synchronous API that still accepts
   throwing Julia code without a defensible fallback. A repair needs a new
   LLVMExtra error-returning or transactional clone operation; merely catching
   and returning the source type/null can leave inconsistent mapped types or a
   partially-mutated destination.
2. Raw ORC definition-generator, transform, lookup, error-reporter and memory-
   manager APIs need high-level wrappers before Julia callables can be safely
   supported. Their existing raw-pointer forms remain available with a no-throw
   contract.
3. Materialization/discard errors are owned by their retained unit. Callers
   using asynchronous ORC work should arrange to poll or forward
   `check_callback_error` at an application-defined safe point.
4. The NewPM heap-lifetime mitigation protects known `StandardInstrumentations`
   global pointers after a foreign raw callback violates the contract. It does
   not make the rest of a pass run recoverable after `longjmp`.

## Adding a callback wrapper

Before accepting a Julia callable, inspect the C++ caller on every materially
different supported LLVM version. Record whether the callback is retained, its
thread, live locks/RAII objects, mutation and ownership already in progress,
and the exact meaning of every return sentinel. A regression must throw after
some useful work, verify normal cleanup, preserve the exception type/message/
backtrace, and exercise a second operation.

Relevant source entry points:

- [LLVMExtra legacy pass and clone adapters](https://github.com/JuliaLLVM/LLVM.jl/blob/main/deps/LLVMExtra/lib/Core.cpp)
- [LLVMExtra NewPM and TTI adapters](https://github.com/JuliaLLVM/LLVM.jl/blob/main/deps/LLVMExtra/lib/NewPM.cpp)
- [LLVM 18 ORC C bindings](https://github.com/llvm/llvm-project/blob/llvmorg-18.1.8/llvm/lib/ExecutionEngine/Orc/OrcV2CBindings.cpp)
- [LLVM 22 ORC C bindings](https://github.com/llvm/llvm-project/blob/llvmorg-22.1.0/llvm/lib/ExecutionEngine/Orc/OrcV2CBindings.cpp)
- [LLVM context diagnostics](https://github.com/llvm/llvm-project/blob/llvmorg-22.1.0/llvm/lib/IR/LLVMContext.cpp)
- [LLVM fatal-error contract](https://github.com/llvm/llvm-project/blob/llvmorg-22.1.0/llvm/lib/Support/ErrorHandling.cpp)
- [LLVM value mapping](https://github.com/llvm/llvm-project/blob/llvmorg-22.1.0/llvm/lib/Transforms/Utils/ValueMapper.cpp)
- [LLVM function cloning](https://github.com/llvm/llvm-project/blob/llvmorg-22.1.0/llvm/lib/Transforms/Utils/CloneFunction.cpp)
