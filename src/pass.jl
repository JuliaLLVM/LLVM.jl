export Pass

# subtypes are expected to have a 'ref::API.LLVMPassRef' field
abstract type Pass end

Base.unsafe_convert(::Type{API.LLVMPassRef}, pass::Pass) = pass.ref

mutable struct LegacyPassState
    callback::Core.Function
    exception::Union{Nothing,Tuple{Any,Vector}}
    LegacyPassState(callback) = new(callback, nothing)
end

function legacy_pass_callback(state::LegacyPassState, value)
    # A function pass can be invoked multiple times during a single run. Once
    # one invocation has failed, avoid running user code again while LLVM
    # finishes unwinding its pass manager normally.
    state.exception === nothing || return true

    try
        return state.callback(value)::Bool
    catch err
        _capture_callback_exception!(state, err)
        # The callback may have changed IR before throwing. Returning true is
        # conservative: it invalidates analyses before the deferred rethrow.
        return true
    end
end


#
# Module passes
#

export ModulePass

function module_pass_callback(ptr, data)
    mod = Module(convert(API.LLVMModuleRef, ptr))
    state = Base.unsafe_pointer_to_objref(data)::LegacyPassState
    changed = legacy_pass_callback(state, mod)
    convert(API.LLVMBool, changed)
end

@checked struct ModulePass <: Pass
    ref::API.LLVMPassRef
    root::Any

    function ModulePass(name::String, runner::Core.Function)
        callback = @cfunction(module_pass_callback, API.LLVMBool, (Ptr{Cvoid}, Ptr{Cvoid}))
        state_box = Ref(LegacyPassState(runner))

        ref = API.LLVMCreateModulePass2(name, callback, state_box)
        refcheck(ModulePass, ref)

        return new(ref, state_box)
    end
end


#
# Function passes
#

export FunctionPass

function function_pass_callback(ptr, data)
    fn = Function(convert(API.LLVMValueRef, ptr))
    state = Base.unsafe_pointer_to_objref(data)::LegacyPassState
    changed = legacy_pass_callback(state, fn)
    convert(API.LLVMBool, changed)
end

@checked struct FunctionPass <: Pass
    ref::API.LLVMPassRef
    root::Any

    function FunctionPass(name::String, runner::Core.Function)
        callback = @cfunction(function_pass_callback, API.LLVMBool, (Ptr{Cvoid}, Ptr{Cvoid}))
        state_box = Ref(LegacyPassState(runner))

        ref = API.LLVMCreateFunctionPass2(name, callback, state_box)
        refcheck(FunctionPass, ref)

        return new(ref, state_box)
    end
end
