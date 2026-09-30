## target

@public Target,
        hasjit, hastargetmachine, hasasmbackend

"""
    Target

A structure exposing target-specific information.

# Properties

    target.name

The name of the target, e.g., `"x86-64"`.

    target.description

A short description of the target.
"""
@checked struct Target
    ref::API.LLVMTargetRef
end
@properties Target

Base.unsafe_convert(::Type{API.LLVMTargetRef}, target::Target) = target.ref

"""
    Target(; name=nothing, triple=nothing)

Create a target from its name or triple, either of which must be specified.
"""
function Target(; name=nothing, triple=nothing)
    (name !== nothing) ⊻ (triple !== nothing) ||
        throw(ArgumentError("Specify either name or triple."))

    if triple !== nothing
        target_ref = Ref{API.LLVMTargetRef}(0)
        error_ref = Ref{Cstring}(C_NULL)
        status = API.LLVMGetTargetFromTriple(triple, target_ref, error_ref) |> Bool
        if status && error_ref[] !== C_NULL
            error = unsafe_message(error_ref[])
            throw(ArgumentError(error))
        elseif status
            throw(ArgumentError("Cannot find a target for triple '$triple'"))
        end
        @assert target_ref[] != C_NULL
        return Target(target_ref[])
    elseif name !== nothing
        target = API.LLVMGetTargetFromName(name)
        if target == C_NULL
            throw(ArgumentError("Cannot find target '$name'"))
        end
        return Target(target)
    end
end

name(t::Target) = unsafe_string(API.LLVMGetTargetName(t))

description(t::Target) = unsafe_string(API.LLVMGetTargetDescription(t))

@property Target name
@property Target description

"""
    hasjit(target::Target)

Check if this targets supports the just-in-time compilation.
"""
hasjit(t::Target) = API.LLVMTargetHasJIT(t) |> Bool

"""
    hastargetmachine(target::Target)

Check if this target supports code generation.
"""
hastargetmachine(t::Target) = API.LLVMTargetHasTargetMachine(t) |> Bool

"""
    hasasmbackend(target::Target)

Check if this target has an assembly backend, which emits object files.
"""
hasasmbackend(t::Target) = API.LLVMTargetHasAsmBackend(t) |> Bool

function Base.show(io::IO, ::MIME"text/plain", target::Target)
  print(io, "LLVM.Target($(name(target))): $(description(target))")
end


## target iteration

@public targets

struct TargetIterator end

"""
    targets()

Get an iterator over the available targets.
"""
targets() = TargetIterator()

Base.eltype(::Type{TargetIterator}) = Target

function Base.iterate(iter::TargetIterator, state=API.LLVMGetFirstTarget())
    state == C_NULL ? nothing : (Target(state), API.LLVMGetNextTarget(state))
end

Base.IteratorSize(::Type{TargetIterator}) = Base.SizeUnknown()
