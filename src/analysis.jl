## module and function verification

@vocabulary IR verify, verification_error

"""
    verify(mod::Module)
    verify(f::Function)

Verify the module or function `mod` or `f`. If verification fails, an `LLVMException` is
thrown with the verifier's message. See [`verification_error`](@ref) for a variant that
does not throw.
"""
function verify(x::Union{Module, Function})
    msg = verification_error(x)
    msg === nothing || throw(LLVMException(msg))
    return
end

"""
    verification_error(mod::Module)
    verification_error(f::Function)

Verify the module or function `mod` or `f`, returning the verifier's message if it is
broken, or `nothing` if it is valid. This is useful to report errors with more context:

```julia
msg = verification_error(f)
msg === nothing || error("Generated invalid code for \$name:\n\$msg\n\$(string(f))")
```
"""
function verification_error(mod::Module)
    out_error = Ref{Cstring}()
    status = API.LLVMVerifyModule(mod, API.LLVMReturnStatusAction, out_error) |> Bool
    msg = unsafe_message(out_error[])
    return status ? msg : nothing
end

function verification_error(f::Function)
    out_error = Ref{Cstring}()
    status = API.LLVMExtraVerifyFunction(f, out_error) |> Bool
    return status ? unsafe_message(out_error[]) : nothing
end


## dominator analysis

@vocabulary IR dominates

"""
    dominates(tree::DomTree, A::Instruction, B::Instruction)
    dominates(tree::PostDomTree, A::Instruction, B::Instruction)

Check if instruction `A` dominates instruction `B` in the dominator tree `tree`.
"""
dominates(tree, A::Instruction, B::Instruction)

# dominance

@vocabulary IR DomTree

"""
    DomTree

Dominator tree for a function.
"""
@checked struct DomTree
    ref::API.LLVMDominatorTreeRef
end

Base.unsafe_convert(::Type{API.LLVMDominatorTreeRef}, domtree::DomTree) =
    mark_use(domtree).ref

"""
    DomTree(f::Function)

Create a dominator tree for the function `f`.

This object needs to be disposed of using [`dispose`](@ref).
"""
DomTree(f::Function) = mark_alloc(DomTree(API.LLVMCreateDominatorTree(f)))

"""
    dispose(::DomTree)

Dispose of a dominator tree.
"""
dispose(domtree::DomTree) = mark_dispose(API.LLVMDisposeDominatorTree, domtree)

function dominates(domtree::DomTree, A::Instruction, B::Instruction)
    API.LLVMDominatorTreeInstructionDominates(domtree, A, B) |> Bool
end


## post-dominance

@vocabulary IR PostDomTree

"""
    PostDomTree

Post-dominator tree for a function.
"""
@checked struct PostDomTree
    ref::API.LLVMPostDominatorTreeRef
end

Base.unsafe_convert(::Type{API.LLVMPostDominatorTreeRef}, postdomtree::PostDomTree) =
    mark_use(postdomtree).ref

"""
    PostDomTree(f::Function)

Create a post-dominator tree for the function `f`.

This object needs to be disposed of using [`dispose`](@ref).
"""
PostDomTree(f::Function) = mark_alloc(PostDomTree(API.LLVMCreatePostDominatorTree(f)))

"""
    dispose(tree::PostDomTree)

Dispose of a post-dominator tree.
"""
dispose(postdomtree::PostDomTree) =
    mark_dispose(API.LLVMDisposePostDominatorTree, postdomtree)

function dominates(postdomtree::PostDomTree, A::Instruction, B::Instruction)
    API.LLVMPostDominatorTreeInstructionDominates(postdomtree, A, B) |> Bool
end
