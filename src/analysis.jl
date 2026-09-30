## module and function verification

@vocabulary IR verify

"""
    verify(mod::Module)
    verify(f::Function)

Verify the module or function `mod` or `f`. If verification fails, an exception is thrown.
"""
verify(::Union{Module, Function})

function verify(mod::Module)
    out_error = Ref{Cstring}()
    status = API.LLVMVerifyModule(mod, API.LLVMReturnStatusAction, out_error) |> Bool

    if status
        error = unsafe_message(out_error[])
        throw(LLVMException(error))
    end
end

function verify(f::Function)
    status = API.LLVMVerifyFunction(f, API.LLVMReturnStatusAction) |> Bool

    if status
        throw(LLVMException("broken function"))
    end
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
