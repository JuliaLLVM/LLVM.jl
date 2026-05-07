# deprecated methods

# lookup(jljit, name) without an explicit JD was removed in Julia 1.13 (JuliaLang/julia#60988).
# On older Julia the JD is ignored internally; on 1.13+ users must create a JD explicitly.
@static if VERSION < v"1.13.0-DEV.0"
    @deprecate(lookup(jljit::JuliaOJIT, name, external_jd_only=false),
               lookup(jljit, JITDylib(jljit), name, external_jd_only), false)
end

@deprecate called_value(inst::CallBase) called_operand(inst)

@deprecate has_orc_v1() false false
@deprecate has_orc_v2() true false
@deprecate has_newpm() true false
@deprecate has_julia_ojit() true false

Base.@deprecate_binding ValueMetadataDict LLVM.InstructionMetadataDict

@deprecate(fence!(builder::IRBuilder, ordering::API.LLVMAtomicOrdering, syncscope::String,
                  Name::String=""),
           fence!(builder, ordering, SyncScope(syncscope), Name), false)

@deprecate(atomic_rmw!(builder::IRBuilder, op::API.LLVMAtomicRMWBinOp, Ptr::Value,
                       Val::Value, ordering::API.LLVMAtomicOrdering, syncscope::String),
           atomic_rmw!(builder, op, Ptr, Val, ordering, SyncScope(syncscope)), false)

@deprecate(atomic_cmpxchg!(builder::IRBuilder, Ptr::Value, Cmp::Value, New::Value,
                           SuccessOrdering::API.LLVMAtomicOrdering,
                           FailureOrdering::API.LLVMAtomicOrdering, syncscope::String),
           atomic_cmpxchg!(builder, Ptr, Cmp, New, SuccessOrdering, FailureOrdering,
                           SyncScope(syncscope)), false)

@deprecate Base.size(vectyp::VectorType) length(vectyp) false

@deprecate Module(mod::Module) copy(mod) false
@deprecate Instruction(inst::Instruction) copy(inst) false

@deprecate Base.delete!(::Function, bb::BasicBlock) remove!(bb) false
@deprecate Base.delete!(::BasicBlock, inst::Instruction) remove!(inst) false

@deprecate unsafe_delete!(::Module, gv::GlobalVariable) erase!(gv)
@deprecate unsafe_delete!(::Module, f::Function) erase!(f)
@deprecate unsafe_delete!(::Function, bb::BasicBlock) erase!(bb)
@deprecate unsafe_delete!(::BasicBlock, inst::Instruction) erase!(inst)

@deprecate predicate_int(inst) predicate(inst)
@deprecate predicate_real(inst) predicate(inst)

@deprecate Base.string(md::MDString) convert(String, md) false
function Base.show(io::IO, ::MIME"text/plain", md::MDString)
    str = @invoke string(md::Metadata)
    print(io, strip(str))
end

@deprecate get_subprogram(func::Function) subprogram(func) false
@deprecate set_subprogram!(func::Function, sp::DISubProgram) subprogram!(func, sp) false
