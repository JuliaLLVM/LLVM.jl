export tbaa_make_child, tbaa_addrspace

"""
    tbaa_make_child(name::String; constant::Bool=false) -> MDNode

Create a TBAA access tag (for `!tbaa` metadata) for accesses of the type `name`, a child of
a custom TBAA root, which doesn't alias with the other types that this function creates.
With `constant=true`, the tag marks the memory as constant.
"""
function tbaa_make_child(name::String; constant::Bool=false)
    tbaa_root = MDNode([MDString("custom_tbaa")])
    tbaa_struct_type =
        MDNode([MDString("custom_tbaa_$name"),
                tbaa_root,
                ConstantInt(0)])
    tbaa_access_tag =
        MDNode([tbaa_struct_type,
                tbaa_struct_type,
                ConstantInt(0),
                ConstantInt(constant ? 1 : 0)])

    return tbaa_access_tag
end

"""
    tbaa_addrspace(as) -> MDNode

Create a TBAA access tag for accesses of memory in address space `as`, so that accesses of
different address spaces don't alias. See [`tbaa_make_child`](@ref).
"""
tbaa_addrspace(as) = tbaa_make_child("addrspace($(as))")
