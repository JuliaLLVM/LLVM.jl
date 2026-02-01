# helpers for wrapping the library

function unsafe_message(ptr, args...)
    str = unsafe_string(ptr, args...)
    API.LLVMDisposeMessage(ptr)
    str
end


## defining types in the LLVM type hierarchy

# llvm.org/docs/doxygen/html/group__LLVMCSupportTypes.html

# macro that adds an inner constructor to a type definition,
# calling `refcheck` on the ref field argument
macro checked(typedef)
    # decode structure definition
    if Meta.isexpr(typedef, :struct)
        structure = typedef.args[2]
        body = typedef.args[3]
    else
        error("argument is not a structure definition")
    end
    if isa(structure, Symbol)
        # basic type definition
        typename = structure
    elseif Meta.isexpr(structure, :<:)
        # typename <: parentname
        all(e->isa(e,Symbol), structure.args) ||
            error("typedef should consist of plain types, ie. not parametric ones")
        typename = structure.args[1]
    else
        error("malformed type definition: cannot decode type name")
    end

    # decode fields
    field_names = Symbol[]
    field_defs = Union{Symbol,Expr}[]
    for arg in body.args
        if isa(arg, LineNumberNode)
            continue
        elseif isa(arg, Symbol)
            push!(field_names, arg)
            push!(field_defs, arg)
        elseif Meta.isexpr(arg, :(::))
            push!(field_names, arg.args[1])
            push!(field_defs, arg)
        end
    end
    :ref in field_names || error("structure definition should contain 'ref' field")

    # insert checked constructor
    push!(body.args, :(
        $typename($(field_defs...)) = (refcheck($typename, ref); new($(field_names...)))
    ))

    return esc(typedef)
end

# the most basic check is asserting that we don't use a null pointer
@inline function refcheck(::Type, ref::Ptr)
    ref==C_NULL && throw(UndefRefError())
end


## helper macro for disposing resources without do-block syntax

export @dispose

"""
    @dispose foo=Foo() bar=Bar() begin
        ...
    end

Helper macro for disposing resources (by calling the `dispose` function for every resource
in reverse order) after executing a block of code. This is often equivalent to calling the
recourse constructor with do-block syntax, but without using (potentially costly) closures.
"""
macro dispose(ex...)
    resources = ex[1:end-1]
    code = ex[end]

    Meta.isexpr(code, :block) ||
        error("Expected a code block as final argument to LLVM.@dispose")

    cleanup = quote
    end
    for res in reverse(resources)
        Meta.isexpr(res, :(=)) ||
            error("Resource arguments to LLVM.@dispose should be assignments")
        push!(cleanup.args, :($dispose($(res.args[1]))))
    end

    ex = quote
        let $(resources...)
            try
                $code
            finally
                $(cleanup.args...)
            end
        end
    end
    esc(ex)
end
