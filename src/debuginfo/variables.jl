## variables

@vocabulary IR DIVariable

"""
    DIVariable

Abstract supertype for all variable-like metadata nodes.

# Properties

    var.file

The file in which the variable is declared, or `nothing` if unknown.

    var.scope

The scope of the variable, or `nothing` if unknown. The scope of a local variable is a
[`DILocalScope`](@ref).

    var.line

The line number at which the variable is declared, or -1 if unknown.

The properties of [`DINode`](@ref LLVM.DINode) and [`MDNode`](@ref LLVM.MDNode) are
available too.
"""
abstract type DIVariable <: DINode end

for var in (:Local, :Global)
    var_name = Symbol("DI$(var)Variable")
    var_kind = Symbol("LLVM$(var_name)MetadataKind")
    @eval begin
        @checked struct $var_name <: DIVariable
            ref::API.LLVMMetadataRef
        end
        register($var_name, API.$var_kind)
    end
end

"""
    DILocalVariable <: DIVariable

A local variable in the source code.
"""
DILocalVariable

"""
    DIGlobalVariable <: DIVariable

A global variable in the source code.
"""
DIGlobalVariable

@vocabulary IR DILocalVariable, DIGlobalVariable

function file(var::DIVariable)
    ref = API.LLVMDIVariableGetFile(var)
    ref == C_NULL ? nothing : Metadata(ref)::DIFile
end

function scope(var::DIVariable)
    ref = API.LLVMDIVariableGetScope(var)
    ref == C_NULL ? nothing : Metadata(ref)::DIScope
end

function scope(var::DILocalVariable)
    ref = API.LLVMDIVariableGetScope(var)
    ref == C_NULL ? nothing : Metadata(ref)::DILocalScope
end

line(var::DIVariable) = line_number(API.LLVMDIVariableGetLine(var))

@property DIVariable file
@property DIVariable scope
@property DIVariable line


## variable factories

@vocabulary Build auto_variable!, parameter_variable!

"""
    auto_variable!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                  file::DIFile, line::Integer, type::DIType;
                  always_preserve::Bool=false, flags=API.LLVMDIFlagZero,
                  align_in_bits::Integer=0) -> DILocalVariable

Create a new local variable descriptor (for a compiler-introduced automatic
variable).
"""
function auto_variable!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                       file::DIFile, line::Integer, type::DIType;
                       always_preserve::Bool=false, flags=API.LLVMDIFlagZero,
                       align_in_bits::Integer=0)
    name = String(name)
    DILocalVariable(API.LLVMDIBuilderCreateAutoVariable(
        builder, scope, name, Csize_t(ncodeunits(name)),
        file, Cuint(line), type,
        always_preserve, flags, UInt32(align_in_bits)))
end

"""
    parameter_variable!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                       arg_no::Integer, file::DIFile, line::Integer, type::DIType;
                       always_preserve::Bool=false,
                       flags=API.LLVMDIFlagZero) -> DILocalVariable

Create a new descriptor for a function parameter variable. `arg_no` is
the 1-based parameter index.
"""
function parameter_variable!(builder::DIBuilder, scope::DILocalScope, name::AbstractString,
                            arg_no::Integer, file::DIFile, line::Integer, type::DIType;
                            always_preserve::Bool=false,
                            flags=API.LLVMDIFlagZero)
    1 <= arg_no <= typemax(Cuint) || throw(ArgumentError("parameter number must be positive and fit in Cuint"))
    name = String(name)
    DILocalVariable(API.LLVMDIBuilderCreateParameterVariable(
        builder, scope, name, Csize_t(ncodeunits(name)), Cuint(arg_no),
        file, Cuint(line), type,
        always_preserve, flags))
end


## expression

@vocabulary IR DIExpression, DIGlobalVariableExpression
@vocabulary Build expression!, constant_value_expression!

"""
    DIExpression

A DWARF expression that modifies how a variable's value is expressed at runtime.
"""
@checked struct DIExpression <: MDNode
    ref::API.LLVMMetadataRef
end
register(DIExpression, API.LLVMDIExpressionMetadataKind)

"""
    DIGlobalVariableExpression

A pairing of a [`DIGlobalVariable`](@ref) and its associated [`DIExpression`](@ref).

# Properties

    gve.variable

The global variable described by the global variable expression.

    gve.expression

The expression of the global variable expression, which describes the location of the
variable.

The properties of [`MDNode`](@ref LLVM.MDNode) are available too.
"""
@checked struct DIGlobalVariableExpression <: MDNode
    ref::API.LLVMMetadataRef
end
register(DIGlobalVariableExpression, API.LLVMDIGlobalVariableExpressionMetadataKind)

"""
    expression!(builder::DIBuilder,
                addr::AbstractVector{<:Integer}=UInt64[]) -> DIExpression

Create a new [`DIExpression`](@ref) from the given array of opcodes (encoding
a DWARF expression such as `DW_OP_plus_uconst`).
"""
function expression!(builder::DIBuilder, addr::AbstractVector{<:Integer}=UInt64[])
    DIExpression(API.LLVMDIBuilderCreateExpression(
        builder, Vector{UInt64}(addr), Csize_t(length(addr))))
end

"""
    constant_value_expression!(builder::DIBuilder, value::Integer) -> DIExpression

Create a new [`DIExpression`](@ref) representing a single constant value.
"""
function constant_value_expression!(builder::DIBuilder, value::Integer)
    DIExpression(API.LLVMDIBuilderCreateConstantValueExpression(
        builder, UInt64(value)))
end

function variable(gve::DIGlobalVariableExpression)
    ref = API.LLVMDIGlobalVariableExpressionGetVariable(gve)
    ref == C_NULL ? nothing : Metadata(ref)::DIGlobalVariable
end

function expression(gve::DIGlobalVariableExpression)
    ref = API.LLVMDIGlobalVariableExpressionGetExpression(gve)
    ref == C_NULL ? nothing : Metadata(ref)::DIExpression
end

@property DIGlobalVariableExpression variable
@property DIGlobalVariableExpression expression


## global variable

@vocabulary Build global_variable_expression!, temp_global_variable_fwd_decl!

"""
    global_variable_expression!(builder::DIBuilder, scope::Union{DIScope,Nothing},
                              name::AbstractString, linkage::AbstractString,
                              file::DIFile, line::Integer, type::DIType,
                              expression::DIExpression; local_to_unit::Bool=false,
                              declaration=nothing,
                              align_in_bits::Integer=0) -> DIGlobalVariableExpression

Create a new global variable descriptor paired with a DWARF expression.
"""
function global_variable_expression!(builder::DIBuilder, scope::Union{DIScope,Nothing},
                                   name::AbstractString, linkage::AbstractString,
                                   file::DIFile, line::Integer, type::DIType,
                                   expression::DIExpression; local_to_unit::Bool=false,
                                   declaration=nothing,
                                   align_in_bits::Integer=0)
    name = String(name)
    linkage = String(linkage)
    DIGlobalVariableExpression(API.LLVMDIBuilderCreateGlobalVariableExpression(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        linkage, Csize_t(ncodeunits(linkage)),
        file, Cuint(line), type, local_to_unit, expression,
        something(declaration, C_NULL), UInt32(align_in_bits)))
end

"""
    temp_global_variable_fwd_decl!(builder::DIBuilder, scope::Union{DIScope,Nothing},
                               name::AbstractString, linkage::AbstractString,
                               file::DIFile, line::Integer, type::DIType;
                               local_to_unit::Bool=false, declaration=nothing,
                               align_in_bits::Integer=0)
        -> TemporaryMDNode{DIGlobalVariable}

Create a temporary forward declaration of a global variable, to be replaced with the
variable (e.g., `gve.variable` of a [`global_variable_expression!`](@ref)) using
[`replace_temporary!`](@ref).
"""
function temp_global_variable_fwd_decl!(builder::DIBuilder, scope::Union{DIScope,Nothing},
                                    name::AbstractString, linkage::AbstractString,
                                    file::DIFile, line::Integer, type::DIType;
                                    local_to_unit::Bool=false, declaration=nothing,
                                    align_in_bits::Integer=0)
    name = String(name)
    linkage = String(linkage)
    TemporaryMDNode{DIGlobalVariable}(API.LLVMDIBuilderCreateTempGlobalVariableFwdDecl(
        builder, something(scope, C_NULL), name, Csize_t(ncodeunits(name)),
        linkage, Csize_t(ncodeunits(linkage)),
        file, Cuint(line), type, local_to_unit,
        something(declaration, C_NULL), UInt32(align_in_bits)))
end
