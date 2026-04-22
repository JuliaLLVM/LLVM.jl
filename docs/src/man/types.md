# Types

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

LLVM has a rich type-system that is used to describe the types of values in a program.
LLVM.jl attempts to reconstruct as much of this type-system as possible in Julia, beyond the
abstractions that the C API provides (where types are opaque objects).

Each type object, represented by a subtype of `LLVMType`, supports a few property functions:

- `issized`: whether the type has a fixed size (e.g. `Int32`).
- `context`: the context in which the type was created.
- `eltype`: the element type of the type (if applicable, e.g., for arrays and vectors).

The types describing LLVM types are not exported by default, so always need to be prefixed
by the `LLVM` module name.


## Integer types

Integer types are subtypes of the `LLVM.IntegerType` abstract type. They can be created
using explicit constructors that encode the bit-width, or using constructors that take a
width argument:

```jldoctest
julia> LLVM.Int1Type()
i1

julia> LLVM.IntType(32)
i32
```

It is possible to query the bit-width of an integer type using the `width` function:

```jldoctest
julia> width(LLVM.Int32Type())
32
```


## Floating-point types

Floating-point types are subtypes of the `LLVM.FloatingPointType` abstract type. They can
only be constructed using explicitly named constructors:

```jldoctest
julia> LLVM.HalfType()
half

julia> LLVM.BFloatType()
bfloat
```


## Function types

Function types are used to create functions, and encode both the return type and the
argument types, which can be queried using respectively the `return_type` and `parameters`
functions.

```jldoctest
julia> LLVM.FunctionType(LLVM.Int1Type())
i1 ()

julia> ft = LLVM.FunctionType(LLVM.Int1Type(), [LLVM.FloatType()])
i1 (float)

julia> return_type(ft)
i1

julia> parameters(ft)
1-element Vector{LLVMType}:
 float
```

To create vararg functions, the `vararg` keyword argument to the `FunctionType` constructor
can be used. This property can be queried using the `isvararg` function.


## Pointer types

Pointer types are represented by the `LLVM.PointerType` type. Depending on the LLVM version,
they can be opaque or have an element type that can be queried using the `eltype` function:

```jldoctest
julia> supports_typed_pointers()
false

julia> ty = LLVM.PointerType(LLVM.Int1Type())
ptr

julia> eltype(ty)
ERROR: Taking the type of an opaque pointer is illegal
```

```julia-repl
julia> supports_typed_pointers()
false

julia> ty = LLVM.PointerType()
ptr

julia> eltype(ty)
ERROR: Taking the type of an opaque pointer is illegal
```

When constructing a pointer type, you can also set the address space, and query it back
using the `addrspace` function:

```jldoctest
julia> ty = LLVM.PointerType(LLVM.Int1Type(), 1)
ptr addrspace(1)

julia> addrspace(ty)
1
```


## Array types

LLVM arrays represent a fixed-size, homoeneous collection of elements. They can be created
using the `LLVM.ArrayType` constructor:

```jldoctest
julia> ty = LLVM.ArrayType(LLVM.Int1Type(), 8)
[8 x i1]

julia> length(ty)
8

julia> eltype(ty)
i1
```


## Vector types

LLVM vectors are similar, but mostly used for SIMD operations:

```jldoctest
julia> ty = LLVM.VectorType(LLVM.Int1Type(), 8)
<8 x i1>

julia> length(ty)
8

julia> eltype(ty)
i1
```


## Structure types

Structure types are used to represent a collection of elements of different types. They can
be created using the `LLVM.StructType` constructor:

```jldoctest
julia> ty = LLVM.StructType([LLVM.Int32Type(), LLVM.FloatType()])
{ i32, float }
```

It is also possible to start with an empty type and add elements to it:

```jldoctest
julia> ty = LLVM.StructType("MyStruct")
%MyStruct = type opaque

julia> elements!(ty, [LLVM.Int32Type(), LLVM.FloatType()])

julia> ty
%MyStruct = type { i32, float }
```

Structure types support a number of queries:

- `name`: the name of the structure type.
- `elements`: the element types of the structure type.
- `ispacked`: whether the structure is packed.
- `isopaque`: whether the structure is opaque.
- `isempty`: whether the structure is empty.


## Other types

There are a few other types that do not fit in a specific category:

- `LLVM.VoidType`: the `void` type.
- `LLVM.LabelType`: the `label` type.
- `LLVM.MetadataType`: the `metadata` type.
- `LLVM.TokenType`: the `token` type.


## Type iteration

Although uncommon, it is possible to iterate the types that are registered in a context
using the iterator returned by the `types` function. This iterator is not actually
iterable, but it can be used to check whether a type is registered in a context:

```jldoctest
julia> ctx = context();

julia> haskey(types(ctx), "Foo")
false

julia> ty = LLVM.StructType("Foo")
%Foo = type opaque

julia> haskey(types(ctx), "Foo")
true

julia> types(ctx)["Foo"]
%Foo = type opaque
```
