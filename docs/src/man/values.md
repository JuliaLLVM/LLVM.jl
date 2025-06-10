# Values

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

Values are the basic building blocks of a program. They are the simplest form of data that
can be manipulated by a program. Many things in LLVM are considered a value: not only
constants, but also instructions, functions, etc.


## General APIs

The `Value` type is the abstract type that represents all values in LLVM. It supports
a range of general APIs that are common to all values:

- `value_type`: get the type of the value.
- `context`: get the context in which the value was created.
- `name!`/`name`: get or set the name of the value.


## User values

A `User` is a value that can have other values as operands. It is the base type for
instructions, functions, and other values that are composed of other values. It supports
a few additional APIs:

- `operands`: get the operands of the user.


## Constant values

Many values are actually constant, i.e., they are known to be immutable at run time.
Constant numbers are examples of constants, but also functions and global variables, because
their address is immutable.

It is possible to quickly create all-zeros and all-ones constants using the `null` and
`all_ones` functions:

```jldoctest
julia> null(LLVM.Int1Type())
i1 false

julia> all_ones(LLVM.FloatType())
float 0xFFFFFFFFE0000000
```

### Constant data

There are several kinds of constant data that can be represented in LLVM. Singleton
constants, which include null, undef, and poison values, can be created using constructors
that take a single type as argument:

```jldoctest
julia> PointerNull(LLVM.PointerType(LLVM.Int1Type()))
i1* null

julia> UndefValue(LLVM.Int1Type())
i1 undef

julia> PoisonValue(LLVM.Int1Type())
i1 poison
```

Constant numbers can be created by passing a type and a value, or simply a value in which
case the Julia type will be mapped to the corresponding LLVM type:

```jldoctest
julia> ConstantInt(LLVM.Int1Type(), 1)
i1 true

julia> ConstantInt(true)
i1 true

julia> ConstantFP(LLVM.FloatType(), 1.0)
float 1.000000e+00

julia> ConstantFP(1.0f0)
float 1.000000e+00
```

It is possible to extract the value of a constant using the `convert` function:

```jldoctest
julia> c = ConstantFP(Float16(1))
half 0xH3C00

julia> convert(Float16, c)
Float16(1.0)
```

Constant structures can be created using the `ConstantStruct` constructor:

```jldoctest
julia> ty = LLVM.StructType([LLVM.Int32Type()])
{ i32 }

julia> ConstantStruct(ty, [LLVM.ConstantInt(Int32(42))])
{ i32 } { i32 42 }

julia> # short-hand where the LLVM type is inferred
       ConstantStruct([LLVM.ConstantInt(Int32(42))])
{ i32 } { i32 42 }
```


Sequential constants, i.e., arrays and vectors, can be created using the `ConstantDataArray`
and `ConstantDataVector` constructors, which again supports the shorthand of only passing
Julia values:

```jldoctest
julia> ConstantDataArray(LLVM.Int32Type(), Int32[1, 2])
[2 x i32] [i32 1, i32 2]

julia> # short-hand where the LLVM type is inferred
       ConstantDataArray(Int32[1, 2])
[2 x i32] [i32 1, i32 2]
```

!!! note

    `ConstantDataVector` is currently not implemented.

While `ConstantDataArray` only supports simple element types, `ConstantArray` supports
arbitrary aggregates as elements:

```jldoctest
julia> val = ConstantStruct([LLVM.ConstantInt(Int32(42))])
{ i32 } { i32 42 }

julia> ty = value_type(val)
{ i32 }

julia> ConstantArray(ty, [val])
[1 x { i32 }] [{ i32 } { i32 42 }]
```

Both `ConstantDataArray` and `ConstantArray` can, to some extent, be manipulated with plain
Julia array operations:

```jldoctest
julia> arr = ConstantArray([1, 2])
[2 x i64] [i64 1, i64 2]

julia> length(arr)
2

julia> arr[1]
i64 1
```

### Constant expressions

Constant expressions are a way to represent computations that are known at compile time.
Their support in LLVM is diminishing, and their results are often constant-folded to other
constants, but the ones that remain can be constructed with `const_`-prefixed functions in
LLVM.jl:

```jldoctest
julia> const_neg(ConstantInt(1))
i64 -1

julia> const_inttoptr(ConstantInt(42), LLVM.PointerType(LLVM.Int1Type()))
i1* inttoptr (i64 42 to i1*)
```

For the exact list of supported constant expressions, refer to the LLVM documentation.

### Inline assembly

Inline assembly is a way to include raw assembly code in a program. It is often used to
access features that are not directly supported by LLVM, or to optimize specific parts of a
program.

The `InlineAsm` constructor takes a function type, assembly string, constraints string, and
a boolean indicating whether the assembly has side effects:

```jldoctest
julia> InlineAsm(LLVM.FunctionType(LLVM.VoidType()), "nop", "", false)
void ()* asm "nop", ""
```

For more details on inline assembly, particularly the format of the constraints string,
refer to the LLVM documentation.

### Global values

Global values are values that are encoded at the top level of a module. They support a
couple of additional APIs:

- `global_value_type`: get the type of the global value.
- `isdeclaration`: whether the global value is a declaration, i.e., it does not have a body.
- `linkage`/`linkage!`: get or set the linkage of the global value.
- `visibility`/`visibility!`: get or set the visibility of the global value.
- `section`/`section!`: get or set the section of the global value.
- `dllstorage`/`dllstorage!`: get or set the DLL storage class of the global value.
- `unnamed_addr`/`unnamed_addr!`: get or set whether the global value has an unnamed address.
- `local_unnamed_addr`/`local_unnamed_addr!`: get or set whether the global value has a local unnamed address.

The most common type of global value is the global variable, which can be created using the `GlobalVariable` constructor:

```jldoctest
julia> mod = LLVM.Module("SomeModule");

julia> ty = LLVM.Int32Type();

julia> gv = GlobalVariable(mod, ty, "SomeGV")
@SomeGV = external global i32
```

Global variables support additional APIs:

- `initializer`/`initializer!`: get or set the initializer of the global variable to a
  constant value (pass `nothing` to remove the initializer).
- `isthreadlocal`/`isthreadlocal!`: get or set whether the global variable is thread-local.
- `isconstant`/`isconstant!`: get or set whether the global variable is constant.
- `isextinit`/`isextinit!`: get or set whether the global variable is externally initialized.
- `erase!`: delete the global variable from its parent module, and delete the object.
- `alignment`/`alignment!`: get or set the alignment of the global variable.


## Uses

It is possible to inspect the uses of a value using the iterator returned by the `uses`
function. This iterator returns `Use` objects which contain both the user and the original
value:

```jldoctest
julia> c1 = ConstantInt(42);

julia> c2 = const_inttoptr(c1, LLVM.PointerType(LLVM.Int1Type()));

julia> use = only(uses(c1));

julia> user(use)
i1* inttoptr (i64 42 to i1*)

julia> value(use)
i64 42
```

It is also possible to _replace_ uses of a value using the `replace_uses!` function
(commonly referred to as "RAUW" in LLVM):

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = parse(LLVM.Module, """
        define i64 @"add"(i64 %0, i64 %1) {
        top:
          %2 = add i64 %1, %0
          ret i64 %2
        }""")

    inst1, inst2 = instructions(entry(functions(mod)["add"]))
end
```

```jldoctest
julia> inst1
%2 = add i64 %1, %0

julia> inst2
ret i64 %2

julia> replace_uses!(inst1, ConstantInt(Int64(42)))

julia> inst2
ret i64 42
```
