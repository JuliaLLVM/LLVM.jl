# Values

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

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

- `val.value_type`: the type of the value.
- `val.name`: the name of the value, which can also be assigned to.
- `context(val)`: the context in which the value was created.
- `take_name!(val, from)`: give `val` the name of `from`, which becomes unnamed. Assigning
  the name instead would make LLVM add a suffix, as `from` still uses it.
- `strip_pointer_casts(val)`: the value behind any bitcasts, address space casts and
  `getelementptr`s with all-zero indices, like C++'s `Value::stripPointerCasts`.
  `strip_pointer_casts_and_aliases` also looks through global aliases.


## User values

A `User` is a value that can have other values as operands. It is the base type for
instructions, functions, and other values that are composed of other values. It supports
a few additional APIs:

- `user.operands`: the operands of the user, as a mutable view: assigning to an element,
  `user.operands[i] = val`, replaces that operand.


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
ptr null

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

Floating-point constants pass through a `Float64`, which cannot represent every value of
wider types like `fp128` or `x86_fp80`. To create such constants exactly, pass their bit
pattern instead, and use the `bitpattern` property to get it back:

```jldoctest
julia> ConstantFP(LLVM.FP128Type(), 0.1)    # rounded to Float64 precision
fp128 0xLA0000000000000003FFB999999999999

julia> c = ConstantFP(LLVM.FP128Type(); bits=0x3ffb999999999999999999999999999a)
fp128 0xL999999999999999A3FFB999999999999

julia> c.bitpattern
0x3ffb999999999999999999999999999a
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

julia> ty = val.value_type
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
ptr inttoptr (i64 42 to ptr)
```

For the exact list of supported constant expressions, refer to the LLVM documentation.

Constant expressions (and constant aggregates) that wrap a given set of constants can be
rewritten into equivalent instructions at each point of use, using
`convert_users_to_instructions!`. This is useful when a constant needs to be replaced with a
function-local value, which is not a valid operand of a constant expression. For example,
given a global variable that is used through a `getelementptr` constant expression:

```llvm
define i32 @getelem() {
entry:
  %0 = load i32, ptr getelementptr inbounds ([4 x i32], ptr @myglobal, i32 0, i32 2), align 4
  ret i32 %0
}
```

calling `convert_users_to_instructions!([myglobal])` materializes the constant expression as
a regular instruction:

```llvm
define i32 @getelem() {
entry:
  %0 = getelementptr inbounds [4 x i32], ptr @myglobal, i32 0, i32 2
  %1 = load i32, ptr %0, align 4
  ret i32 %1
}
```

Operands of `phi` nodes are materialized in their respective incoming blocks. The function
returns whether anything was changed, and by default removes the constants that became dead
as a result of the rewrite.

### Inline assembly

Inline assembly is a way to include raw assembly code in a program. It is often used to
access features that are not directly supported by LLVM, or to optimize specific parts of a
program.

The `InlineAsm` constructor takes a function type, assembly string, constraints string, and
a boolean indicating whether the assembly has side effects:

```jldoctest
julia> InlineAsm(LLVM.FunctionType(LLVM.VoidType()), "nop", "", false)
ptr asm "nop", ""
```

For more details on inline assembly, particularly the format of the constraints string,
refer to the LLVM documentation.

### Global values

Global values are values that are encoded at the top level of a module. They support a
couple of additional APIs:

- `gv.global_value_type`: the type of the global value.
- `gv.parent`: the module that contains the global value.
- `gv.linkage`, `gv.visibility`, `gv.section`, `gv.dllstorage`: the linkage, visibility,
  section and DLL storage class of the global value.
- `gv.unnamed_addr`: whether the address of the global value is significant, e.g.,
  `LLVM.API.LLVMGlobalUnnamedAddr` for an `unnamed_addr` global.
- `isdeclaration(gv)`: whether the global value is a declaration, i.e., it does not have a
  body.

All of these properties, except for `global_value_type`, can also be assigned to (the
section only on global objects, i.e., not on aliases).

The most common type of global value is the global variable, which can be created using the `GlobalVariable` constructor:

```jldoctest
julia> mod = LLVM.Module("SomeModule");

julia> ty = LLVM.Int32Type();

julia> gv = GlobalVariable(mod, ty, "SomeGV")
@SomeGV = external global i32
```

Global variables support additional APIs:

- `gv.initializer`: the initializer of the global variable, a constant value (assign
  `nothing` to remove the initializer).
- `gv.alignment`: the alignment of the global variable.
- `gv.threadlocal`: whether the global variable is thread-local.
- `gv.threadlocal_mode`: the thread-local storage model of the global variable.
- `gv.constant`: whether the global variable is constant.
- `gv.externally_initialized`: whether the global variable is externally initialized.
- `erase!`: delete the global variable from its parent module, and delete the object.

All of these properties can be assigned to. The `threadlocal` flag is a view of the
thread-local mode: making a variable thread-local selects the general dynamic model, which
can be refined by assigning to `threadlocal_mode`:

```jldoctest
julia> mod = LLVM.Module("SomeModule");

julia> gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGV");

julia> gv.threadlocal = true;

julia> gv.threadlocal_mode
LLVMGeneralDynamicTLSModel::LLVMThreadLocalMode = 0x00000001

julia> gv.threadlocal_mode = LLVM.API.LLVMLocalExecTLSModel;

julia> gv
@SomeGV = external thread_local(localexec) global i32
```

A global alias introduces a new symbol for an existing global value, or for a constant
expression involving one. It can be created with the `GlobalAlias` constructor, which takes
the value type and address space from the global value it refers to:

```jldoctest
julia> mod = LLVM.Module("SomeModule");

julia> gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGV");

julia> gv.initializer = ConstantInt(Int32(42));

julia> ga = GlobalAlias(mod, gv, "SomeAlias")
@SomeAlias = alias i32, ptr @SomeGV

julia> ga.aliasee
@SomeGV = global i32 42
```

For constant expressions, pass the value type explicitly, as in
`GlobalAlias(mod, typ, aliasee, name)`. The aliasee can be changed by assigning to the
`aliasee` property.

Similarly, an indirect function or ifunc is a symbol whose address is determined at load
time by calling a resolver function. It is created with the `GlobalIFunc` constructor, which
takes the function type of the ifunc (not that of the resolver), and the resolver itself.
The resolver is available as the `resolver` property, which can also be assigned to, and
the ifunc can be removed with `erase!`.


## Uses

It is possible to inspect the uses of a value using its `uses` property, a read-only view
of `Use` objects, whose `user` and `value` properties refer to respectively the user and the
original value:

```jldoctest
julia> c1 = ConstantInt(42);

julia> c2 = const_inttoptr(c1, LLVM.PointerType(LLVM.Int1Type()));

julia> use = only(c1.uses);

julia> use.user
ptr inttoptr (i64 42 to ptr)

julia> use.value
i64 42
```

It is also possible to _replace_ uses of a value using the `replace_uses!` function
(commonly referred to as "RAUW" in LLVM):

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = parse(LLVM.Module, """
        define i64 @"add"(i64 %0, i64 %1) {
        top:
          %2 = add i64 %1, %0
          ret i64 %2
        }""")

    inst1, inst2 = mod.functions["add"].entry.instructions
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

To only replace the uses of a value in a specific instruction, replace the matching
operands of the instruction using `replace!(inst.operands, old => new)`.

When only the users of a value are needed, use its `users` property, which returns the
`user` of each use (so a user that uses the value multiple times occurs multiple times).
After replacing or erasing the instructions that use a constant, constant expressions that
used it may linger without being used themselves. These can be removed with
`remove_dead_constant_users!(c)`, e.g., before checking whether a global variable is still
used.
