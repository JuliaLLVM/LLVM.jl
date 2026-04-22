# Functions

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

Functions are part of a module, and represent a callable piece of code. They can be looked up in a module using the `functions` iterator, or created from scratch:

```jldoctest function
julia> mod = LLVM.Module("SomeModule");

julia> fty = LLVM.FunctionType(LLVM.VoidType(), [LLVM.Int32Type()])
void (i32)

julia> fun = LLVM.Function(mod, "SomeFunction", fty)
declare void @SomeFunction(i32)
```

Several APIs can be used to interact with functions:

- `function_type`: get the function type of the function (this differs from `value_type`, which will return a pointer to the function type).
- `personality`/`personality!`: get or set the personality function of the function (pass `nothing` to remove the personality function).
- `callconv`/`callconv!`: get or set the calling convention of the function.
- `gc`/`gc!`: get or set the garbage collector for the function.
- `isintrinsic`: check if the function is an intrinsic.
- `erase!`: delete the function from its parent module, and delete the object.


## Intrinsics

Intrinsic functions are special function declarations that are recognized by the LLVM
compiler and possibly treated specially by the code generator. It is normally not necessary
to create `Intrinsic` objects directly, as function declarations that match an intrinsic's
name and signature will be treated as such:

```jldoctest
julia> mod = LLVM.Module("SomeModule");

julia> f = LLVM.Function(mod, "llvm.trap", LLVM.FunctionType(LLVM.VoidType()))
; Function Attrs: cold noreturn nounwind memory(inaccessiblemem: write)
declare void @llvm.trap() #0

julia> isintrinsic(f)
true
```

However, the `Intrinsic` type supports additional APIs:

- `name`: get the base name of the intrinsic, or a specific overloaded name by passing additional argument types.
- `isoverloaded`: check if the intrinsic is overloaded.

It can also be useful to construct a function from a well-known intrinsic, to make sure the
overloaded name is correct:

```jldoctest
julia> mod = LLVM.Module("SomeModule");

julia> intr = LLVM.Intrinsic("llvm.abs")
Intrinsic(5): overloaded intrinsic

julia> isoverloaded(intr)
true

julia> LLVM.Function(mod, intr, [LLVM.Int32Type()])
; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.abs.i32(i32, i1 immarg) #0
```


## Attributes

Functions can have attributes associated with them, which can be set and retrieved using the
iterators returned by the `function_attributes`, `parameter_attributes` and
`return_attributes` functions to respectively set attributes on the function, its
parameters, and its return value:

```jldoctest function
julia> push!(function_attributes(fun), StringAttribute("nounwind"))

julia> push!(parameter_attributes(fun, 1), StringAttribute("nocapture"))

julia> push!(return_attributes(fun), StringAttribute("sret"))

julia> mod
; ModuleID = 'SomeModule'
source_filename = "SomeModule"

declare "sret" void @SomeFunction(i32 "nocapture") #0

attributes #0 = { "nounwind" }
```

Attributes can be removed from these iterators using the `delete!` function.

Different kinds of attributes are supported:

- `EnumAttribute`: an attribute identified by its enum id, optionally associated with an
  integer value.
- `StringAttribute`: an attribute identified by its string name, optionally associated with
  a string value
- `TypeAttribute`: an attribute identified by its enum id, associated with a type.

```jldoctest
julia> EnumAttribute("nounwind")
EnumAttribute 38=0

julia> StringAttribute("frame-pointer", "none")
StringAttribute frame-pointer=none

julia> TypeAttribute("byval", LLVM.Int32Type())
TypeAttribute 74=LLVM.IntegerType(i32)
```


## Parameters

Parameters are values that represent the arguments to a function, and can be used as operands to other values. They can be queried using the `parameters` function:

```jldoctest function
julia> collect(parameters(fun))
1-element Vector{Argument}:
 i32 %0
```

## Basic blocks

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end

    ir = """
        define i64 @"add"(i64 %0, i64 %1) {
        top:
          %2 = add i64 %1, %0
          ret i64 %2
        }"""
    mod = parse(LLVM.Module, ir);
    fun = only(functions(mod));
end
```

Functions are composed of basic blocks, which are sequences of instructions that are
executed in order. Basic blocks can be iterated using the `blocks` function:

```jldoctest
julia> fun
define i64 @add(i64 %0, i64 %1) {
top:
  %2 = add i64 %1, %0
  ret i64 %2
}

julia> collect(blocks(fun))
1-element Vector{BasicBlock}:
 BasicBlock("top")

julia> # to simply get the first block
       entry(fun)
top:
  %2 = add i64 %1, %0
  ret i64 %2
```

In addition to the iteration interface, it is possible to move from one basic block to the
previous or next one using respectively the `prevblock` and `nextblock` functions.
