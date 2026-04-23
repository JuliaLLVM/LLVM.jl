# Modules

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

LLVM modules are the main container of LLVM IR code. They are created using the `Module`
constructor (not exported because of the name conflict with `Base.Module`):

```jldoctest module
julia> mod = LLVM.Module("SomeModule")
; ModuleID = 'SomeModule'
source_filename = "SomeModule"
```

The only argument to the constructor is the module's name. Along with some other properties,
this can be read and modified using dedicated functions:

- `name`/`name!`: module name
- `triple`/`triple!`: target triple string
- `datalayout`/`datalayout!`: a data layout string or `DataLayout` object
- `inline_asm`/`inline_asm!`: module-level inline assembly
- `sdk_version`/`sdk_version!`: Apple SDK version
- `set_used!` and `set_compiler_used!`: to set `@llvm.used` and `@llvm.compiler.used`


## Textual representation

In the REPL, LLVM modules are displayed verbosely, i.e., they print their IR code. Simply
printing the module object will instead output a compact object representation, so if you
want to debug your application by printing IR you need to invoke the `display` function or
explicitly `string`ify the object:

```jldoctest module
julia> print(mod)
LLVM.Module("SomeModule")

julia> show(stdout, "text/plain", mod)  # equivalent of `display`
; ModuleID = 'SomeModule'
source_filename = "SomeModule"

julia> @info "My module:\n" * string(mod)
┌ Info: My module:
│ ; ModuleID = 'SomeModule'
└ source_filename = "SomeModule"
```

To parse an LLVM module from a textual string, simply use the `parse` function:

```jldoctest
julia> ir = """
         define i64 @"add"(i64 %0, i64 %1) {
         top:
           %2 = add i64 %1, %0
           ret i64 %2
         }""";

julia> parse(LLVM.Module, ir)
define i64 @add(i64 %0, i64 %1) {
top:
  %2 = add i64 %1, %0
  ret i64 %2
}
```


## Binary representation ("bitcode")

If you need the binary bitcode, you can convert the module to a vector of bytes, or write it
to an I/O stream:

```jldoctest module
julia> # only showing the first two bytes, for brevity
       convert(Vector{UInt8}, mod)[1:2]
2-element Vector{UInt8}:
 0x42
 0x43

julia> sprint(write, mod)[1:2]
"BC"
```

Parsing bitcode is again done with the `parse` function, dispatching on the fact that the
bitcode is represented as a vector of bytes:

```julia-repl
julia> bc = UInt8[0x42, 0x43, ...]

julia> parse(LLVM.Module, bc)
source_filename = "SomeModule"
```


## Contents

To iterate the contents of a module, several iterators are provided (with different levels
of functionality, based on what the LLVM C API provides).

### Global objects

Globals, such as global variables, can be iterated with the `globals` function:

```jldoctest
julia> mod = LLVM.Module("SomeModule");

julia> gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal");

julia> collect(globals(mod))
1-element Vector{GlobalVariable}:
 @SomeGlobal = external global i32
```

In addition to the iteration interface, it is possible to move from one global to the
previous or next one using respectively the `prevglobal` and `nextglobal` functions.

### Functions

Functions can be iterated with the `functions` function:

```jldoctest module
julia> fun = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.VoidType()));

julia> collect(functions(mod))
1-element Vector{LLVM.Function}:
 declare void @SomeFunction()
```

Again, it is possible to move from one function to the previous or next one using
respectively the `prevfun` and `nextfun` functions.

### Flags

Modules can also have flags associated with them, which can be set and retrieved using the
associative iterator returned by the `flags` function:

```jldoctest module
julia> mod = LLVM.Module("SomeModule");

julia> flags(mod)["SomeFlag", LLVM.API.LLVMModuleFlagBehaviorError] = Metadata(ConstantInt(42))
i64 42

julia> mod
; ModuleID = 'SomeModule'
source_filename = "SomeModule"

!llvm.module.flags = !{!0}

!0 = !{i32 1, !"SomeFlag", i64 42}
```

Note the additional argument to `setindex!`, which indicates the flag behavior.


## Linking

Modules can be linked together using the `link!` function. This function takes two modules,
destroying the source module in the process:

```jldoctest
julia> src = parse(LLVM.Module, "define void @foo() { ret void }");

julia> dst = parse(LLVM.Module, "define void @bar() { ret void }");

julia> link!(dst, src)

julia> dst
define void @bar() {
  ret void
}

define void @foo() {
  ret void
}
```

Pass `only_needed=true` to only link symbols from the source module that are
referenced (but not defined) in the destination module, leaving unreferenced
definitions behind:

```jldoctest
julia> src = parse(LLVM.Module, """
           define void @needed() { ret void }
           define void @extra() { ret void }""");

julia> dst = parse(LLVM.Module, """
           declare void @needed()
           define void @caller() {
             call void @needed()
             ret void
           }""");

julia> link!(dst, src; only_needed=true)

julia> dst
define void @caller() {
  call void @needed()
  ret void
}

define void @needed() {
  ret void
}
```

Pass `override_from_src=true` to have definitions in the source module shadow
any conflicting definitions in the destination module.
