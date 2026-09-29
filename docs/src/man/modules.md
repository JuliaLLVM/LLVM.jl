# Modules

```@meta
DocTestSetup = quote
    using LLVM, LLVM.IR, LLVM.Build, LLVM.Passes, LLVM.ORC

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

The only argument to the constructor is the module's name. Along with some other
attributes, this can be read and modified using properties:

- `mod.name`: module name
- `mod.triple`: target triple string
- `mod.datalayout`: data layout, which can be assigned a string or `DataLayout` object
- `mod.sdk_version`: Apple SDK version

The global values that should be kept even if they appear to be unused, i.e., those in
`@llvm.used` and `@llvm.compiler.used`, are available as the `used` and `compiler_used`
properties, which are sets that support `push!` and `delete!`:

```jldoctest used
julia> mod = LLVM.Module("SomeModule");

julia> gv = GlobalVariable(mod, LLVM.Int32Type(), "kept");

julia> push!(mod.used, gv);

julia> gv in mod.used
true

julia> mod.globals["llvm.used"]
@llvm.used = appending global [1 x ptr] [ptr @kept], section "llvm.metadata"
```

Module-level inline assembly is a collection of assembly fragments, available as the
`inline_asm` property. Fragments can be added with `push!` and removed with `empty!`, while
`String` returns the assembly text:

```jldoctest module
julia> push!(mod.inline_asm, "nop");

julia> String(mod.inline_asm)
"nop\n"

julia> empty!(mod.inline_asm);

julia> isempty(mod.inline_asm)
true
```


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

The contents of a module are available as properties, which return views of the module
(with different levels of functionality, based on what the LLVM C API provides). These views
always reflect the current contents of the module, and can be indexed by name.

### Global objects

Globals, such as global variables, are available as the `globals` property:

```jldoctest
julia> mod = LLVM.Module("SomeModule");

julia> gv = GlobalVariable(mod, LLVM.Int32Type(), "SomeGlobal");

julia> collect(mod.globals)
1-element Vector{GlobalVariable}:
 @SomeGlobal = external global i32
```

In addition to iterating the globals of a module, it is possible to move from one global to
the previous or next one using respectively the `gv.prev` and `gv.next` properties.
Global variables can be reordered with `move_before` and `move_after`, or sorted in place
with `sort!(mod.globals)`. The latter defaults to sorting by name, which is useful for
producing deterministic module layouts.

### Functions

Functions are available as the `functions` property:

```jldoctest module
julia> fun = LLVM.Function(mod, "SomeFunction", LLVM.FunctionType(LLVM.VoidType()));

julia> collect(mod.functions)
1-element Vector{LLVM.Function}:
 declare void @SomeFunction()
```

Again, it is possible to move from one function to the previous or next one using
respectively the `f.prev` and `f.next` properties. Functions can be reordered with
`move_before` and `move_after`, or sorted by name with `sort!(mod.functions)` to produce a
deterministic module layout.

### Aliases and ifuncs

Global aliases and ifuncs are not included in the `globals` or `functions` of a module, and
are available as the separate `aliases` and `ifuncs` properties:

```jldoctest module
julia> ga = GlobalAlias(mod, fun, "SomeAlias");

julia> collect(mod.aliases)
1-element Vector{GlobalAlias}:
 @SomeAlias = alias void (), ptr @SomeFunction
```

Here too it is possible to move to the previous or next element with the `prev` and `next`
properties.

### Flags

Modules can also have flags associated with them, which can be set and retrieved using the
dictionary-like view returned by the `flags` property:

```jldoctest module
julia> mod = LLVM.Module("SomeModule");

julia> mod.flags["SomeFlag", LLVM.ModuleFlagBehavior.Error] = Metadata(ConstantInt(42))
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
