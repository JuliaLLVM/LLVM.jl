# Metadata

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end
end
```

Metadata is a way to attach additional information to various entities in LLVM IR. Various
metadata object types exist, each prefixed with `MD`, and with a variety of APIs to query
and manipulate them. The abstract supertype of all metadata objects is `Metadata`.

Metadata strings can be constructed with the `MDString` constructor, and converted back to
Julia strings with a `convert` call:

```jldoctest
julia> md = MDString("hello")
!"hello"

julia> convert(String, md)
"hello"
```

Then there are metadata nodes, which can have other metadata as operands. For example, a
tuple of metadata nodes can be created with the following `MDNode` constructor:

```jldoctest mdtuple
julia> md = MDNode([MDString("hello"), MDString("world")])
<0x6000022c7f20> = !{!"hello", !"world"}
```

These operands can be extracted again using the `operands` function:

```jldoctest mdtuple
julia> operands(md)
2-element Vector{MDString}:
 !"hello"
 !"world"
```


## Converting between Metadata and Value

It happens often that you want to put values in metadata, or use metadata with APIs that
expect a value. In LLVM.jl, this is possible by using the `Value` and `Metadata`
constructors with respectively `Metadata` and `Value` inputs, automatically wrapping them in
the correct type:

```jldoctest
julia> val = ConstantInt(42);

julia> md = Metadata(val)
i64 42

julia> typeof(md)
LLVM.ConstantAsMetadata
```

```jldoctest
julia> md = MDString("test");

julia> val = Value(md)
!"test"

julia> typeof(val)
LLVM.MetadataAsValue
```


## Inspecting and attaching

Any global value and instruction can have metadata attached to it. In LLVM.jl, it's possible
to inspect and mutate that metadata using the `metadata` function:

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end

    ir = """
        define i64 @"julia_+"(i64 signext %0, i64 signext %1) {
        top:
          %2 = add i64 %1, %0
          ret i64 %2
        }"""
    mod = parse(LLVM.Module, ir)
    add = only(functions(mod))
    bb = entry(add)
    inst = first(instructions(bb))
end
```

```jldoctest
julia> inst
%2 = add i64 %1, %0

julia> isempty(metadata(inst))
true

julia> metadata(inst)["dbg"] = MDNode([MDString("hello")])
<0x5ff68c3f2f28> = !{!"hello"}

julia> inst
%2 = add i64 %1, %0, !dbg !0
```

Metadata can also be attached to a module, in which case it needs to be grouped in a named
metadata node (which can only contain other metadata nodes, and not e.g. strings directly).
Operands can be appended to module-level named metadata with `push!` and removed with
`empty!`:

```jldoctest
julia> md = metadata(mod);

julia> isempty(md)
true

julia> push!(md["hello"], MDNode([MDString("world")]));

julia> md
ModuleMetadataIterator for module :
  !hello = !{<0x60000394d3f8> = !{!"world"}}
```


## Debug information

When generating IR, it is possible to add debug information metadata to the generated code.
This information can be used by debuggers to provide a better debugging experience.

!!! note

    LLVM.jl currently does not offer extensive wrappers for generating debug info, and is
    mostly focussed on the ability to inspect or copy existing information.

LLVM represents debug information as a variety of `DI`-prefixed structures, which are
subtypes of the above metadata types. In LLVM.jl, various functions are provided to inspect
properties of these structures:

- `DILocation`: `line`, `column`, `scope`, `inlined_at`
- `DIVariable`: `file`, `scope`, `line`
- `DIScope`: `file`, `name`
- `DIFile`: `directory`, `filename`, `source`
- `DIType`: `name`, `sizeof`, `offset`, `line`, `flags`
- `DISubProgram`: `line` (and methods inherited from `DIScope`)

To query the debug info attached to an instruction, one queries the `!dbg` metadata using
`metadata(inst)["dbg"]`.

```@meta
DocTestSetup = quote
    using LLVM

    if context(; throw_error=false) === nothing
        Context()
    end

    ir = """
        define i64 @"julia_+"(i64 signext %0, i64 signext %1) !dbg !5 {
        top:
          %2 = add i64 %1, %0, !dbg !15
          ret i64 %2, !dbg !15
        }

        !llvm.module.flags = !{!0, !1, !2}
        !llvm.dbg.cu = !{!3}

        !0 = !{i32 2, !"Dwarf Version", i32 4}
        !1 = !{i32 2, !"Debug Info Version", i32 3}
        !2 = !{i32 2, !"julia.debug_level", i32 1}
        !3 = distinct !DICompileUnit(language: DW_LANG_Julia, file: !4, producer: "julia", isOptimized: true, runtimeVersion: 0, emissionKind: NoDebug, nameTableKind: GNU)
        !4 = !DIFile(filename: "julia", directory: ".")
        !5 = distinct !DISubprogram(name: "+", linkageName: "julia_+", scope: null, file: !6, line: 87, type: !7, scopeLine: 87, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !3, retainedNodes: !8)
        !6 = !DIFile(filename: "int.jl", directory: ".")
        !7 = !DISubroutineType(types: !8)
        !8 = !{}
        !15 = !DILocation(line: 87, scope: !5)"""
    mod = parse(LLVM.Module, ir)
    add = only(functions(mod))
    bb = entry(add)
    inst = first(instructions(bb))
end
```

```jldoctest
julia> inst
%2 = add i64 %1, %0, !dbg !9

julia> dbg = metadata(inst)["dbg"]
!DILocation(line: 87, scope: <0x6000056c5dd0>) = !DILocation(line: 87, scope: <0x6000056c5dd0>)

julia> line(dbg)
87

julia> file(scope(dbg))
<0x6000000beb80> = !DIFile(filename: "int.jl", directory: ".")
```

Debug info can also be attached to functions, which can be queried and modified using
respectively `subprogram` and `subprogram!`:

```jldoctest
julia> sp = subprogram(add)
<0x600003edfad0> = distinct !DISubprogram(name: "+", linkageName: "julia_+", scope: null, file: <0x600003ba6fe0>, line: 87, type: <0x600003494c90>, scopeLine: 87, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: <0x6000021d8428>, retainedNodes: <0x6000010f09d0>)
```
