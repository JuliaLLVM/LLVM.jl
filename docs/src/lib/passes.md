# Passes

Functions that return the name of one of LLVM's passes, with the options given as keyword
arguments, as a string for use with [`add!`](@ref) or [`run!`](@ref). They are part of the
`LLVM.Passes` vocabulary.

## Module passes

```@autodocs
Modules = [LLVM]
Filter = f -> any(((mod, name, kind),) -> mod === LLVM && kind == "module pass" &&
                                          getfield(mod, name) === f, LLVM.pass_functions)
```

## CGSCC passes

```@autodocs
Modules = [LLVM]
Filter = f -> any(((mod, name, kind),) -> mod === LLVM && kind == "CGSCC pass" &&
                                          getfield(mod, name) === f, LLVM.pass_functions)
```

## Function passes

```@autodocs
Modules = [LLVM]
Filter = f -> any(((mod, name, kind),) -> mod === LLVM && kind == "function pass" &&
                                          getfield(mod, name) === f, LLVM.pass_functions)
```

## Loop passes

```@autodocs
Modules = [LLVM]
Filter = f -> any(((mod, name, kind),) -> mod === LLVM && kind == "loop pass" &&
                                          getfield(mod, name) === f, LLVM.pass_functions)
```

## Alias analyses

```@autodocs
Modules = [LLVM]
Filter = f -> any(((mod, name, kind),) -> mod === LLVM && kind == "alias analysis" &&
                                          getfield(mod, name) === f, LLVM.pass_functions)
```

## Extension point callbacks

```@autodocs
Modules = [LLVM]
Filter = f -> any(((mod, name, kind),) -> mod === LLVM &&
                                          kind == "extension point callbacks" &&
                                          getfield(mod, name) === f, LLVM.pass_functions)
```
