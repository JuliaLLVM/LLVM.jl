# Julia integration

```@meta
DocTestSetup = quote
    using LLVM
    using LLVM.Interop
    using InteractiveUtils

    if context(; throw_error=false) === nothing
        Context()
    end

    mod = LLVM.Module("SomeModule")
end
```

LLVM.jl offers several tools to interoperate with Julia's LLVM-based code generator. These
tools are part of the `Interop` submodule, and need to be imported explicitly.


## Type conversion

Julia types can be converted to their LLVM counterparts using the `convert` function.

```jldoctest
julia> convert(LLVMType, Int64)
i64
```

By default, this conversion rejects boxed types, as the result would often be surprising.
It is possible to query whether a type would be boxed using the `isboxed` function,
and/or allow the conversion by passing `allow_boxed=true` to `convert`:

```jldoctest
julia> isboxed(Int64)
false

julia> isboxed(String)
true

julia> convert(LLVMType, String; allow_boxed=true)
ptr addrspace(10)
```

Another useful query is `isghosttype`, which returns whether a type is a ghost type, i.e.,
a sizeless type:

```jldoctest
julia> isghosttype(Nothing)
true

julia> isghosttype(LLVM.VoidType())
true
```


## Generated IR functions

With generated functions, it is possible to manually generate Julia IR when a function is
visited by the Julia compiler. LLVM.jl extends this with the ability to define functions
that generate LLVM IR; which is very useful to generate code that is not easily expressible
in Julia:

```jldoctest
@generated function add(x::T, y::T) where {T}
  @dispose ctx=Context() begin
    # get the element type
    eltyp = convert(LLVMType, T)

    # create a function
    paramtyps = [eltyp, eltyp]
    f, ft = create_function(eltyp, paramtyps)

    # generate IR
    @dispose builder=IRBuilder() begin
      entry = BasicBlock(f, "entry")
      position!(builder, entry)

      val = add!(builder, parameters(f)[1], parameters(f)[2])

      ret!(builder, val)
    end

    call_function(f, T, Tuple{T, T}, :x, :y)
  end
end

@code_llvm debuginfo=:none add(1, 2)

add(1,2)

# output

; Function Signature: add(Int64, Int64)
define i64 @julia_add_3944(i64 signext %"x::Int64", i64 signext %"y::Int64") #0 {
top:
  %0 = add i64 %"y::Int64", %"x::Int64"
  ret i64 %0
}
3
```

The `call_function` is where the magic happens: it generates LLVM IR for the function that's
being called, and embeds it in the generated Julia IR so that it can be processed by the
Julia compiler.


## Inline assembly

An extension of this mechanism is the `@asmcall` macro, which allows embedding inline
assembly in Julia functions:

```julia-repl
julia> add(x::Int, y::Int) = @asmcall("add \$0, \$1, \$2", "=r,r,r",
                                      Int64, Tuple{Int64,Int64},
                                      x, y);

julia> add(1,2)
3

julia> @code_native add(1,2)
	; InlineAsm Start
	add	x0, x0, x1
	; InlineAsm End
	ret
```


## LLVM pointers

Julia's pointer type `Ptr` only keeps track of the element type, and not the address space.
Julia has `Core.LLVMPtr` to track address space information, with the necessary codegen
support, but no utility functions. LLVM.jl provides the functionality that's commonly needed
when working with pointers:

- `pointerref`: get the value of memory, at a specific index, with specific alignment
- `pointerset`: set the value of memory, at a specific index, with specific alignment
- `unsafe_load` and `unsafe_store!`: higher-level versions of these functions
- basic pointer arithmetic: conversions to/from integers, addition, subtraction,
  comparison, etc.
- `addrspacecast`: convert between pointers with different address spaces

```jldoctest
julia> a = [1];

julia> ptr = Core.LLVMPtr{Int,0}(pointer(a));

julia> unsafe_load(ptr)
1

julia> unsafe_store!(ptr, 42)

julia> a
1-element Vector{Int64}:
 42
```


## Intrinsics

LLVM.jl also provides Julia functions for common intrinsics, allowing them to be used in
regular Julia code. For example, the `assume` function can be used to emit `llvm.assume`
intrinsic calls:

```jldoctest
julia> max(a, b) = a < b ? b : a;

julia> @code_llvm debuginfo=:none max(1,2)
; Function Signature: max(Int64, Int64)
define i64 @julia_max_22435(i64 signext %"a::Int64", i64 signext %"b::Int64") #0 {
top:
  %"a::Int64.b::Int64" = call i64 @llvm.smax.i64(i64 %"a::Int64", i64 %"b::Int64")
  ret i64 %"a::Int64.b::Int64"
}

julia> function max(a, b)
         assume(a > b)
         a < b ? b : a
       end;

julia> @code_llvm debuginfo=:none max(1,2)
; Function Signature: max(Int64, Int64)
define i64 @julia_max_22441(i64 signext %"a::Int64", i64 signext %"b::Int64") #0 {
top:
  %0 = icmp slt i64 %"b::Int64", %"a::Int64"
  call void @llvm.assume(i1 %0)
  ret i64 %"a::Int64"
}
```

To abort execution, the `trap()` function can be used, generating a call to `@llvm.trap`.


## Optimization passes

Julia's LLVM passes are usable in the same way as LLVM's passes, and are automatically
available to any `PassBuilder`. Similarly, the default Julia optimization pipeline
can be used through the `JuliaPipeline` pipeline object.

```jldoctest
julia> run!(JuliaPipeline(), mod)
```

This object supports many keyword arguments to configure the pipeline; refer to the
API documentation for more information.
