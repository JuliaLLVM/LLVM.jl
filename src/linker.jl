export link!

"""
    link!(dst::Module, src::Module; only_needed=false, override_from_src=false)

Link the source module `src` into the destination module `dst`. The source module
is destroyed in the process.

Keyword arguments:

- `only_needed`: if true, only link symbols from `src` that are needed by `dst`
  (i.e., referenced but not defined there). This corresponds to LLVM's
  `Linker::LinkOnlyNeeded` flag.
- `override_from_src`: if true, have symbols from `src` shadow those in `dst`.
  This corresponds to LLVM's `Linker::OverrideFromSrc` flag.
"""
function link!(dst::Module, src::Module;
               only_needed::Bool=false, override_from_src::Bool=false)
    flags = UInt32(0)
    if only_needed
        flags |= UInt32(API.LLVMLinkerLinkOnlyNeeded)
    end
    if override_from_src
        flags |= UInt32(API.LLVMLinkerOverrideFromSrc)
    end

    status = API.LLVMLinkModules3(dst, src, flags) |> Bool
    @assert !status # caught by diagnostics handler
    mark_dispose(src)

    return nothing
end
