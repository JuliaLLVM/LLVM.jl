@checked struct LLVMError
    ref::API.LLVMErrorRef
    library::Library
end
Base.unsafe_convert(::Type{API.LLVMErrorRef}, err::LLVMError) = err.ref

# wrap an error of the current library
LLVMError(ref::API.LLVMErrorRef) = LLVMError(ref, library())

argument_library(err::LLVMError) = err.library

# Consumes the error without handling it.
function consume(err::LLVMError)
    API.LLVMConsumeError(err)
end

# NOTE: handles the error, calling consume on `err` is illegal
function Base.convert(::Type{LLVMException}, err::LLVMError)
    errmsg = API.LLVMGetErrorMessage(err)
    local msg
    try
        msg = Base.unsafe_string(errmsg)
    finally
        API.LLVMDisposeErrorMessage(err.library, errmsg)
    end
    LLVMException(msg)
end

# throw an error that a function returned, which belongs to the library that the function
# called (by default, the current one)
macro check(expr)
    :(@check library() $(esc(expr)))
end
macro check(lib, expr)
    quote
        err_ref = $(esc(expr))
        if err_ref != C_NULL
            err = LLVMError(err_ref, $(esc(lib)))
            throw(convert(LLVMException, err))
        end
    end
end
