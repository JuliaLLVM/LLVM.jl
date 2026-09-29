## typecheck: ensuring that the types of objects is as expected

const typecheck_enabled = parse(Bool, @load_preference("typecheck", "false"))


## memcheck: keeping track when objects are valid

const memcheck_enabled = parse(Bool, @load_preference("memcheck", "false"))

const tracked_objects = Dict{Any,Any}()

function mark_alloc(obj::Any; allow_overwrite::Bool=false)
    @static if memcheck_enabled
        io = Core.stdout
        new_alloc_bt = backtrace()[2:end]

        if haskey(tracked_objects, obj) && !allow_overwrite
            old_alloc_bt, dispose_bt = tracked_objects[obj]
            if dispose_bt == nothing
                print(io, "\nWARNING: An instance of $(typeof(obj)) was not properly disposed of, and a new allocation will overwrite it.")
                print(io, "\nThe original allocation was at:")
                Base.show_backtrace(io, old_alloc_bt)
                print(io, "\nThe new allocation is at:")
                Base.show_backtrace(io, new_alloc_bt)
                println(io)
            end
        end

        tracked_objects[obj] = (new_alloc_bt, nothing)
    end
    return obj
end

function mark_use(obj::Any)
    @static if memcheck_enabled
        io = Core.stdout

        if !haskey(tracked_objects, obj)
            # we have to ignore unknown objects, as they may originate externally.
            # for example, a Julia-created Type we call `context` on.
            return obj
        end

        alloc_bt, dispose_bt = tracked_objects[obj]
        if dispose_bt !== nothing
            print(io, "\nWARNING: An instance of $(typeof(obj)) is being used after it was disposed of.")
            print(io, "\nThe object was allocated at:")
            Base.show_backtrace(io, alloc_bt)
            print(io, "\nThe object was disposed of at:")
            Base.show_backtrace(io, dispose_bt)
            print(io, "\nThe object is being used at:")
            Base.show_backtrace(io, backtrace()[2:end])
            println(io)
        end
    end
    return obj
end

# stop tracking an object whose lifetime is managed by something else, e.g., the context
# owned by a thread-safe context. such an object can be allocated at the address of an
# object that was disposed of earlier, which would otherwise be reported as a use after
# dispose.
function mark_untracked(obj::Any)
    @static if memcheck_enabled
        delete!(tracked_objects, obj)
    end
    return obj
end

mark_dispose(obj) = mark_dispose(Returns(nothing), obj)

function mark_dispose(f, obj)
    data = @static if memcheck_enabled
        io = Core.stdout
        new_dispose_bt = backtrace()[2:end]

        if !haskey(tracked_objects, obj)
            print(io, "\nWARNING: An unknown instance of $(typeof(obj)) is being disposed of.")
            Base.show_backtrace(io, new_dispose_bt)
            nothing
        else
            alloc_bt, old_dispose_bt = tracked_objects[obj]
            if old_dispose_bt !== nothing
                print(io, "\nWARNING: An instance of $(typeof(obj)) is being disposed of twice.")
                print(io, "\nThe object was allocated at:")
                Base.show_backtrace(io, alloc_bt)
                print(io, "\nThe object was already disposed of at:")
                Base.show_backtrace(io, old_dispose_bt)
                print(io, "\nThe object is being disposed of again at:")
                Base.show_backtrace(io, new_dispose_bt)
                println(io)
            end

            (alloc_bt, new_dispose_bt)
        end
    end
    ret = f(obj)
    @static if memcheck_enabled
        if data !== nothing
            tracked_objects[obj] = data
        end
    end
    return
end

function report_leaks(code=0)
    # if we errored, we can't trust the memory state
    if code != 0
        return
    end

    @static if memcheck_enabled
        io = Core.stdout
        for (obj, (alloc_bt, dispose_bt)) in tracked_objects
            if dispose_bt === nothing
                print(io, "\nWARNING: An instance of $(typeof(obj)) was not properly disposed of.")
                print(io, "\nThe object was allocated at:")
                Base.show_backtrace(io, alloc_bt)
                println(io)
            end
        end
    end
end
