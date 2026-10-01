## typecheck: ensuring that the types of objects is as expected

const typecheck_enabled = parse(Bool, @load_preference("typecheck", "false"))


## memcheck: keeping track when objects are valid

const memcheck_enabled = parse(Bool, @load_preference("memcheck", "false"))

const tracked_objects = Dict{Any,Any}()

# Problems are reported once for every combination of the kind of problem, the type of
# object, and where in user code the object was allocated and disposed of. Later
# occurrences, e.g., uses of the object (or of other objects allocated and disposed of at
# the same locations) elsewhere, are only counted, by the location where they occur, as the
# same problem can otherwise cause a flood of reports: an update is printed when a problem
# occurred 10, 100, 1000, ... times, and all repeated problems are summarized at exit.
mutable struct MemcheckProblem
    id::Int
    count::Int
    sites::Dict{Any,Int}    # occurrences by location
end
const memcheck_lock = ReentrantLock()
const memcheck_problems = Dict{Any,MemcheckProblem}()
const memcheck_problem_keys = Any[]                     # by number

const package_dir = dirname(@__DIR__)

function frame_module(frame::Base.StackTraces.StackFrame)
    def = frame.linfo
    while def !== nothing
        def isa Method && return def.module
        def isa Core.Module && return def
        hasfield(typeof(def), :def) || return nothing
        def = getfield(def, :def)
    end
    return nothing
end

function is_internal_frame(frame::Base.StackTraces.StackFrame)
    frame.from_c && return true
    mod = frame_module(frame)
    if mod === nothing
        return startswith(string(frame.file), package_dir)
    end
    root = mod
    while parentmodule(root) !== root && root !== LLVM
        root = parentmodule(root)
    end
    return root === LLVM || root === Base || root === Core
end

# the location in user code that a backtrace points to: its first frame outside of LLVM.jl
# and Julia's Base library, identified by function and line (not by method instance, so
# that different specializations of the same code are the same location)
function user_site(bt)
    for ip in bt, frame in Base.StackTraces.lookup(ip)
        is_internal_frame(frame) && continue
        return (frame_module(frame), frame.func, frame.file, frame.line)
    end
    return nothing
end

function format_site(site)
    site === nothing && return "an unknown location"
    mod, func, file, line = site
    return "$file:$line"
end

# record an occurrence of a problem, at `site`, returning its number if it should be
# reported in full (the first time it occurs), or `nothing` otherwise, after printing an
# update if needed
function record_problem!(io, key, site=nothing)
    id, n, nsites = @lock memcheck_lock begin
        problem = get!(memcheck_problems, key) do
            push!(memcheck_problem_keys, key)
            MemcheckProblem(length(memcheck_problem_keys), 0, Dict{Any,Int}())
        end
        problem.count += 1
        problem.sites[site] = get(problem.sites, site, 0) + 1
        (problem.id, problem.count, length(problem.sites))
    end
    n == 1 && return id
    if n == 10^ndigits(n - 1)   # 10, 100, 1000, ...
        kind, T, _ = key
        print(io, "\nWARNING: memcheck problem #$id ($T $(first(problem_descriptions[kind]))) has occurred $n times",
              nsites > 1 ? ", at $nsites locations" : "", ".\n")
    end
    return nothing
end

function print_problem_footer(io, id)
    print(io, "\nThis is memcheck problem #$id. This report is representative: later occurrences for objects allocated and disposed of at the same locations are counted, and summarized at exit.\n")
end

# a description of each kind of problem, of the sites that are part of its key, and of the
# site where it occurs
const problem_descriptions = Dict(
    :overwrite => ("not disposed of before being overwritten",
                   ("allocated", "overwritten"), nothing),
    :use => ("used after being disposed of", ("allocated", "disposed of"), "used"),
    :unknown_dispose => ("unknown instance disposed of", ("disposed of",), nothing),
    :double_dispose => ("disposed of twice", ("allocated", "disposed of"),
                        "disposed of again"))

function report_repeated_problems(io)
    # problems can be reported concurrently, e.g., by ORC callbacks, so take a snapshot
    repeated = @lock memcheck_lock begin
        [(key, problem.id, problem.count, copy(problem.sites))
         for (key, problem) in ((key, memcheck_problems[key]) for key in memcheck_problem_keys)
         if problem.count > 1]
    end
    isempty(repeated) && return
    print(io, "\nWARNING: The following problems were only reported the first time:")
    for (key, id, count, sites) in repeated
        kind, T, group_sites = key
        description, labels, site_label = problem_descriptions[kind]
        print(io, "\n- #$id: $T $description, $count times: ",
              join(["$label at $(format_site(site))"
                    for (label, site) in zip(labels, group_sites)], ", "))
        if site_label !== nothing
            for (site, n) in sort!(collect(sites); by=last, rev=true)
                print(io, "\n  - $n times $site_label at $(format_site(site))")
            end
        end
    end
    println(io)
end

function mark_alloc(obj::Any; allow_overwrite::Bool=false)
    @static if memcheck_enabled
        io = Core.stdout
        new_alloc_bt = backtrace()[2:end]

        if haskey(tracked_objects, obj) && !allow_overwrite
            old_alloc_bt, dispose_bt = tracked_objects[obj]
            id = dispose_bt === nothing ?
                record_problem!(io, (:overwrite, typeof(obj),
                                     (user_site(old_alloc_bt), user_site(new_alloc_bt)))) :
                nothing
            if id !== nothing
                print(io, "\nWARNING: An instance of $(typeof(obj)) was not properly disposed of, and a new allocation will overwrite it.")
                print(io, "\nThe original allocation was at:")
                Base.show_backtrace(io, old_alloc_bt)
                print(io, "\nThe new allocation is at:")
                Base.show_backtrace(io, new_alloc_bt)
                print_problem_footer(io, id)
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
            use_bt = backtrace()[2:end]
            id = record_problem!(io, (:use, typeof(obj),
                                      (user_site(alloc_bt), user_site(dispose_bt))),
                                 user_site(use_bt))
            if id !== nothing
                print(io, "\nWARNING: An instance of $(typeof(obj)) is being used after it was disposed of.")
                print(io, "\nThe object was allocated at:")
                Base.show_backtrace(io, alloc_bt)
                print(io, "\nThe object was disposed of at:")
                Base.show_backtrace(io, dispose_bt)
                print(io, "\nThe object is being used at:")
                Base.show_backtrace(io, use_bt)
                print_problem_footer(io, id)
            end
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
            id = record_problem!(io, (:unknown_dispose, typeof(obj),
                                      (user_site(new_dispose_bt),)))
            if id !== nothing
                print(io, "\nWARNING: An unknown instance of $(typeof(obj)) is being disposed of.")
                Base.show_backtrace(io, new_dispose_bt)
                print_problem_footer(io, id)
            end
            nothing
        else
            alloc_bt, old_dispose_bt = tracked_objects[obj]
            if old_dispose_bt !== nothing
                id = record_problem!(io, (:double_dispose, typeof(obj),
                                          (user_site(alloc_bt), user_site(old_dispose_bt))),
                                     user_site(new_dispose_bt))
                if id !== nothing
                    print(io, "\nWARNING: An instance of $(typeof(obj)) is being disposed of twice.")
                    print(io, "\nThe object was allocated at:")
                    Base.show_backtrace(io, alloc_bt)
                    print(io, "\nThe object was already disposed of at:")
                    Base.show_backtrace(io, old_dispose_bt)
                    print(io, "\nThe object is being disposed of again at:")
                    Base.show_backtrace(io, new_dispose_bt)
                    print_problem_footer(io, id)
                end

                # don't dispose of the object again: that would free memory that was freed
                # already, which corrupts the heap, or makes the C library abort while it
                # holds a lock that Julia's crash handler then waits for, hanging the
                # process instead of reporting the problem.
                return
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
    @static if memcheck_enabled
        io = Core.stdout
        report_repeated_problems(io)

        # if we errored, we can't trust the memory state
        code == 0 || return

        # report leaks by the type of object and where they were allocated
        leaks = Dict{Any,Tuple{Int,Any}}()
        order = Any[]
        for (obj, (alloc_bt, dispose_bt)) in tracked_objects
            dispose_bt === nothing || continue
            key = (typeof(obj), user_site(alloc_bt))
            n, bt = get(leaks, key, (0, alloc_bt))
            n == 0 && push!(order, key)
            leaks[key] = (n + 1, bt)
        end
        for key in order
            n, alloc_bt = leaks[key]
            T = first(key)
            if n == 1
                print(io, "\nWARNING: An instance of $T was not properly disposed of.")
                print(io, "\nThe object was allocated at:")
            else
                print(io, "\nWARNING: $n instances of $T were not properly disposed of.")
                print(io, "\nThey were allocated at the same location, e.g.:")
            end
            Base.show_backtrace(io, alloc_bt)
            println(io)
        end
    end
end
