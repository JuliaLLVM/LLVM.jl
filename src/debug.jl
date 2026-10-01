## typecheck: ensuring that the types of objects is as expected

const typecheck_enabled = parse(Bool, @load_preference("typecheck", "false"))


## memcheck: keeping track when objects are valid

const memcheck_enabled = parse(Bool, @load_preference("memcheck", "false"))

# the objects that are tracked, by their wrapper, with when they were allocated and disposed
# of. an object can have an owner, another tracked object that ends its lifetime when it is
# disposed of, like a context does with the modules it contains (see `memcheck_owner`).
#
# objects are identified by `===`, so that the checker never calls `==` or `hash` methods,
# which wrapper types of other packages may implement by calling into foreign code (and
# thus back into the checker). an immutable wrapper is identified by its fields (typically
# just its handle), so that wrapping the same handle again gives the same object, and a
# mutable wrapper by the Julia object itself.
struct TrackedObject
    alloc_bt::Vector
    dispose_bt::Union{Nothing,Vector}
    # while alive: the owner, if any. once disposed of: `nothing`, or the type of the owner
    # whose disposal ended its lifetime (possibly an owner of its owner).
    owner::Any
end
const tracked_objects = IdDict{Any,TrackedObject}()

# the objects that are alive, by their owner
const owned_objects = IdDict{Any,Base.IdSet{Any}}()

# the owner of a tracked object: a tracked object whose disposal ends its lifetime, or
# `nothing`. this is only called when memcheck is enabled.
memcheck_owner(obj) = nothing

# the allocations that are being disposed of (including the objects they own), by their
# allocation backtrace, at whose address other threads may already allocate new objects
const disposing_allocations = Base.IdSet{Any}()

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
        print(io, "\nWARNING: memcheck problem #$id ($(describe_problem(key))) has occurred $n times",
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
                        "disposed of again"),
    # the key of these problems also contains the type of the owner
    :owner_use => ("used after its owner was disposed of",
                   ("allocated", "owner disposed of"), "used"),
    :owner_dispose => ("disposed of after its owner was disposed of",
                       ("allocated", "owner disposed of"), "disposed of"),
    :double_adopt => ("adopted while it was owned already", ("allocated",), "adopted"),
    # an owner that was passed explicitly, but can't own the object
    :unknown_owner => ("allocated with an owner that isn't tracked", ("allocated",), nothing),
    :dead_owner => ("allocated with an owner that was disposed of",
                    ("allocated", "owner allocated"), nothing),
    :cyclic_owner => ("allocated with an owner that it owns",
                      ("allocated", "owner allocated"), nothing))

function describe_problem(key)
    kind, T = key
    description = first(problem_descriptions[kind])
    if length(key) > 3
        description = replace(description, "its owner" => "its owning $(key[4])",
                                           "an owner" => "an owning $(key[4])")
    end
    return "$T $description"
end

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
        _, labels, site_label = problem_descriptions[kind]
        print(io, "\n- #$id: $(describe_problem(key)), $count times: ",
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

# the default `owner` of `track_alloc`, determined using `memcheck_owner`
struct DefaultOwner end

# stop tracking an object as owned by its owner
function detach_owned!(obj, entry::TrackedObject)
    entry.dispose_bt === nothing && entry.owner !== nothing || return
    objs = get(owned_objects, entry.owner, nothing)
    objs === nothing && return
    delete!(objs, obj)
    isempty(objs) && delete!(owned_objects, entry.owner)
    return
end

# stop tracking the objects that are owned by an object as being owned by anything
function release_owned!(owner)
    for obj in something(pop!(owned_objects, owner, nothing), ())
        entry = tracked_objects[obj]
        tracked_objects[obj] = TrackedObject(entry.alloc_bt, entry.dispose_bt, nothing)
    end
end

# the objects that are owned by an object, directly or indirectly, with their allocation
function owned_subtree!(subtree, owner)
    for obj in get(owned_objects, owner, ())
        push!(subtree, obj => tracked_objects[obj].alloc_bt)
        owned_subtree!(subtree, obj)
    end
    return subtree
end

# end the lifetime of the objects that are owned by an object that was disposed of
function end_owned_lifetimes!(owner, dispose_bt, owner_type=typeof(owner))
    objs = pop!(owned_objects, owner, nothing)
    objs === nothing && return
    for obj in objs
        entry = tracked_objects[obj]
        tracked_objects[obj] = TrackedObject(entry.alloc_bt, dispose_bt, owner_type)
        end_owned_lifetimes!(obj, dispose_bt, owner_type)
    end
end

# why `owner` can't own `obj`, or `nothing` if it can: it must be tracked, alive (and not
# being disposed of), and not owned by `obj` (directly or indirectly)
function owner_problem(obj, owner)
    owner === obj && return :cyclic_owner
    entry = get(tracked_objects, owner, nothing)
    entry === nothing && return :unknown_owner
    if entry.dispose_bt !== nothing || entry.alloc_bt in disposing_allocations
        return :dead_owner
    end
    # the owners of a live object are alive
    ancestor = entry.owner
    while ancestor !== nothing
        ancestor === obj && return :cyclic_owner
        ancestor = tracked_objects[ancestor].owner
    end
    return nothing
end

# start tracking an object. `owner` is another tracked object whose disposal ends the
# lifetime of `obj` (like a context does with its modules), which defaults to
# `memcheck_owner(obj)`. only owners that can own the object are recorded, and owners that
# were passed explicitly (other than `nothing`) are reported otherwise. `allow_overwrite` is for
# objects whose earlier allocation at the same address wasn't disposed of (as far as
# memcheck knows), e.g., the borrowed modules of thread-safe modules. when `adopting` an
# object that foreign code handed over, an object that is tracked as being alive at the
# same address is not overwritten, but reported (keeping what memcheck knows about it).
function track_alloc(obj::Any; allow_overwrite::Bool=false, owner=DefaultOwner(),
                     adopting::Bool=false)
    @static if memcheck_enabled
        io = Core.stdout
        new_alloc_bt = backtrace()[2:end]
        explicit_owner = !(owner isa DefaultOwner)
        if !explicit_owner
            owner = memcheck_owner(obj)
        end

        tracked, old, invalid_owner = @lock memcheck_lock begin
            old = get(tracked_objects, obj, nothing)
            # another thread may be disposing of the object at this address
            alive = old !== nothing && old.dispose_bt === nothing &&
                    !(old.alloc_bt in disposing_allocations)
            invalid_owner = nothing
            if adopting && alive
                return_value = (false, old, invalid_owner)
            else
                # check the owner before forgetting what an earlier object at this address
                # owned, which can be the reason why it can't own this object
                if owner !== nothing
                    problem = owner_problem(obj, owner)
                    if problem !== nothing
                        if explicit_owner
                            invalid_owner = (problem, owner,
                                             get(tracked_objects, owner, nothing))
                        end
                        owner = nothing
                    end
                end

                if old !== nothing
                    detach_owned!(obj, old)
                    # the objects owned by an earlier object at this address (which
                    # memcheck didn't see being disposed of) don't belong to the new one
                    release_owned!(obj)
                end

                tracked_objects[obj] = TrackedObject(new_alloc_bt, nothing, owner)
                owner === nothing || push!(get!(Base.IdSet{Any}, owned_objects, owner), obj)
                return_value = (true, alive ? old : nothing, invalid_owner)
            end
            return_value
        end

        if invalid_owner !== nothing
            report_invalid_owner(io, obj, invalid_owner..., new_alloc_bt)
        end

        if !tracked
            id = record_problem!(io, (:double_adopt, typeof(obj),
                                      (user_site(old.alloc_bt),)),
                                 user_site(new_alloc_bt))
            if id !== nothing
                print(io, "\nWARNING: An instance of $(typeof(obj)) is being adopted, but it is owned already: it was allocated or adopted before, and hasn't been disposed of.")
                print(io, "\nThe object was allocated at:")
                Base.show_backtrace(io, old.alloc_bt)
                print(io, "\nThe object is being adopted at:")
                Base.show_backtrace(io, new_alloc_bt)
                print_problem_footer(io, id)
            end
        elseif old !== nothing && !allow_overwrite
            id = record_problem!(io, (:overwrite, typeof(obj),
                                      (user_site(old.alloc_bt), user_site(new_alloc_bt))))
            if id !== nothing
                print(io, "\nWARNING: An instance of $(typeof(obj)) was not properly disposed of, and a new allocation will overwrite it.")
                print(io, "\nThe original allocation was at:")
                Base.show_backtrace(io, old.alloc_bt)
                print(io, "\nThe new allocation is at:")
                Base.show_backtrace(io, new_alloc_bt)
                print_problem_footer(io, id)
            end
        end
    end
    return obj
end

function report_invalid_owner(io, obj, problem, owner, owner_entry, alloc_bt)
    T = typeof(obj)
    O = typeof(owner)
    owner_site = owner_entry === nothing ? () : (user_site(owner_entry.alloc_bt),)
    id = record_problem!(io, (problem, T, (user_site(alloc_bt), owner_site...), O))
    id === nothing && return
    reason = if problem === :unknown_owner
        "that isn't tracked (it wasn't allocated using `LLVM.mark_alloc`, or was untracked)"
    elseif problem === :dead_owner
        owner_entry.dispose_bt === nothing ? "that is being disposed of" :
                                             "that was disposed of"
    else
        "that it owns already (directly or indirectly)"
    end
    print(io, "\nWARNING: An instance of $T is being allocated with an owning $O $reason, so it is tracked without an owner.")
    if owner_entry !== nothing
        print(io, "\nThe owner was allocated at:")
        Base.show_backtrace(io, owner_entry.alloc_bt)
        if owner_entry.dispose_bt !== nothing
            print(io, "\nThe owner was disposed of at:")
            Base.show_backtrace(io, owner_entry.dispose_bt)
        end
    end
    print(io, "\nThe object is being allocated at:")
    Base.show_backtrace(io, alloc_bt)
    print_problem_footer(io, id)
end

@public mark_alloc, mark_use, mark_dispose, mark_untracked

"""
    LLVM.mark_alloc(obj; owner=nothing) -> obj

Register `obj` as a newly allocated object with the `memcheck` debugging mode, which then
reports using it after it was disposed of (see [`LLVM.mark_use`](@ref)), disposing of it
twice (see [`LLVM.mark_dispose`](@ref)), and not disposing of it at all (when the process
exits). This is meant for packages that wrap a C API in wrapper types of their own, to
check these like LLVM.jl's objects (see [Checking other wrapper types](@ref)). Register the
wrapper that owns a resource when creating it:

```julia
Thing() = LLVM.mark_alloc(Thing(API.thing_create()))
```

Objects are identified by `===`, not by `==` or `hash`: wrapping the same handle in an
immutable wrapper type again gives the same object (if its other fields, if any, are `===`
too), while a mutable wrapper is identified by the Julia object itself, and wrappers of
different types are different objects. Registering an allocation of an object that is still
alive is reported, as it wasn't disposed of; when the address of an object that was
disposed of is reused, wrappers of the earlier object become indistinguishable from the
new one.

`owner` is another tracked object whose disposal ends the lifetime of `obj`, like a context
does with the modules in it: using or disposing of `obj` after its owner was disposed of is
reported, and `obj` isn't reported as leaked once its owner was disposed of. This is only
bookkeeping, which doesn't extend the lifetime of the owner's resource, or transfer
ownership in the foreign library. The owner must be tracked and alive, and not be owned by
`obj`; otherwise, the problem is reported, and `obj` is tracked without an owner.

When memcheck is disabled, this only returns `obj` (although the arguments are still
evaluated). When it is enabled, it keeps the wrappers that it tracks reachable, also after
they have been disposed of, so garbage collection doesn't finalize mutable wrappers.
"""
mark_alloc(obj::Any; owner=nothing) = track_alloc(obj; owner)


"""
    LLVM.mark_use(obj) -> obj

Check that `obj`, as registered using [`LLVM.mark_alloc`](@ref), hasn't been disposed of,
and that its owner hasn't been disposed of either, reporting the problem otherwise. Objects
that aren't tracked, e.g., wrappers of handles that the foreign library lends out, aren't
checked. Typically, this is used when a wrapper is converted to its handle:

```julia
Base.unsafe_convert(::Type{API.ThingRef}, t::Thing) = LLVM.mark_use(t).ref
```

This only reports the problem: using the object can still crash the process afterwards.
When memcheck is disabled, this only returns `obj`.
"""
function mark_use(obj::Any)
    @static if memcheck_enabled
        io = Core.stdout

        entry = @lock memcheck_lock get(tracked_objects, obj, nothing)
        if entry === nothing
            # we have to ignore unknown objects, as they may originate externally.
            # for example, a Julia-created Type we call `context` on.
            return obj
        end

        if entry.dispose_bt !== nothing
            use_bt = backtrace()[2:end]
            sites = (user_site(entry.alloc_bt), user_site(entry.dispose_bt))
            if entry.owner === nothing
                id = record_problem!(io, (:use, typeof(obj), sites), user_site(use_bt))
                id === nothing && return obj
                print(io, "\nWARNING: An instance of $(typeof(obj)) is being used after it was disposed of.")
                print(io, "\nThe object was allocated at:")
                Base.show_backtrace(io, entry.alloc_bt)
                print(io, "\nThe object was disposed of at:")
            else
                id = record_problem!(io, (:owner_use, typeof(obj), sites, entry.owner),
                                     user_site(use_bt))
                id === nothing && return obj
                print(io, "\nWARNING: An instance of $(typeof(obj)) is being used after the $(entry.owner) that owns it was disposed of.")
                print(io, "\nThe object was allocated at:")
                Base.show_backtrace(io, entry.alloc_bt)
                print(io, "\nThe owner was disposed of at:")
            end
            Base.show_backtrace(io, entry.dispose_bt)
            print(io, "\nThe object is being used at:")
            Base.show_backtrace(io, use_bt)
            print_problem_footer(io, id)
        end
    end
    return obj
end

# stop tracking an object whose lifetime is managed by something else, e.g., the context
# owned by a thread-safe context, or a module borrowed from a thread-safe module. such an
# object can be allocated at the address of an object that was disposed of earlier, which
# would otherwise be reported as a use after dispose. objects that it owns are not owned by
# anything anymore.
"""
    LLVM.mark_untracked(obj) -> obj

Stop tracking `obj` with the `memcheck` debugging mode, without disposing of it: it isn't
checked anymore, or reported as leaked. Use this when a tracked object is handed over to
the foreign library, which keeps it alive (and disposes of it), after the operation that
takes ownership succeeded:

```julia
function Base.push!(c::Container, t::Thing)
    API.container_append_owned(c, t)
    LLVM.mark_untracked(t)
    return c
end
```

If the object can't be used anymore after handing it over, e.g., because the operation
destroys it, use [`LLVM.mark_dispose`](@ref) instead
(`LLVM.mark_dispose(t -> API.container_consume(c, t), t)`), so that later uses are
reported. This can also be used for wrappers of handles that the foreign library lends
out, which might otherwise be mistaken for an object that was disposed of at the same
address.

The objects that `obj` owned stay tracked without an owner, so they are reported as leaked
unless they are disposed of (while the objects that they own keep their owner). Memcheck
doesn't follow the object to its new owner, and doesn't relate other wrappers of the same
resource to it: a wrapper of another type, e.g., one that views the resource differently, is
a different object, that isn't tracked unless it is registered itself.

When memcheck is disabled, this only returns `obj`.
"""
function mark_untracked(obj::Any)
    @static if memcheck_enabled
        @lock memcheck_lock begin
            entry = get(tracked_objects, obj, nothing)
            if entry !== nothing
                detach_owned!(obj, entry)
                delete!(tracked_objects, obj)
            end
            release_owned!(obj)
        end
    end
    return obj
end

# start tracking an object that foreign code handed over (see `adopt`)
mark_adopt(obj::Any) = track_alloc(obj; adopting=true)

# record that an object was disposed of, e.g., by an operation that consumed it
mark_disposed(obj) = mark_dispose(Returns(nothing), obj)

function done_disposing!(entry, owned)
    delete!(disposing_allocations, entry.alloc_bt)
    for (_, alloc_bt) in owned
        delete!(disposing_allocations, alloc_bt)
    end
end

"""
    LLVM.mark_dispose(f, obj) -> nothing

Dispose of `obj` by calling `f(obj)` (discarding what it returns), and record its disposal
with the `memcheck` debugging mode:

```julia
dispose(t::Thing) = LLVM.mark_dispose(API.thing_destroy, t)
```

- When `obj` was disposed of already, or its owner was, the problem is reported, and `f` is
  not called, as freeing its memory again would crash the process or corrupt the heap. This
  only applies to disposals that have been recorded: it doesn't synchronize concurrent (or
  recursive) disposal of the same object.
- When `obj` isn't tracked (see [`LLVM.mark_alloc`](@ref)), its disposal is reported, and
  `f` is called nonetheless.
- When `f` returns, `obj` is recorded as disposed of, which ends the lifetime of the objects
  that it owns. When `f` throws, the exception is rethrown without recording the disposal
  (even though `f` may have freed the object already).

When memcheck is disabled, this only calls `f(obj)`.
"""
function mark_dispose(f, obj)
    entry = @static if memcheck_enabled
        io = Core.stdout
        new_dispose_bt = backtrace()[2:end]

        entry, owned = @lock memcheck_lock begin
            entry = get(tracked_objects, obj, nothing)
            owned = owned_subtree!(Pair{Any,Vector}[], obj)
            if entry !== nothing && entry.dispose_bt === nothing
                push!(disposing_allocations, entry.alloc_bt)
                for (_, alloc_bt) in owned
                    push!(disposing_allocations, alloc_bt)
                end
            end
            entry, owned
        end
        if entry === nothing
            id = record_problem!(io, (:unknown_dispose, typeof(obj),
                                      (user_site(new_dispose_bt),)))
            if id !== nothing
                print(io, "\nWARNING: An unknown instance of $(typeof(obj)) is being disposed of.")
                Base.show_backtrace(io, new_dispose_bt)
                print_problem_footer(io, id)
            end
        elseif entry.dispose_bt !== nothing
            sites = (user_site(entry.alloc_bt), user_site(entry.dispose_bt))
            if entry.owner === nothing
                id = record_problem!(io, (:double_dispose, typeof(obj), sites),
                                     user_site(new_dispose_bt))
                if id !== nothing
                    print(io, "\nWARNING: An instance of $(typeof(obj)) is being disposed of twice.")
                    print(io, "\nThe object was allocated at:")
                    Base.show_backtrace(io, entry.alloc_bt)
                    print(io, "\nThe object was already disposed of at:")
                    Base.show_backtrace(io, entry.dispose_bt)
                    print(io, "\nThe object is being disposed of again at:")
                    Base.show_backtrace(io, new_dispose_bt)
                    print_problem_footer(io, id)
                end
            else
                id = record_problem!(io, (:owner_dispose, typeof(obj), sites, entry.owner),
                                     user_site(new_dispose_bt))
                if id !== nothing
                    print(io, "\nWARNING: An instance of $(typeof(obj)) is being disposed of after the $(entry.owner) that owns it was disposed of, which ended its lifetime, so it is not disposed of again.")
                    print(io, "\nThe object was allocated at:")
                    Base.show_backtrace(io, entry.alloc_bt)
                    print(io, "\nThe owner was disposed of at:")
                    Base.show_backtrace(io, entry.dispose_bt)
                    print(io, "\nThe object is being disposed of at:")
                    Base.show_backtrace(io, new_dispose_bt)
                    print_problem_footer(io, id)
                end
            end

            # don't dispose of the object again: that would free memory that was freed
            # already, which corrupts the heap, or makes the C library abort while it
            # holds a lock that Julia's crash handler then waits for, hanging the
            # process instead of reporting the problem.
            return
        end
        entry
    end
    @static if memcheck_enabled
        try
            f(obj)
        catch
            entry === nothing || @lock memcheck_lock done_disposing!(entry, owned)
            rethrow()
        end

        # the object is only recorded as disposed of afterwards, as `f` uses it. by then,
        # another thread may have allocated a new object at the same address, so only
        # record the disposal if the object is still the one we disposed of (its entry may
        # have changed, e.g., when its owner was untracked).
        if entry !== nothing
            @lock memcheck_lock begin
                done_disposing!(entry, owned)
                current = get(tracked_objects, obj, nothing)
                if current !== nothing && current.alloc_bt === entry.alloc_bt &&
                   current.dispose_bt === nothing
                    detach_owned!(obj, current)
                    tracked_objects[obj] = TrackedObject(entry.alloc_bt, new_dispose_bt, nothing)
                    end_owned_lifetimes!(obj, new_dispose_bt)
                else
                    # the new object doesn't own the objects that the disposed one did, but
                    # their lifetime ended nonetheless (unless they were reallocated too)
                    for (owned_obj, alloc_bt) in owned
                        owned_entry = get(tracked_objects, owned_obj, nothing)
                        owned_entry !== nothing && owned_entry.alloc_bt === alloc_bt &&
                            owned_entry.dispose_bt === nothing || continue
                        detach_owned!(owned_obj, owned_entry)
                        pop!(owned_objects, owned_obj, nothing)
                        tracked_objects[owned_obj] =
                            TrackedObject(alloc_bt, new_dispose_bt, typeof(obj))
                    end
                end
            end
        end
    else
        f(obj)
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
        objects = @lock memcheck_lock collect(tracked_objects)
        for (obj, entry) in objects
            entry.dispose_bt === nothing || continue
            key = (typeof(obj), user_site(entry.alloc_bt))
            n, bt = get(leaks, key, (0, entry.alloc_bt))
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
