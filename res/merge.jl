# merge the wrappers generated for every LLVM version into a single set of wrappers
#
# `res/wrap.jl` generates `libLLVM.jl` and `libLLVM_extra.jl` for every supported version of
# LLVM. This script merges them into one `lib/libLLVM.jl` and `lib/libLLVM_extra.jl`, where
# every definition appears once and is valid for every version:
#
# - functions that only exist on some versions check the version before calling LLVM;
# - functions whose signature changed get a method per signature (which must differ in
#   their number of arguments);
# - functions that LLVMExtra provides on some versions, and LLVM itself on others, call the
#   library that provides them on the version in use;
# - enums contain the members of every version. Their values are those of the most recent
#   version of LLVM when they are first generated, and stay the same afterwards (new members
#   get new values), so the values that the previous wrappers defined are read back. When
#   the values of the C API differ, or members are not available on every version, a method
#   of `abi_tables` translates between them, and the wrappers taking or returning such enums
#   translate their values;
# - all other definitions (types and constants) must be the same on every version.
#
# It also writes `res/ABI.md`, listing how the C APIs of the different versions differ.

struct Item
    kind::Symbol            # :function, :enum, :struct, :const or :prologue
    key::String             # identifies the item across versions
    name::Symbol
    file::Symbol            # :llvm or :extra
    text::String            # the source code of the item, including documentation
    doc::String             # the docstring, including quotes (or empty)
    comment::String         # comments preceding the item (or empty)
    def::Any                # the definition, without documentation or line numbers
end

const ANONYMOUS_ENUM = r"^(##Ctag#|__JL_Ctag_)"

isanonymous(name::Symbol) = occursin(ANONYMOUS_ENUM, String(name))

strip_lines(ex) = Base.remove_linenums!(deepcopy(ex))

function parse_items(path, file)
    src = read(path, String)
    lines = split(src, '\n')
    top = Meta.parseall(src; filename=path)

    entries = Tuple{Int,Any}[]
    line = 0
    for ex in top.args
        if ex isa LineNumberNode
            line = ex.line
        else
            push!(entries, (line, ex))
        end
    end

    # the source lines of each entry, with trailing comments moved to the next entry
    ranges = map(enumerate(entries)) do (i, (line, _))
        stop = i < length(entries) ? entries[i+1][1] - 1 : length(lines)
        line:stop
    end
    texts = [String.(lines[r]) for r in ranges]
    comments = [String[] for _ in texts]
    for i in 1:length(texts)-1
        t = texts[i]
        while !isempty(t) && (isempty(strip(t[end])) || startswith(t[end], "#"))
            l = pop!(t)
            isempty(strip(l)) || pushfirst!(comments[i+1], l)
        end
    end

    items = Item[]
    for (i, (_, ex)) in enumerate(entries)
        text = strip(join(texts[i], '\n'))
        doc = ""
        def = ex
        if Meta.isexpr(ex, :macrocall) &&
           (ex.args[1] == GlobalRef(Core, Symbol("@doc")) || ex.args[1] == Symbol("@doc"))
            def = ex.args[end]
            # the docstring is everything up to the closing quotes
            tlines = split(text, '\n')
            @assert tlines[1] == "\"\"\""
            close = findnext(==("\"\"\""), tlines, 2)
            doc = join(tlines[1:close], '\n')
        end
        def = strip_lines(def)
        comment = join(comments[i], '\n')
        isempty(comment) || (text = comment * "\n" * text)

        if Meta.isexpr(def, :using)
            continue
        elseif Meta.isexpr(def, :function)
            name = def.args[1].args[1]::Symbol
            push!(items, Item(:function, String(name), name, file, text, doc, comment, def))
        elseif Meta.isexpr(def, :macrocall) && def.args[1] == Symbol("@cenum")
            name = def.args[3].args[1]
            name = name isa Symbol ? name : Symbol(name)
            members = def.args[end].args
            # anonymous enums get a different name on every version: identify by content
            key = isanonymous(name) ? "<anonymous: $(members[1].args[1])>" : String(name)
            push!(items, Item(:enum, key, name, file, text, doc, comment, def))
        elseif Meta.isexpr(def, :struct)
            name = def.args[2]::Symbol
            push!(items, Item(:struct, String(name), name, file, text, doc, comment, def))
        elseif Meta.isexpr(def, :const)
            name = def.args[1].args[1]::Symbol
            push!(items, Item(:const, String(name), name, file, text, doc, comment, def))
        elseif Meta.isexpr(def, :if)
            push!(items, Item(:prologue, "<prologue: if>", Symbol(), file, text, doc, comment, def))
        else
            error("Unsupported definition in $path:\n$text")
        end
    end
    return items
end


## versions

# a set of versions as a compact string, e.g. "15–17, 19+"
function format_versions(versions, all)
    versions = sort(collect(versions))
    isempty(versions) && return "none"
    versions == all && return "all"
    parts = String[]
    for r in ranges(versions)
        if last(r) == last(all)
            push!(parts, "$(first(r))+")
        elseif first(r) == last(r)
            push!(parts, "$(first(r))")
        else
            push!(parts, "$(first(r))–$(last(r))")
        end
    end
    join(parts, ", ")
end

# contiguous ranges of a sorted set of versions
function ranges(versions)
    rs = UnitRange{Int}[]
    for v in sort(collect(versions))
        if !isempty(rs) && last(rs[end]) + 1 == v
            rs[end] = first(rs[end]):v
        else
            push!(rs, v:v)
        end
    end
    rs
end


## functions

struct CallInfo
    lib::Symbol
    ret::Any
    types::Vector{Any}
    argnames::Vector{Any}
end

function call_info(item::Item)
    def = item.def
    argnames = def.args[1].args[2:end]
    body = def.args[2]
    @assert Meta.isexpr(body, :block) && length(body.args) == 1
    call = body.args[1]
    @assert Meta.isexpr(call, :call) && call.args[1] == :ccall
    target = call.args[2]
    @assert Meta.isexpr(target, :tuple) && target.args[1] == QuoteNode(item.name)
    types = call.args[4]
    @assert Meta.isexpr(types, :tuple)
    @assert call.args[5:end] == argnames
    CallInfo(target.args[2], call.args[3], types.args, argnames)
end

signature(c::CallInfo) = (c.ret, c.types)

contains_type(ex, T::Symbol) = ex === T || (ex isa Expr && any(a -> contains_type(a, T), ex.args))


## enums

enum_type(item::Item) = item.def.args[3].args[2]

function enum_members(item::Item)
    map(item.def.args[end].args) do m
        @assert Meta.isexpr(m, :(=))
        m.args[1]::Symbol => Int(Core.eval(Main, m.args[2]))
    end
end

struct EnumInfo
    key::String
    name::Symbol
    members::Vector{Pair{Symbol,Int}}           # canonical values, in order of definition
    raw::Dict{Int,Dict{Symbol,Int}}             # the values of the C API, per version
    tables::Dict{Int,Any}                       # translation tables, per version
    differs::Bool                               # whether some C API values differ
end

# translation tables for a version: `nothing` if the C API has every member with the same
# value, or the C API value of every canonical value (`to`) and vice versa (`from`), with
# -1 marking members that are not available. Values that are not members of the enum are
# passed as-is (e.g., calling conventions that the C API does not name).
function translation_tables(members, raw)
    if all(((m, v),) -> get(raw, m, nothing) == v, members)
        return nothing
    end
    maxc = maximum(last, members)
    maxr = isempty(raw) ? -1 : maximum(values(raw))
    minimum(last, members) >= 0 && maxc < 1024 &&
        (isempty(raw) || (minimum(values(raw)) >= 0 && maxr < 1024)) ||
        error("cannot generate translation tables for values outside of 0:1023")
    to = collect(0:maxc)
    from = collect(0:maxr)
    for (m, c) in members
        to[c+1] = get(raw, m, -1)
    end
    for (m, c) in reverse(members)
        haskey(raw, m) && (from[raw[m]+1] = c)
    end
    return (; to=Tuple(to), from=Tuple(from))
end

function merge_enum(key, per_version::Dict{Int,Item}, baseline, versions)
    newest = per_version[maximum(keys(per_version))]
    raw = Dict(v => Dict(enum_members(per_version[v])) for v in keys(per_version))
    for v in versions
        haskey(raw, v) || (raw[v] = Dict{Symbol,Int}())
    end

    # canonical values: those of the previous wrappers, then those of the most recent
    # version that has the member, unless the value is in use already (except by an alias)
    canonical = Pair{Symbol,Int}[]
    if baseline !== nothing
        for (m, c) in enum_members(baseline)
            any(v -> haskey(raw[v], m), versions) && push!(canonical, m => c)
        end
    end
    for v in sort(collect(keys(per_version)); rev=true), (m, r) in enum_members(per_version[v])
        any(p -> first(p) == m, canonical) && continue
        users = [first(p) for p in canonical if last(p) == r]
        if all(u -> get(raw[v], u, nothing) == r, users)
            push!(canonical, m => r)
        else
            push!(canonical, m => maximum(last, canonical) + 1)
        end
    end

    # aliases (members with the same value) would make the translation ambiguous
    if any(v -> translation_tables(canonical, raw[v]) !== nothing, versions)
        allunique(last.(canonical)) ||
            error("$key has aliases and needs translation tables, which is not supported")
        for v in versions
            allunique(values(raw[v])) ||
                error("$key has aliases on LLVM $v and needs translation tables, which is not supported")
        end
    end

    tables = Dict{Int,Any}()
    differs = false
    for v in versions
        tables[v] = translation_tables(canonical, raw[v])
        for (m, c) in canonical
            r = get(raw[v], m, nothing)
            r === nothing || r == c || (differs = true)
        end
    end
    return EnumInfo(key, newest.name, canonical, raw, tables, differs)
end

# whether the enum's values need to be checked or translated
needs_tables(e::EnumInfo) = any(!isnothing, values(e.tables))

function format_enum(item::Item, e::EnumInfo)
    io = IOBuffer()
    isempty(item.comment) || println(io, item.comment)
    isempty(item.doc) || println(io, item.doc)
    println(io, "@cenum ", item.def.args[3], " begin")
    for (m, c) in e.members
        println(io, "    ", m, " = ", c)
    end
    print(io, "end")
    String(take!(io))
end

# the tables are global constants, as indexing a tuple that is not would copy it first
function format_tables(e::EnumInfo, versions)
    io = IOBuffer()
    # group the versions with identical tables, from the most recent one
    groups = Tuple{Int,Any}[]
    for v in sort(versions; rev=true)
        if !isempty(groups) && groups[end][2] == e.tables[v]
            groups[end] = (v, e.tables[v])
        else
            push!(groups, (v, e.tables[v]))
        end
    end
    table_name(lo) = "$(e.name)_abi_$lo"
    for (lo, t) in groups
        t === nothing && continue
        println(io, "const ", table_name(lo), " = (to = ", t.to, ",")
        println(io, "    from = ", t.from, ")")
        println(io)
    end
    println(io, "function abi_tables(::Type{", e.name, "}, major::Integer)")
    for (i, (lo, t)) in enumerate(groups)
        if i == 1
            println(io, "    if major >= ", lo)
        elseif i == length(groups)
            println(io, "    else")
        else
            println(io, "    elseif major >= ", lo)
        end
        println(io, "        ", t === nothing ? "nothing" : table_name(lo))
    end
    length(groups) == 1 && println(io, "    else\n        nothing")
    println(io, "    end")
    print(io, "end")
    String(take!(io))
end


## merging

struct Merged
    versions::Vector{Int}
    order::Vector{String}
    items::Dict{String,Dict{Int,Item}}
    enums::Dict{String,EnumInfo}
end

function merge_versions(dir, versions, baseline_dir)
    per_version = Dict(v => vcat(parse_items(joinpath(dir, string(v), "libLLVM.jl"), :llvm),
                                 parse_items(joinpath(dir, string(v), "libLLVM_extra.jl"), :extra))
                       for v in versions)

    # the order of definitions: that of the most recent version, with the definitions of
    # older versions inserted after the definition that precedes them there
    order = String[]
    for v in sort(versions; rev=true)
        prev = 0
        for item in per_version[v]
            idx = findfirst(==(item.key), order)
            if idx === nothing
                insert!(order, prev + 1, item.key)
                prev += 1
            else
                prev = idx
            end
        end
    end

    items = Dict{String,Dict{Int,Item}}()
    for v in versions, item in per_version[v]
        d = get!(items, item.key, Dict{Int,Item}())
        haskey(d, v) && error("$(item.key) is defined twice on LLVM $v")
        d[v] = item
    end
    for (key, d) in items
        kinds = unique(item.kind for item in values(d))
        length(kinds) == 1 || error("$key is a $(join(kinds, " and a ")) on different versions")
    end

    baseline = Dict{String,Item}()
    for file in ("libLLVM.jl", "libLLVM_extra.jl")
        path = joinpath(baseline_dir, file)
        isfile(path) || continue
        for item in parse_items(path, :llvm)
            item.kind == :enum && (baseline[item.key] = item)
        end
    end

    enums = Dict{String,EnumInfo}()
    for (key, d) in items
        first(values(d)).kind == :enum || continue
        enums[key] = merge_enum(key, d, get(baseline, key, nothing), versions)
    end

    return Merged(versions, order, items, enums)
end

newest_item(d::Dict{Int,Item}) = d[maximum(keys(d))]

function format_function(m::Merged, key, d::Dict{Int,Item})
    infos = Dict(v => call_info(item) for (v, item) in d)
    name = newest_item(d).name
    versions = m.versions

    # the enums whose values are checked or translated
    checked = Dict(e.name => e for e in values(m.enums) if needs_tables(e))

    variants = Dict{Any,Vector{Int}}()
    for (v, info) in infos
        push!(get!(variants, signature(info), Int[]), v)
    end
    arities = [length(sig[2]) for sig in keys(variants)]
    allunique(arities) ||
        error("$name has different signatures with the same number of arguments")
    simple = length(variants) == 1 && sort(collect(keys(d))) == versions &&
             length(unique(info.lib for info in values(infos))) == 1 &&
             !any(T -> contains_type(newest_item(d).def, T), keys(checked))
    simple && return newest_item(d).text

    io = IOBuffer()
    item = newest_item(d)
    isempty(item.comment) || println(io, item.comment)
    sorted = sort(collect(variants); by=p -> maximum(p[2]), rev=true)
    for (i, (sig, vs)) in enumerate(sorted)
        i == 1 || print(io, "\n\n")
        info = infos[maximum(vs)]
        i == 1 && !isempty(item.doc) && println(io, item.doc)
        println(io, "function ", name, "(", join(info.argnames, ", "), ")")

        rs = ranges(vs)
        length(rs) == 1 || error("$name is not available on a contiguous range of versions")
        r = only(rs)
        if r != first(versions):last(versions)
            if last(r) == last(versions)
                println(io, "    check_available(:", name, ", ", first(r), ")")
            else
                println(io, "    check_available(:", name, ", ", first(r), ", ", last(r) + 1, ")")
            end
        end

        # convert the arguments and return value of enums that need to be translated
        args = map(zip(info.types, info.argnames)) do (T, a)
            if T isa Symbol && haskey(checked, T)
                "to_abi($T, $a)"
            else
                for E in keys(checked)
                    contains_type(T, E) && error("$name passes $E indirectly, which cannot be translated")
                end
                string(a)
            end
        end
        ret = info.ret
        if !(ret isa Symbol && haskey(checked, ret))
            for E in keys(checked)
                contains_type(ret, E) && error("$name returns $E indirectly, which cannot be translated")
            end
        end
        translate_ret = ret isa Symbol && haskey(checked, ret) && checked[ret].differs

        function ccall_str(lib)
            types = Expr(:tuple, info.types...)
            s = "ccall((:$name, $lib), $ret, $types" * join(", " .* args) * ")"
            translate_ret ? "from_abi($ret, $s)" : s
        end

        # the library that provides the function, per range of versions
        libs = Tuple{UnitRange{Int},Symbol}[]
        for v in sort(vs)
            lib = infos[v].lib
            if !isempty(libs) && libs[end][2] == lib
                libs[end] = (first(libs[end][1]):v, lib)
            else
                push!(libs, (v:v, lib))
            end
        end
        if length(libs) == 1
            println(io, "    ", ccall_str(only(libs)[2]))
        else
            reverse!(libs)
            for (j, (lr, lib)) in enumerate(libs)
                if j == 1
                    println(io, "    if version().major >= ", first(lr))
                elseif j == length(libs)
                    println(io, "    else")
                else
                    println(io, "    elseif version().major >= ", first(lr))
                end
                println(io, "        ", ccall_str(lib))
            end
            println(io, "    end")
        end
        print(io, "end")
    end
    String(take!(io))
end

function format_item(m::Merged, key)
    d = m.items[key]
    item = newest_item(d)
    if item.kind == :function
        return format_function(m, key, d)
    elseif item.kind == :enum
        e = m.enums[key]
        text = if all(t -> t === nothing, values(e.tables)) && enum_members(item) == e.members
            item.text
        else
            format_enum(item, e)
        end
        if needs_tables(e)
            text *= "\n\n" * format_tables(e, m.versions)
        end
        return text
    else
        defs = unique(i.def for i in values(d))
        length(defs) == 1 ||
            error("$key differs between versions:\n" * join(string.(defs), "\n"))
        return item.text
    end
end

function file_of(m::Merged, key)
    newest_item(m.items[key]).file
end

const HEADER = """
    # Automatically generated by res/wrap.jl from the C API of LLVM %VERSIONS%.
    # Do not edit; see res/merge.jl for how the APIs of different versions are combined.

    using CEnum: CEnum, @cenum
    """

function write_wrappers(m::Merged, libdir)
    names = Dict{Symbol,String}()
    for key in m.order
        name = newest_item(m.items[key]).name
        name == Symbol() && continue
        prev = get(names, name, nothing)
        prev === nothing || error("$key and $prev are both defined as $name")
        names[name] = key
    end

    header = replace(HEADER, "%VERSIONS%" => "$(first(m.versions)) to $(last(m.versions))")
    for (file, path) in ((:llvm, "libLLVM.jl"), (:extra, "libLLVM_extra.jl"))
        open(joinpath(libdir, path), "w") do io
            println(io, header)
            if file == :llvm
                println(io, "# the versions of LLVM that these wrappers were generated for")
                println(io, "const WRAPPED_VERSIONS = $(first(m.versions)):$(last(m.versions))")
                println(io)
            end
            for key in m.order
                file_of(m, key) == file || continue
                println(io, format_item(m, key))
                println(io)
            end
        end
    end
end


## inventory

function write_inventory(m::Merged, path; provenance)
    versions = m.versions
    fmt(vs) = format_versions(vs, versions)
    io = IOBuffer()
    println(io, """
        # Differences between the C APIs of LLVM $(first(versions)) to $(last(versions))

        Generated by `res/wrap.jl` (see `res/merge.jl`); do not edit. This lists how the C API
        of LLVM, and the API of LLVMExtra, differ between the versions of LLVM that LLVM.jl
        supports, and how the wrappers in `lib/` deal with that. Version ranges ending in `+`
        include the most recent version.
        """)

    println(io, "## Sources\n")
    println(io, "| Version | Headers |")
    println(io, "|:--------|:--------|")
    for v in versions
        println(io, "| ", v, " | ", get(provenance, v, "unknown"), " |")
    end
    println(io)

    function counts(kind, file)
        [count(d -> haskey(d, v) && d[v].kind == kind && d[v].file == file, values(m.items))
         for v in versions]
    end
    println(io, "| Definitions | ", join(versions, " | "), " |")
    println(io, "|:------------|", repeat("--:|", length(versions)))
    for (kind, label) in ((:function, "functions"), (:enum, "enums"), (:struct, "structs"),
                          (:const, "constants"))
        for (file, lib) in ((:llvm, "LLVM"), (:extra, "LLVMExtra"))
            println(io, "| ", lib, " ", label, " | ", join(counts(kind, file), " | "), " |")
        end
    end
    println(io)

    keys_of(kind) = sort([k for (k, d) in m.items if first(values(d)).kind == kind])

    # functions
    partial = String[]
    routed = String[]
    variants = String[]
    for key in keys_of(:function)
        d = m.items[key]
        vs = sort(collect(keys(d)))
        infos = Dict(v => call_info(item) for (v, item) in d)
        vs == versions || push!(partial, "| `$key` | $(fmt(vs)) |")
        libs = Dict{Symbol,Vector{Int}}()
        for (v, info) in infos
            push!(get!(libs, info.lib, Int[]), v)
        end
        if length(libs) > 1
            push!(routed, "| `$key` | " *
                  join(["`$lib` ($(fmt(vs)))" for (lib, vs) in sort(collect(libs); by=p->minimum(p[2]))], ", ") * " |")
        end
        sigs = Dict{Any,Vector{Int}}()
        for (v, info) in infos
            push!(get!(sigs, signature(info), Int[]), v)
        end
        if length(sigs) > 1
            for (sig, vs) in sort(collect(sigs); by=p->minimum(p[2]))
                push!(variants, "| `$key` | $(fmt(vs)) | `$(sig[1])` | `$(Expr(:tuple, sig[2]...))` |")
            end
        end
    end
    println(io, """
        ## Functions

        ### Functions that are not available on every version

        Calling these on another version of LLVM throws an error.

        | Function | Versions |
        |:---------|:---------|""")
    foreach(l -> println(io, l), partial)
    println(io, """

        ### Functions provided by different libraries

        LLVMExtra provides some functions of the C API on versions of LLVM that lack them.

        | Function | Libraries |
        |:---------|:----------|""")
    foreach(l -> println(io, l), routed)
    println(io, """

        ### Functions whose signature differs

        These have a method per signature.

        | Function | Versions | Returns | Arguments |
        |:---------|:---------|:--------|:----------|""")
    foreach(l -> println(io, l), variants)

    # enums
    println(io, """

        ## Enums

        ### Enums that are not available on every version

        | Enum | Versions |
        |:-----|:---------|""")
    for key in keys_of(:enum)
        vs = sort(collect(keys(m.items[key])))
        vs == versions || println(io, "| `$key` | $(fmt(vs)) |")
    end
    println(io, """

        ### Members that are not available on every version

        Arguments with these values are rejected on versions that lack them, and
        `LLVM.isavailable` reports them as unavailable.

        | Enum | Member | Versions |
        |:-----|:-------|:---------|""")
    for key in keys_of(:enum)
        e = m.enums[key]
        for (member, _) in e.members
            vs = [v for v in versions if haskey(e.raw[v], member)]
            vs == versions || println(io, "| `$key` | `$member` | $(fmt(vs)) |")
        end
    end
    println(io, """

        ### Members whose value differs

        The wrappers define each member once; arguments and return values of these enums are
        translated to and from the values of the version in use.

        | Enum | Member | Value | Values in the C API |
        |:-----|:-------|------:|:--------------------|""")
    for key in keys_of(:enum)
        e = m.enums[key]
        e.differs || continue
        for (member, c) in e.members
            byval = Dict{Int,Vector{Int}}()
            for v in versions
                r = get(e.raw[v], member, nothing)
                r === nothing || push!(get!(byval, r, Int[]), v)
            end
            (length(byval) > 1 || any(!=(c), keys(byval))) || continue
            vals = join(["$r ($(fmt(vs)))" for (r, vs) in sort(collect(byval); by=p->minimum(p[2]))], ", ")
            println(io, "| `$key` | `$member` | $c | $vals |")
        end
    end

    # other definitions
    println(io, """

        ## Types and constants

        Types, structs and constants that exist on several versions are the same on all of
        them; merging the wrappers fails otherwise. Constants that differ are excluded from
        the wrappers in `res/llvm.toml`.

        ### Definitions that are not available on every version

        These are defined on every version, as defining them is harmless.

        | Definition | Kind | Versions |
        |:-----------|:-----|:---------|""")
    for kind in (:struct, :const)
        for key in keys_of(kind)
            vs = sort(collect(keys(m.items[key])))
            vs == versions || println(io, "| `$key` | $kind | $(fmt(vs)) |")
        end
    end
    println(io, """

        ### Callback types

        The wrappers type callbacks as `Ptr{Cvoid}`, so their signatures are not checked.
        These are their C declarations, and the versions that have them.

        | Callback | Versions | Declaration |
        |:---------|:---------|:------------|""")
    for key in keys_of(:const)
        d = m.items[key]
        decls = Dict{String,Vector{Int}}()
        for (v, item) in d
            startswith(item.comment, "# typedef") || continue
            push!(get!(decls, strip(chopprefix(item.comment, "#")), Int[]), v)
        end
        for (decl, vs) in sort(collect(decls); by=p->minimum(p[2]))
            println(io, "| `$key` | $(fmt(vs)) | `$(replace(decl, "|" => "\\|"))` |")
        end
    end

    write(path, String(take!(io)))
end
