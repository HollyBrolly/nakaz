-module(nakaz_errors).

-include("nakaz_internal.hrl").

-export([render/1]).

-spec render(any()) -> binary().
render(Error) ->
    case r(Error) of
        {Msg, Args} -> iolist_to_binary(io_lib:format(Msg, Args));
        Msg         -> list_to_binary(Msg)
    end.

-spec r(any()) -> {string(), [any()]}.
r({cant_execute_magic_fun, Mod}) ->
    {"can't execute 'magic function' that must be generated "
     "by nakaz_pt in module ~s", [Mod]};
r(config_empty) ->
    "ooops, looks like the config is empty";
r({malformed, [{app, {Name, _Body}}|_Rest]}) ->
    {"malformed application structure in ~p, sections aren't mappings?",
     [Name]};
r({malformed, [{section, {Name, _Body}}|_Rest]}) ->
    {"malformed section structure in ~p, not a mapping?'",
     [Name]};
r({missing, {app, Name}}) ->
    {"missing application ~p", [Name]};
r({missing, {section, Name, App}}) ->
    {"missing section ~p for application ~p", [Name, App]};
r({missing, {field, Name, Section}}) ->
    {"missing field ~p in section ~p", [Name, Section]};
r({invalid, {Name, Type, Value, {Line, _Column}}}) ->
    {"invalid type at line ~p: value ~p for field ~p doesn't match ~s",
     [Line+1, Value, Name, pp_type(Type)]};
r({unsupported, Line, Mod}) ->
    {"unsupported type expression at ~p.erl:~p", [Mod, Line+1]};
r(single_config_file_required) ->
    "nakaz requires a single configuration file";
r(no_config_path_provided) ->
    "please provide a path to config file";
r(UnknownError) ->
    ok = error_logger:warning_msg("no clause for rendering error ~p", [UnknownError]),
    %% NOTE(Dmitry): the following is Sergei's legacy, please don't touch
    %%               or he'll be a sad panda
    {"evil martians are remote controlling your node! maybe that'll help: ~p",
     [UnknownError]}.

-spec pp_type(nakaz_typespec()) -> iolist().
pp_type({undefined, range, [From, To]}) ->
    io_lib:format("~p..~p", [From, To]);
pp_type({undefined, tuple, SubTypes}) ->
    io_lib:format("{~s}", [pp_types(SubTypes)]);
pp_type({undefined, union, SubTypes}) ->
    Sep = " or ",
    case lists:all(fun is_atom/1, SubTypes) of
        true  ->
            string:join(lists:map(fun atom_to_list/1, SubTypes), Sep);
        false -> pp_types(SubTypes, Sep)
    end;
pp_type({undefined, Type, []}) ->
    atom_to_list(Type);
pp_type({undefined, Type, SubTypes}) ->
    io_lib:format("~s(~s)", [Type, pp_types(SubTypes)]);
pp_type({Mod, Type, SubTypes}) ->
    io_lib:format("~s:~s", [Mod, pp_type({undefined, Type, SubTypes})]).

-spec pp_types([nakaz_typespec()]) -> string().
pp_types(Types) ->
    pp_types(Types, ", ").

-spec pp_types([nakaz_typespec()], string()) -> string().
pp_types(Types, Sep) ->
    string:join(lists:map(fun pp_type/1, Types), Sep).
