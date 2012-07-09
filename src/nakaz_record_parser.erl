%%% @private
%%% @author Alexander Neganov <ikkeps@gmail.com>
%%%
%%% @doc
%%% This module parses forms that cames from {@see nakaz_pt:parse_transform}
%%% and extracts all records specs. If record spec is unsupported
%%% (in case if some fields have unsupported notation), record spec
%%% is marked as 'unsupported' with line number and module name information.
%%% @end

-module(nakaz_record_parser).

-include("nakaz_internal.hrl").

-export([extract_records_specs/2]).

-define(BUILTIN_TYPES,
        [binary, integer, pos_integer, neg_integer, non_neg_integer,
         range, number, string, nonempty_string, module, node, timeout,
         none, byte, char, nil, list, nonempty_list, tuple, float,
         record, boolean, atom, union]).

%% Note(Sergei): those types cannot be covnerted from YAML values;
%% most of them simple makes no sense in a config file.
-define(UNSUPPORTED_TYPES,
        [any, port, pid, none, reference, term, maybe_improper_list,
         iolist, none, no_return]).

%% @doc Extract records specifications from parse terms
-spec extract_records_specs([term()], module()) -> record_specs().
extract_records_specs(Forms, Module) ->
    lists:foldl(fun (F,Acc) -> handle_type(F, Acc, Module) end,
                [],
                Forms).

%% @doc Handles only 'type' forms, if it is record spec then extracts it
%%      and accumulates with Acc.
-spec handle_type(term(), record_specs(), module()) -> record_specs().
handle_type(Form, Acc, Module) ->
    case erl_syntax_lib:analyze_form(Form) of
        {attribute, {type, {type, Type}}} ->
            handle_record(Type, Acc, Module);
        _ ->
            Acc
    end.

%% @doc Handle only records types forms
-spec handle_record(term(), record_specs(), module()) -> record_specs().
handle_record({{record, Name}, Fields, _Args}, Acc, Module) ->
    RecordFields =
        try
            [handle_field(Field, Module) || Field <- Fields]
        catch
            throw:{unsupported_field, Form, Module} ->
                %% FIXME(Sashs): Does form always have a lineno as its
                %% second element?
                {unsupported, element(2, Form), Module}
        end,
    %% Update ordset only if record is one of already referenced
    [{Name, RecordFields} | Acc];
handle_record(_, Acc, _) ->
    Acc.

%% @doc Handle record field form. Only typed_record_field supported.
-spec handle_field(term(), module()) -> record_field_spec().
handle_field({typed_record_field,
              {record_field, _, {atom, _, Name}}, Type}, Module) ->
    Field = handle_field_type(Type, Module),
    {Name, Field, undefined};
handle_field({typed_record_field,
              {record_field, _, {atom, _, Name}, Default}, Type},
             Module) ->
    %% Handle special case with default value.
    Field   = handle_value_param(Type, Module),
    Default = handle_value_param(Default, Module),
    {Name, Field, Default};
handle_field(Other, Module) ->
    throw({unsupported_field, Other, Module}).

%% @doc Handle record field type. For now, only types of 'union'
%%      supported (actually, most of field types)
-spec handle_field_type(term(), module()) -> nakaz_typespec().
handle_field_type({type, _, union, [{atom, _, undefined}|Types]}, Module) ->
    handle_union(Types, Module);
handle_field_type(Other, Module) ->
    throw({unsupported_field, Other, Module}).

%% @doc Handle list of type forms. If there are more than one element
%%      in list - wrap element in 'union'.
-spec handle_union(list(term()), module()) -> nakaz_typespec().
handle_union([Type], Module) ->
    handle_value_param(Type, Module);
handle_union(Types, Module) ->
    {get_module_for_type(union, Module),
     union,
     [handle_value_param(Type, Module)
      || Type <- Types]}.

%% @doc Handle actual form of field type.
-spec handle_value_param(term(), module()) -> nakaz_typespec().
handle_value_param({remote_type, _,
                    [{atom, _, Module},
                     {atom, _, Type},
                     Args]}, _Module) ->
    {Module, Type,
     [handle_value_param(Arg, Module) || Arg <- Args]};
handle_value_param({type,_,record,[]}=Form, Module) ->
    %% We do not support generic record type
    throw({unsupported_field, Form, Module});
handle_value_param({type,_, Type, Args}=Form, Module) when is_list(Args) ->
    case lists:member(Type, ?UNSUPPORTED_TYPES) of
        true -> throw({unsupported_field, Form, Module});
        false ->
            {get_module_for_type(Type, Module),
             Type,
             [handle_value_param(Arg, Module) || Arg <- Args]}
    end;
handle_value_param({type,_, Type, Arg}, Module) ->
    %% Handle special case when type arguments is not list
    {get_module_for_type(Type, Module),
     Type,
     [Arg]};
%% FIXME: rewrite this using erl_syntax_lib for great good.
handle_value_param({atom,_, Atom}, _) ->
    Atom;
handle_value_param({integer, _, Integer}, _) ->
    Integer;
handle_value_param({op,_, '-', {integer, _, Integer}}, _) ->
    %% Unary minus. Special case. Again.
    -Integer;
handle_value_param({float,_,Float}, _) ->
    Float;
handle_value_param({boolean,_,Boolean}, _) ->
    Boolean;
handle_value_param({tuple,_,Values}, Module) ->
    list_to_tuple([handle_value_param(V, Module) || V <- Values]);
handle_value_param({_, LineNo, _}, Module) ->
    throw({unsupported_field, LineNo, Module}).

%% @doc Get module for type name. If type in builtin types, returns undefined
%%      otherwise, returns 'undefined'.
-spec get_module_for_type(atom(), module())-> module() | undefined.
get_module_for_type(TypeName, Module) ->
    case lists:member(TypeName, ?BUILTIN_TYPES) of
        true  -> undefined;
        false -> Module
    end.
