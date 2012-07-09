-module(nakaz_props).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Generators

well_typed_atom() ->
    ?LET({Value, Type}, {atom(), oneof([node, module, atom])},
         {{undefined, Type, []},
          atom_to_binary(Value, utf8),
          Value}).

well_typed_binary() ->
    ?LET(Value, binary(),
         {{undefined, binary, []}, Value, Value}).

well_typed_string() ->
    %% FIXME(Sergei): just using 'string/0' fails with badarg on
    %% 'list_to_binary()' call.
    ?LET(Value, list(byte()),
         {{undefined, string, []}, list_to_binary(Value), Value}).

well_typed_nonempty_string() ->
    %% FIXME(Sergei): see note above.
    ?LET(Value, non_empty(list(byte())),
         {{undefined, nonempty_string, []},
          list_to_binary(Value),
          Value}).

well_typed_integer() ->
    ?LET(Value, integer(),
         {{undefined, integer, []},
          list_to_binary(integer_to_list(Value)),
          Value}).

well_typed_pos_integer() ->
    ?LET(Value, pos_integer(),
         {{undefined, pos_integer, []},
          list_to_binary(integer_to_list(Value)),
          Value}).

well_typed_non_neg_integer() ->
    ?LET(Value, non_neg_integer(),
         {{undefined, non_neg_integer, []},
          list_to_binary(integer_to_list(Value)),
          Value}).

well_typed_float() ->
    ?LET(Value, float(),
         {{undefined, float, []},
          list_to_binary(float_to_list(Value)),
          Value}).

well_typed_yaml() ->
    oneof([well_typed_atom(),
           well_typed_binary(),
           well_typed_string(),
           well_typed_nonempty_string(),
           well_typed_integer(),
           well_typed_pos_integer(),
           well_typed_non_neg_integer(),
           well_typed_float()]).

%% Properties

prop_well_typed() ->
    Pos = {0, 0},
    ?FORALL({Type, Bin, Value}, well_typed_yaml(),
            begin
                case nakaz_typer:type_field(Type, {Bin, Pos}) of
                    {ok, Value} -> true;
                    _Any        -> false
                end
            end).

%% Suite

proper_test_() ->
    {timeout, 600,
     ?_assertEqual([], proper:module(?MODULE, [{to_file, user},
                                               {numtests, 1000}]))}.
