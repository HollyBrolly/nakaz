-module(nakaz).
-behaviour(application).
-include("nakaz_internal.hrl").

%% API

-export([start/0, stop/0]).
-export([ensure/2, ensure/3, ensure/4,
         use/2, use/3]).

%% Application callbacks

-export([start/2, stop/1]).

%% Types

-type nakaz_option()  :: {reload_type, reload_type()}.
-type nakaz_options() :: [nakaz_option()].

%% API

start() ->
    ok = application:start(nakaz).

stop() ->
    ok = application:stop(nakaz).

-spec ensure(atom(), [record()]) -> ret_novalue().
ensure(Mod, Records) ->
    ensure(Mod, Records, []).

-spec ensure(atom(), [record()], nakaz_options()) -> ret_novalue().
ensure(Mod, Records, Options) ->
    ensure(Mod, application:get_application(), Records, Options).

-spec ensure(atom(), atom(), [record()], nakaz_options()) -> ret_novalue().
ensure(Mod, App, Records, Options) ->
    nakaz_core:ensure(Mod, App, Records, Options).

-spec use(atom(), T) -> ret_value(T) when T :: record().
use(Mod, Record) ->
    use(Mod, application:get_application(), Record).

-spec use(atom(), atom(), T) -> ret_value(T) when T :: record().
use(Mod, App, Record) ->
    nakaz_core:use(Mod, App, Record).

%% Application callbacks

%% FIXME(Dmitry): we need to document this behaviour with erl argument
%%                and config
start(_StartType, _StartArgs) ->
    case init:get_argument(nakaz) of
        {ok, [[ConfPath]]} ->
            nakaz_sup:start_link(ConfPath);
        {ok, _} ->
            {error, "nakaz requires a single configuration file"};
        error ->
            case application:get_env(nakaz, config) of
                {ok, ConfPath} ->
                    nakaz_sup:start_link(ConfPath);
                _ ->
                    %% FIXME(Dmitry): fix error message
                    {error, "please provide a path to config file"}
            end
    end.

stop(_State) ->
    ok.
