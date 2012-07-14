-module(nakaz).
-behaviour(application).
-include("nakaz_internal.hrl").

%% API

-export([start/0, stop/0]).
-export([ensure/2, ensure/3, ensure/4,
         use/2, use/3,
         reload/0, reload/1]).

%% Application callbacks

-export([start/2, stop/1]).

%% API

start() ->
    ok = application:start(nakaz).

stop() ->
    ok = application:stop(nakaz).

-spec ensure(atom(), [record_()]) -> ret_novalue().
ensure(Mod, Records) ->
    ensure(Mod, Records, []).

-spec ensure(atom(), [record_()], nakaz_core:options()) -> ret_novalue().
ensure(Mod, Records, Options) ->
    {ok, AppName} = application:get_application(),
    ensure(Mod, AppName, Records, Options).

-spec ensure(atom(), atom(), [record_()],
             nakaz_core:options()) -> ret_novalue().
ensure(Mod, App, Records, Options) ->
    nakaz_core:ensure(Mod, App, Records, Options).

-spec use(atom(), T) -> ret_value(T) when T :: record_().
use(Mod, Record) ->
    {ok, AppName} = application:get_application(),
    use(Mod, AppName, Record).

-spec use(atom(), atom(), T) -> ret_value(T) when T :: record_().
use(Mod, App, Record) ->
    nakaz_core:use(Mod, App, Record).

-spec reload() -> [{atom(), ret_novalue()}].
reload() ->
    nakaz_core:reload().

-spec reload(atom()) -> ret_novalue().
reload(App) ->
    nakaz_core:reload(App).

%% Application callbacks

%% FIXME(Dmitry): we need to document this behaviour with erl argument
%%                and config
start(_StartType, _StartArgs) ->
    case init:get_argument(nakaz) of
        {ok, [[ConfPath]]} ->
            nakaz_sup:start_link(ConfPath);
        {ok, _} ->
            {error, nakaz_errors:render(single_config_file_required)};
        error ->
            case application:get_env(nakaz, config) of
                {ok, ConfPath} ->
                    nakaz_sup:start_link(ConfPath);
                _ ->
                    %% FIXME(Dmitry): fix error message
                    {error, nakaz_errors:render(no_config_path_provide)}
            end
    end.

stop(_State) ->
    ok.
