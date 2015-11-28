-module(pgpool).

%% API
-export([start/0, stop/0]).


%% ===================================================================
%% API
%% ===================================================================
-spec start() -> ok.
start() ->
    ok = ensure_started(poolboy),
    ok = ensure_started(epgsql),
    ok = ensure_started(pgpool).

-spec stop() -> ok.
stop() ->
    ok = application:stop(pgpool),
    ok = application:stop(epgsql),
    ok = application:stop(poolboy).

%% ===================================================================
%% Internal
%% ===================================================================
-spec ensure_started(App :: atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
