-module(pgpool).

%% API
-export([start/0, stop/0]).
-export([squery/2, equery/3]).

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

-spec squery(DatabaseName :: atom(), Sql :: string() | iodata()) -> any() | {error, no_connection}.
squery(DatabaseName, Sql) ->
    pgpool_worker:squery(DatabaseName, Sql).

-spec equery(DatabaseName :: atom(), Statement :: string(), Params :: list()) -> any() | {error, no_connection}.
equery(DatabaseName, Statement, Params) ->
    pgpool_worker:equery(DatabaseName, Statement, Params).

%% ===================================================================
%% Internal
%% ===================================================================
-spec ensure_started(App :: atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
