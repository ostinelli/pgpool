-module(pgpool).

%% API
-export([start/0, stop/0]).
-export([squery/2, equery/3]).
-export([squery_retry/3, equery_retry/4]).

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

-spec squery(DatabaseName :: atom(), Sql :: string() | iodata()) ->
    any() | {error, no_connection}.
squery(DatabaseName, Sql) ->
    pgpool_worker:squery(DatabaseName, Sql).

-spec equery(DatabaseName :: atom(), Statement :: string(), Params :: list()) ->
    any() | {error, no_connection}.
equery(DatabaseName, Statement, Params) ->
    pgpool_worker:equery(DatabaseName, Statement, Params).

-spec equery_retry(DatabaseName :: atom(), Statement :: string(), Params :: list(), RetryTimeout :: non_neg_integer() | infinity) ->
    any() | {error, no_connection}.
equery_retry(DatabaseName, Statement, Params, RetryTimeout) ->
    pgpool_worker:equery_retry(DatabaseName, Statement, Params, RetryTimeout).

-spec squery_retry(DatabaseName :: atom(), Sql :: string() | iodata(), RetryTimeout :: non_neg_integer() | infinity) ->
    any() | {error, no_connection}.
squery_retry(DatabaseName, Sql, RetryTimeout) ->
    pgpool_worker:squery_retry(DatabaseName, Sql, RetryTimeout).

%% ===================================================================
%% Internal
%% ===================================================================
-spec ensure_started(App :: atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
