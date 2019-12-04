%% ==========================================================================================================
%% PGPool - A PosgreSQL client that automatically uses connection pools and reconnects in case of errors.
%%
%% The MIT License (MIT)
%%
%% Copyright (c) 2016-2019 Roberto Ostinelli <roberto@ostinelli.net> and Neato Robotics, Inc.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% ==========================================================================================================
-module(pgpool).

%% API
-export([start/0, stop/0]).
-export([squery/2, squery/3]).
-export([equery/3, equery/4]).
-export([batch/2, batch/3]).

%% includes
-include("pgpool.hrl").

%% ===================================================================
%% API
%% ===================================================================
-spec start() -> ok.
start() ->
    ok = ensure_started(poolboy),
    ok = ensure_started(asn1),
    ok = ensure_started(crypto),
    ok = ensure_started(public_key),
    ok = ensure_started(ssl),
    ok = ensure_started(epgsql),
    ok = ensure_started(pgpool).

-spec stop() -> ok.
stop() ->
    ok = application:stop(pgpool).

-spec squery(DatabaseName :: atom(), Sql :: string() | iodata()) ->
    {ok, Count :: non_neg_integer()}
    | {ok, Count :: non_neg_integer(), Rows :: any()}
    | {error, no_connection | no_available_connections}.
squery(DatabaseName, Sql) ->
    pgpool_worker:squery(DatabaseName, Sql).

-spec squery(DatabaseName :: atom(), Sql :: string() | iodata(), Options :: [pgpool_query_option()]) ->
    {ok, Count :: non_neg_integer()}
    | {ok, Count :: non_neg_integer(), Rows :: any()}
    | {error, no_connection | no_available_connections}.
squery(DatabaseName, Sql, Options) ->
    pgpool_worker:squery(DatabaseName, Sql, Options).

-spec equery(DatabaseName :: atom(), Statement :: string(), Params :: list()) -> any().
equery(DatabaseName, Statement, Params) ->
    pgpool_worker:equery(DatabaseName, Statement, Params).

-spec equery(DatabaseName :: atom(), Statement :: string(), Params :: list(), Options :: [pgpool_query_option()]) ->
    any() | {error, no_available_connections}.
equery(DatabaseName, Statement, Params, Options) ->
    pgpool_worker:equery(DatabaseName, Statement, Params, Options).

-spec batch(DatabaseName :: atom(), [{Statement :: string(), Params :: list()}]) ->
    [{ok, Count :: non_neg_integer()} | {ok, Count :: non_neg_integer(), Rows :: any()}].
batch(DatabaseName, StatementsWithParams) ->
    pgpool_worker:batch(DatabaseName, StatementsWithParams).

-spec batch(DatabaseName :: atom(), [{Statement :: string(), Params :: list()}], Options :: [pgpool_query_option()]) ->
    [{ok, Count :: non_neg_integer()} | {ok, Count :: non_neg_integer(), Rows :: any()}] | {error, no_available_connections}.
batch(DatabaseName, StatementsWithParams, Options) ->
    pgpool_worker:batch(DatabaseName, StatementsWithParams, Options).

%% ===================================================================
%% Internal
%% ===================================================================
-spec ensure_started(App :: atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.
