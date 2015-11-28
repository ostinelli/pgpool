-module(pgpool_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API
%% ===================================================================
-spec start_link() -> {ok, pid()} | {already_started, pid()} | shutdown.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Callbacks
%% ===================================================================
-spec init([]) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    %% get databases
    Children = case application:get_env(databases) of
        {ok, Databases} ->
            children_spec(Databases);
        _ ->
            []
    end,
    %% start sup
    {ok, {{one_for_one, 10, 10}, Children}}.

%% ===================================================================
%% Internal
%% ===================================================================
-spec children_spec(Databases :: any()) -> [supervisor:child_spec()].
children_spec(Databases) ->
    children_spec(Databases, []).
children_spec([], Specs) ->
    Specs;
children_spec([{DatabaseName, DatabaseInfo} | T], Specs) ->
    PoolArgs0 = proplists:get_value(pool, DatabaseInfo),
    ConnectionArgs0 = proplists:get_value(connection, DatabaseInfo),

    PoolArgs = [
        {name, {local, DatabaseName}},
        {worker_module, pgpool_worker}
    ] ++ PoolArgs0,

    ConnectionArgs = [
        {database_name = DatabaseName}
    ] ++ ConnectionArgs0,

    Spec = poolboy:child_spec(DatabaseName, PoolArgs, ConnectionArgs),
    children_spec(T, [Spec | Specs]).
