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
    %% maybe substitute file:// password references with their value
    maybe_dereference_password(),
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
    ] ++ remove_list_elements([name, worker_module], PoolArgs0),

    ConnectionArgs = [
        {database_name, DatabaseName}
    ] ++ remove_list_elements([database_name], ConnectionArgs0),

    Spec = poolboy:child_spec(DatabaseName, PoolArgs, ConnectionArgs),
    children_spec(T, [Spec | Specs]).

remove_list_elements([], L) -> L;
remove_list_elements([El|T], L) ->
    remove_list_elements(T, lists:keydelete(El, 1, L)).


%% Maybe read DB passwords from files.  If {pass, Value} has a Value
%% that matches "file://" (e.g., "file:///var/run/secrets/password"),
%% substitue the text of the file /var/run/secrets/password for Value.
maybe_dereference_password() ->
    case application:get_env(databases) of
        {ok, DBs} ->
            NewDBs = [ dereference_password(DB) || DB <- DBs ],
            application:set_env(pgpool, databases, NewDBs);
        [] ->
            ok
    end.

dereference_password(DB) ->
    {Name, DBPlist} = DB,
    Connection = proplists:get_value(connection, DBPlist),
    {pass, ExistingPass} = lists:keyfind(pass, 1, Connection),
    NewPass = replace_password(ExistingPass),
    NewConnection = lists:keyreplace(pass, 1, Connection, {pass, NewPass}),
    NewDBPlist = lists:keyreplace(connection, 1, DBPlist,
                                  {connection, NewConnection}),
    {Name, NewDBPlist}.

replace_password("file://" ++ PasswordFile) ->
    {ok, NewPassword} = file:read_file(PasswordFile),
    string:trim(binary_to_list(NewPassword));
replace_password(ExistingPassword) ->
    ExistingPassword.
