%% ==========================================================================================================
%% PGPool - A PosgreSQL client that automatically uses connection pools and reconnects in case of errors.
%%
%% The MIT License (MIT)
%%
%% Copyright (c) 2016 Roberto Ostinelli <roberto@ostinelli.net> and Neato Robotics, Inc.
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
-module(pgpool_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).
-export([squery/2, squery/3]).
-export([equery/3, equery/4]).
-export([batch/2, batch/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% records
-record(state, {
    database_name = undefined :: undefined | atom(),
    host = undefined :: undefined | string(),
    user = undefined :: undefined | string(),
    pass = undefined :: undefined | string(),
    connect_options = undefined :: undefined | list(),
    conn = undefined :: undefined | any(),
    timer_ref = undefined :: undefined | reference(),
    prepared_statements = undefined :: any()
}).

%% macros
-define(RECONNECT_TIMEOUT_MS, 5000).
-define(RETRY_SLEEP_MS, 1000).

%% includes
-include("pgpool.hrl").

%% ===================================================================
%% API
%% ===================================================================
-spec start_link(Args :: list()) -> {ok, pid()} | {error, any()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec squery(DatabaseName :: atom(), Sql :: string() | iodata()) ->
    {ok, Count :: non_neg_integer()}
    | {ok, Count :: non_neg_integer(), Rows :: any()}
    | {error, no_connection | no_available_connections}.
squery(DatabaseName, Sql) ->
    squery(DatabaseName, Sql, []).

-spec squery(DatabaseName :: atom(), Sql :: string() | iodata(), Options :: [pgpool_query_option()]) ->
    {ok, Count :: non_neg_integer()}
    | {ok, Count :: non_neg_integer(), Rows :: any()}
    | {error, no_connection | no_available_connections}.
squery(DatabaseName, Sql, Options) ->
    transaction(DatabaseName, {squery, Sql}, Options).

-spec equery(DatabaseName :: atom(), Statement :: string(), Params :: list()) ->
    {ok, Count :: non_neg_integer()}
    | {ok, Count :: non_neg_integer(), Rows :: any()}
    | {error, no_connection | no_available_connections}.
equery(DatabaseName, Statement, Params) ->
    equery(DatabaseName, Statement, Params, []).

-spec equery(DatabaseName :: atom(), Statement :: string(), Params :: list(), Options :: [pgpool_query_option()]) ->
    {ok, Count :: non_neg_integer()}
    | {ok, Count :: non_neg_integer(), Rows :: any()}
    | {error, no_connection | no_available_connections}.
equery(DatabaseName, Statement, Params, Options) ->
    transaction(DatabaseName, {equery, Statement, Params}, Options).

-spec batch(DatabaseName :: atom(), [{Statement :: string(), Params :: list()}]) ->
    [{ok, Count :: non_neg_integer()} | {ok, Count :: non_neg_integer(), Rows :: any()}]
    | {error, no_connection | no_available_connections}.
batch(DatabaseName, StatementsWithParams) ->
    batch(DatabaseName, StatementsWithParams, []).

-spec batch(DatabaseName :: atom(), [{Statement :: string(), Params :: list()}], Options :: [pgpool_query_option()]) ->
    [{ok, Count :: non_neg_integer()} | {ok, Count :: non_neg_integer(), Rows :: any()}]
    | {error, no_connection | no_available_connections}.
batch(DatabaseName, StatementsWithParams, Options) ->
    transaction(DatabaseName, {batch, StatementsWithParams}, Options).

%% ===================================================================
%% Callbacks
%% ===================================================================

%% ----------------------------------------------------------------------------------------------------------
%% Init
%% ----------------------------------------------------------------------------------------------------------
-spec init(Args :: list()) ->
    {ok, #state{}} |
    {ok, #state{}, Timeout :: non_neg_integer()} |
    ignore |
    {stop, Reason :: any()}.
init(Args) ->
    process_flag(trap_exit, true),

    %% read options
    DatabaseName = proplists:get_value(database_name, Args),

    %% read connection options
    Host = proplists:get_value(host, Args),
    User = proplists:get_value(user, Args),
    Pass = proplists:get_value(pass, Args),
    ConnectOptions = proplists:get_value(options, Args),

    %% build state
    State = #state{
        database_name = DatabaseName,
        host = Host,
        user = User,
        pass = Pass,
        connect_options = ConnectOptions,
        prepared_statements = dict:new()
    },

    %% connect
    State1 = connect(State),

    %% return
    {ok, State1}.

%% ----------------------------------------------------------------------------------------------------------
%% Call messages
%% ----------------------------------------------------------------------------------------------------------
-spec handle_call(Request :: any(), From :: any(), #state{}) ->
    {reply, Reply :: any(), #state{}} |
    {reply, Reply :: any(), #state{}, Timeout :: non_neg_integer()} |
    {noreply, #state{}} |
    {noreply, #state{}, Timeout :: non_neg_integer()} |
    {stop, Reason :: any(), Reply :: any(), #state{}} |
    {stop, Reason :: any(), #state{}}.

handle_call(_Msg, _From, #state{conn = undefined} = State) ->
    {reply, {error, no_connection}, State};

handle_call({squery, Sql}, _From, #state{conn = Conn} = State) ->
    {reply, epgsql:squery(Conn, Sql), State};

handle_call({equery, Statement, Params}, _From, #state{conn = Conn} = State) ->
    {_, Name, State1} = prepare_or_get_statement(Statement, State),
    {reply, epgsql:prepared_query(Conn, Name, Params), State1};

handle_call({batch, StatementsWithParams}, _From, #state{
    conn = Conn
} = State) ->
    %% prepare & cache statements
    F = fun({Statement, Params}, {PreparedStatementsAcc, StateAcc}) ->
        %% get or prepare
        {PreparedStatement, _, StateAcc1} = prepare_or_get_statement(Statement, StateAcc),
        %% acc
        {[{PreparedStatement, Params} | PreparedStatementsAcc], StateAcc1}
    end,
    {StatementsForBatchRev, State1} = lists:foldl(F, {[], State}, StatementsWithParams),
    StatementsForBatch = lists:reverse(StatementsForBatchRev),
    %% execute batch
    {reply, epgsql:execute_batch(Conn, StatementsForBatch), State1};

handle_call(Request, From, State) ->
    error_logger:warning_msg("Received from ~p an unknown call message: ~p", [Request, From]),
    {reply, undefined, State}.

%% ----------------------------------------------------------------------------------------------------------
%% Cast messages
%% ----------------------------------------------------------------------------------------------------------
-spec handle_cast(Msg :: any(), #state{}) ->
    {noreply, #state{}} |
    {noreply, #state{}, Timeout :: non_neg_integer()} |
    {stop, Reason :: any(), #state{}}.

handle_cast(Msg, State) ->
    error_logger:warning_msg("Received an unknown cast message: ~p", [Msg]),
    {noreply, State}.

%% ----------------------------------------------------------------------------------------------------------
%% All non Call / Cast messages
%% ----------------------------------------------------------------------------------------------------------
-spec handle_info(Info :: any(), #state{}) ->
    {noreply, #state{}} |
    {noreply, #state{}, Timeout :: non_neg_integer()} |
    {stop, Reason :: any(), #state{}}.

handle_info(connect, State) ->
    State1 = connect(State),
    {noreply, State1};

handle_info({'EXIT', _From, _Reason}, State) ->
    error_logger:error_msg("epgsql process died, start a timer to reconnect"),
    State1 = timeout(State),
    {noreply, State1};

handle_info(Info, State) ->
    error_logger:warning_msg("Received an unknown info message: ~p", [Info]),
    {noreply, State}.

%% ----------------------------------------------------------------------------------------------------------
%% Terminate
%% ----------------------------------------------------------------------------------------------------------
-spec terminate(Reason :: any(), #state{}) -> terminated.
terminate(_Reason, #state{conn = Conn}) ->
    %% terminate
    case Conn of
        undefined -> ok;
        _ -> ok = epgsql:close(Conn)
    end,
    terminated.

%% ----------------------------------------------------------------------------------------------------------
%% Convert process state when code is changed.
%% ----------------------------------------------------------------------------------------------------------
-spec code_change(OldVsn :: any(), #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================
-spec connect(#state{}) -> #state{}.
connect(#state{
    database_name = DatabaseName,
    host = Host,
    user = User,
    pass = Pass,
    connect_options = ConnectOptions
} = State) ->
    error_logger:info_msg("Connecting to database ~p", [DatabaseName]),

    case epgsql:connect(Host, User, Pass, ConnectOptions) of
        {ok, Conn} ->
            State#state{conn = Conn, prepared_statements = dict:new()};
        Error ->
            error_logger:error_msg("Error connecting to database ~p with host ~p, user ~p, options ~p: ~p, will try reconnecting in ~p ms", [
                DatabaseName, Host, User, ConnectOptions, Error, ?RECONNECT_TIMEOUT_MS
            ]),
            State#state{conn = undefined}
    end.

-spec timeout(#state{}) -> #state{}.
timeout(#state{
    timer_ref = TimerPrevRef
} = State) ->
    case TimerPrevRef of
        undefined -> ignore;
        _ -> erlang:cancel_timer(TimerPrevRef)
    end,
    TimerRef = erlang:send_after(?RECONNECT_TIMEOUT_MS, self(), connect),
    State#state{timer_ref = TimerRef}.

-spec prepare_or_get_statement(Statement :: string(), #state{}) -> {PreparedStatement :: any(), StatementName :: string(), #state{}}.
prepare_or_get_statement(Statement, #state{
    conn = Conn,
    prepared_statements = PreparedStatements
} = State) ->
    Name = "statement_" ++ integer_to_list(erlang:phash2(Statement)),
    case dict:find(Name, PreparedStatements) of
        {ok, PreparedStatement} ->
            {PreparedStatement, Name, State};
        error ->
            %% prepare statement
            {ok, PreparedStatement} = epgsql:parse(Conn, Name, Statement, []),
            %% store
            PreparedStatements1 = dict:store(Name, PreparedStatement, PreparedStatements),
            %% update state
            State1 = State#state{prepared_statements = PreparedStatements1},
            %% return
            {PreparedStatement, Name, State1}
    end.

-spec transaction(
    DatabaseName :: atom(),
    Message :: {squery, Sql :: string() | iodata()}
    | {equery, Statement :: string(), Params :: list()}
    | {batch, [{Statement :: string(), Params :: list()}]},
    Options :: [pgpool_query_option()]
) ->
    {ok, Count :: non_neg_integer()}
    | {ok, Count :: non_neg_integer(), Rows :: any()}
    | [{ok, Count :: non_neg_integer()} | {ok, Count :: non_neg_integer(), Rows :: any()}]
    | {error, no_connection | no_available_connections}.
transaction(DatabaseName, Message, Options) ->
    Block = not lists:member(no_wait, Options),
    case poolboy:checkout(DatabaseName, Block) of
        full ->
            {error, no_available_connections};
        Worker ->
            try
                gen_server:call(Worker, Message)
            after
                ok = poolboy:checkin(DatabaseName, Worker)
            end
    end.
