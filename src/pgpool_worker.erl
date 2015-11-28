-module(pgpool_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).
-export([squery/2, equery/3]).

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
    timer_ref = undefined :: undefined | reference()
}).

%% macros
-define(RECONNECT_TIMEOUT_MS, 5000).


%% ===================================================================
%% API
%% ===================================================================
-spec start_link(Args :: list()) -> {ok, pid()} | {error, any()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec squery(DatabaseName :: atom(), Sql :: string() | iodata()) -> any() | {error, no_connection}.
squery(DatabaseName, Sql) ->
    poolboy:transaction(DatabaseName, fun(Worker) ->
        gen_server:call(Worker, {squery, Sql})
    end).

-spec equery(DatabaseName :: atom(), Statement :: string(), Params :: list()) -> any() | {error, no_connection}.
equery(DatabaseName, Statement, Params) ->
    poolboy:transaction(DatabaseName, fun(Worker) ->
        gen_server:call(Worker, {equery, Statement, Params})
    end).

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
        connect_options = ConnectOptions
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
    {reply, epgsql:equery(Conn, Statement, Params), State};

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
    %% epgsql process died, start a timer to reconnect
    State1 = timeout(State),
    {noreply, State1};

handle_info(Info, State) ->
    error_logger:warning_msg("Received an unknown info message: ~p", [Info]),
    {noreply, State}.

%% ----------------------------------------------------------------------------------------------------------
%% Terminate
%% ----------------------------------------------------------------------------------------------------------
-spec terminate(Reason :: any(), #state{}) -> terminated.
terminate(Reason, #state{conn = Conn}) ->
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
            State#state{conn = Conn};
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
