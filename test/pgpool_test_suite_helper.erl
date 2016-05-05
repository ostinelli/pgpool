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
-module(pgpool_test_suite_helper).

%% API
-export([set_environment_variables/0, set_environment_variables/1]).

%% macros
-define(PGPOOL_TEST_CONFIG_FILENAME, "pgpool-test.config").


%% ===================================================================
%% API
%% ===================================================================
set_environment_variables() ->
    set_environment_variables(node()).
set_environment_variables(Node) ->
    % read config file
    ConfigFilePath = filename:join([filename:dirname(code:which(?MODULE)), ?PGPOOL_TEST_CONFIG_FILENAME]),
    {ok, [AppsConfig]} = file:consult(ConfigFilePath),
    % loop to set variables
    F = fun({AppName, AppConfig}) ->
        set_environment_for_app(Node, AppName, AppConfig)
    end,
    lists:foreach(F, AppsConfig).

%% ===================================================================
%% Internal
%% ===================================================================
set_environment_for_app(Node, AppName, AppConfig) ->
    F = fun({Key, Val}) ->
        ok = rpc:call(Node, application, set_env, [AppName, Key, Val])
    end,
    lists:foreach(F, AppConfig).
