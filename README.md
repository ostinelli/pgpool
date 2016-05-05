[![Build Status](https://travis-ci.org/ostinelli/pgpool.svg?branch=master)](https://travis-ci.org/ostinelli/pgpool)

# PGPool

PGPool is a PosgreSQL client that automatically uses connection pools and handles reconnections in case of errors.

Under the hood, it uses:

 * [epgsql](https://github.com/epgsql/epgsql) as the PostgreSQL driver.
 * [poolboy](https://github.com/devinus/poolboy) as pooling library.


## Install

If you're using [rebar](https://github.com/rebar/rebar), add `pgpool` as a dependency in your project's `rebar.config` file:

```erlang
{pgpool, ".*", {git, "git://github.com/ostinelli/pgpool.git", "master"}}
```

Then, get and compile your dependencies:

```
$ rebar get-deps
$ rebar compile
```

## Usage

### Setup
Ensure to start PGPool from your application. This can be done by either providing it as a dependency in your `.app` file, or by starting it manually:

```erlang
pgpool:start().
```

### Specify Databases
Databases can be set in the environment variable `pgpool`. You're probably best off using an application configuration file (in releases, `sys.config`):

```erlang

{pgpool, [
  {databases, [
    {db1_name, [
      {pool, [
        %% poolboy options <https://github.com/devinus/poolboy>
        %% The `name` and `worker_module` options here will be ignored.
        {size, 10},         %% maximum pool size
        {max_overflow, 20}, %% maximum number of workers created if pool is empty
        {strategy, lifo}    %% can be lifo or fifo (default is lifo)
      ]},
      {connection, [
        {host, "localhost"},
        {user, "postgres"},
        {pass, ""},
        {options, [
          %% epgsql connect_options() <https://github.com/epgsql/epgsql>
          {port, 5432},
          {ssl, false},
          {database, "db1"}
        ]}
      ]}
    ]},

    {db2_name, [
      {pool, [
        {size, 10},
        {max_overflow, 20},
        {strategy, lifo}
      ]},
      {connection, [
        {host, "localhost"},
        {user, "postgres"},
        {pass, ""},
        {options, [
          {port, 5432},
          {ssl, false},
          {database, "db2"}
        ]}
      ]}
    ]}
  ]}
]}
```

### Queries
Please refer to [epgsql README](https://github.com/epgsql/epgsql) for how to perform queries. Currently, PGPool supports the following.

#### Simple Query

```erlang
pgpool:squery(DatabaseName, Sql) -> Result

Types:
  DatabaseName = atom()
  Sql = string() | iodata()
  Result =  {ok, Count} | {ok, Count, Rows} | {error, no_connection}
    Count =  non_neg_integer()
    Rows = (see epgsql for more details)
```

For example:

```erlang
pgpool:squery(db1_name, "SELECT * FROM users;").
```

##### Retries
In case there's no available connection to the database, the standard `squery/2` function will return `{error, no_connection}`. If you want to keep retrying until a connection is available, you can use `squery/3`.

Note however that this is a blocking call, and should be used only if needed.

```erlang
pgpool:squery(DatabaseName, Sql, RetryTimeout) -> Result

Types:
  DatabaseName = atom()
  Sql = string() | iodata()
  RetryTimeout = non_neg_integer() | infinity
  Result = {ok, Count} | {ok, Count, Rows} | {error, no_connection}
    Count = non_neg_integer()
    Rows = (see epgsql for more details)
```

`RetryTimeout` specifies how much time (in milliseconds) will be spent waiting to retry (that is, excluding the time taken to call the database). Set to `infinity` if you want the call to block forever until a connection becomes available.

For example:

```erlang
pgpool:squery(db1_name, "SELECT * FROM users;", 60000).
```

#### Extended Query

```erlang
pgpool:equery(DatabaseName, Statement, Params) -> Result

Types:
  DatabaseName = atom()
  Statement = string()
  Params = list()
  Result = {ok, Count} | {ok, Count, Rows} | {error, no_connection}
    Count = non_neg_integer()
    Rows = (see epgsql for more details)
```

For example:

```erlang
pgpool:equery(db1_name, "SELECT * FROM users WHERE id = $1;", [3]).
```

##### Retries
In case there's no available connection to the database, the standard `equery/3` function will return `{error, no_connection}`. If you want to keep retrying until a connection is available, you can use `equery/4`.

Note however that this is a blocking call, and should be used only if needed.

```erlang
pgpool:equery(DatabaseName, Statement, Params, RetryTimeout) -> Result

Types:
  DatabaseName = atom()
  Statement = string()
  Params = list()
  RetryTimeout = non_neg_integer() | infinity
  Result = {ok, Count} | {ok, Count, Rows} | {error, no_connection}
    Count = non_neg_integer()
    Rows = (see epgsql for more details)
```

`RetryTimeout` specifies how much time (in milliseconds) will be spent waiting to retry (that is, excluding the time taken to call the database). Set to `infinity` if you want the call to block forever until a connection becomes available.

For example:

```erlang
pgpool:equery(db1_name, "SELECT * FROM users WHERE id = $1;", [3], 60000).
```

#### Prepared Statements
First, prepare your query:

```erlang
pgpool:parse(DatabaseName, StatementName, Statement) -> Result

Types:
  DatabaseName = atom()
  StatementName = string()
  Statement = string()
  Result = {ok, ParsedStatement} | {error, Reason}
    ParsedStatement = (see epgsql for more details)
    Reason = (see epgsql for more details)
```
Then, execute it:

```erlang
pgpool:execute(DatabaseName, StatementName, Params) -> Result

Types:
  DatabaseName = atom()
  StatementName = string()
  Params = list()
  Result = {ok, Count} | {ok, Count, Rows} | {error, no_connection}
    Count = non_neg_integer()
    Rows = (see epgsql for more details)
```

For example:

```erlang
StatementName = "insert_user",

{ok, _} = pgpool:parse(pgpool_test, StatementName, "INSERT INTO users (name) VALUES ($1);"),
{ok, 1} = pgpool:execute(pgpool_test, StatementName, ["Hedy"]).
```

#### Batch Query
First, prepare your query / queries:

```erlang
pgpool:parse(DatabaseName, Statement) -> Result

Types:
  DatabaseName = atom()
  Statement = string()
  Result = {ok, ParsedStatement} | {error, Reason}
    ParsedStatement = (see epgsql for more details)
    Reason = (see epgsql for more details)
```

Then, execute a batch:

```erlang
pgpool:execute_batch(DatabaseName, Statements) -> Result

Types:
  DatabaseName = atom()
  Statements = [{ParsedStatement, Params}]
  ParsedStatement = (see epgsql for more details)
  Params = list()
  Result =  [{ok, Count} | {ok, Count, Rows}].
    Count =  non_neg_integer()
    Rows = (see epgsql for more details)
```

For example:

```erlang
{ok, S1} = pgpool:parse(pgpool_test, "INSERT INTO users (name) VALUES ($1);"),
{ok, S2} = pgpool:parse(pgpool_test, "INSERT INTO films (name, year) VALUES ($1, $2);"),

[{ok, 1}, {ok, 1}] = pgpool:execute_batch(pgpool_test, [
    {S1, ["Hedy"]},
    {S2, ["First Movie", 1978]}
]).
```

## Contributing
So you want to contribute? That's great! Please follow the guidelines below. It will make it easier to get merged in.

Before implementing a new feature, please submit a ticket to discuss what you intend to do. Your feature might already be in the works, or an alternative implementation might have already been discussed.

Do not commit to master in your fork. Provide a clean branch without merge commits. Every pull request should have its own topic branch. In this way, every additional adjustments to the original pull request might be done easily, and squashed with `git rebase -i`. The updated branch will be visible in the same pull request, so there will be no need to open new pull requests when there are changes to be applied.
