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

##### Simple Query

```erlang
pgpool:squery(DatabaseName, Sql) -> Result

Types:
  DatabaseName = atom()
  Sql = string() | iodata()
  Result = (see epgsql for reply/error types)
```

For example:

```erlang
pgpool:squery(db1_name, "SELECT * FROM users;").
```

##### Extended Query

```erlang
pgpool:equery(DatabaseName, Statement, Params) -> Result

Types:
  DatabaseName = atom()
  Statement = string()
  Params = list()
  Result = (see epgsql for reply/error types)
```

For example:

```erlang
pgpool:equery(db1_name, "SELECT * FROM users WHERE id = $1;", [3]).
```

##### Transaction

```erlang
pgpool:transaction(DatabaseName, Fun) -> (see epgsql for reply/error types)

Types:
  DatabaseName = atom()
  Fun = fun()
```

For example:

```erlang
pgpool:transaction(db1_name,
                   fun(C) ->
                           pgpool:squery(C, "DELETE FROM users WHERE foo = true"),
                           pgpool:squery(C, "UPDATE users SET bar = 100"),
                           pgpool:squery(C, "SELECT * FROM users")
                   end).
```

## Contributing
So you want to contribute? That's great! Please follow the guidelines below. It will make it easier to get merged in.

Before implementing a new feature, please submit a ticket to discuss what you intend to do. Your feature might already be in the works, or an alternative implementation might have already been discussed.

Do not commit to master in your fork. Provide a clean branch without merge commits. Every pull request should have its own topic branch. In this way, every additional adjustments to the original pull request might be done easily, and squashed with `git rebase -i`. The updated branch will be visible in the same pull request, so there will be no need to open new pull requests when there are changes to be applied.
