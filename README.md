# autocompile

**`autocompile` automatically recompiles your Erlang modules for you.**

Just start the application and it will watch for file modifications in your current directory.
Modules will be recompiled with the same compile options.

```erlang
application:ensure_all_started(autocompile).
```

When starting, you should see this message:

```
[autocompile] [info]: Started
```

When a module has been recompiled, you should see this message: 

```
[autocompile] [info]: Module my_module was sucessfully recompiled
```

## Requirements:

* For Linux: `inotify-tools`
  - `sudo apt-get install inotify-tools`

## Bonus for Erlang beginners

You'll need to have `autocompile` and its dependency `fs` in your [Erlang code path][code-path].
I'd suggest that you create a directory named `erl_libs` in your home directory and use [`rebar`][rebar]
to fetch and compile `autocompile`.
Then add the `lib` directory to the environment variable `ERL_LIBS`.

`~/erl_libs/rebar.config`:

```erlang
{deps_dir, "lib"}.

{ deps
, [
    { autocompile, ""
    , {git, "https://github.com/JYZR/autocompile.git", {branch, "master"}}
    }
  ]
}.
```

```
rebar get-deps compile
```

Set `ERL_LIBS`:

```bash
if [ -z $ERL_LIBS ]; then
    export ERL_LIBS=~/erl_libs/lib
else
    export ERL_LIBS=~/erl_libs/lib:$ERL_LIBS
fi
```

If you're as lazy as me, you could also make sure that `autocompile` is automatically started.
Add a file name `.erlang` to your home directory and it will be evaluated every time Erlang is started.
If you don't always want to start it, check the node name first.

`~/.erlang`:

```erlang
case node() of
  my_node@localhost -> application:ensure_all_started(autocompile);
  _                 -> ok
end.
```

## First Aid

### `inotify` watches limit [Linux]

```
Failed to watch /home/jyzr/my_app; upper limit on inotify watches reached!
Please increase the amount of inotify watches allowed per user via '/proc/sys/fs/inotify/max_user_watches'.
```

**How to increase the value:**

```
sudo sysctl fs.inotify.max_user_watches=32768
```

<!-- Links -->

[code-path]:  http://www.erlang.org/doc/man/code.html#id104169
[rebar]:      https://github.com/rebar/rebar
