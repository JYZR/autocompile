-module(autocompile).

-behaviour(gen_server).

%% API
-export(
 [
    start_link/0
 ]).

%% gen_server callbacks
-export(
 [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
 ]).

%%%=============================================================================
%%% Type definitions
%%%=============================================================================

%%%=============================================================================
%%% Record definitions
%%%=============================================================================

-record(state,
        {
        }).

%%%=============================================================================
%%% Macro definitions
%%%=============================================================================

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init(_Args) ->
    fs:subscribe(),
    p_info("Started"),
    {ok, #state{}}.

handle_call(Msg, _From, State) ->
    p_error("Unexpected call: ~p", [Msg]),
    {reply, {error, unexpected_call}, State}.

handle_cast(Msg, State) ->
    p_error("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({_, {fs, file_event}, {Path, Events}}, State) ->
    p_debug("File: ~s\n"
                "Events: ~p", [Path, Events]),
    case lists:member(modified, Events) of
        true ->
            p_debug("File ~s was modified", [Path]),
            is_erl_file(Path) andalso recompile(Path);
        false -> ok
    end,
   {noreply, State};

handle_info(Msg, State) ->
    p_error("Unexpected message: ~p", [Msg]),
    {noreply, State}.

terminate(Reason,  _State) ->
    p_info("terminate: ~p", [Reason]).

code_change(_OldVsn, State, _Extra) ->
    p_info("code_change"),
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

is_erl_file(Path) ->
    string:substr(Path, string:len(Path) - 3) =:= ".erl".

recompile(Path) ->
    {LibDir, ModuleName} = extract_lib_dir_and_module_name(Path),
    Module = list_to_atom(ModuleName),
    CO = get_prev_compile_options(Module),
    p_debug("Recompiling module ~w", [Module]),
    {ok, TopDir} = file:get_cwd(),
    c:cd(LibDir),
    p_debug("Compile options: ~p", [CO]),
    case c:c(Path, CO) of
        {ok, Module} ->
            p_info("Module ~w was successfully recompiled", [Module]),
            c:cd(TopDir),
            ok;
        error ->
            p_info("Module ~w could not be compiled", [Module]),
            c:cd(TopDir),
            error
    end.

extract_lib_dir_and_module_name(Path) ->
    {ok, RE} = re:compile("(.*)/(?:src|test)/([\\w\\d]+)\\.erl"),
    {match, Matches} = re:run(Path, RE),
    [ {0, _}
    , {LibDirStart, LibDirLength}
    , {ModStart, ModLength}
    ] = Matches,
    { string:substr(Path, LibDirStart+1, LibDirLength)
    , string:substr(Path, ModStart+1, ModLength)
    }.

get_prev_compile_options(Module) ->
    MI = Module:module_info(),
    kw_get(options, kw_get(compile, MI)).

kw_get(Key, Proplist) ->
    case proplists:lookup(Key, Proplist) of
        {Key, Value} -> Value;
        none -> none
    end.

p_debug(Format) ->
    case application:get_env(autocompile, debug) of
        {ok, true} ->
            io:format("[autocompile] [debug]: " ++ Format ++ "\n");
        _ ->
            ok
    end.

p_debug(Format, Data) ->
    case application:get_env(autocompile, debug) of
        {ok, true} ->
            io:format("[autocompile] [debug]: " ++ Format ++ "\n", Data);
        _ ->
            ok
    end.

p_info(Format) ->
    case application:get_env(autocompile, info) of
        {ok, true} ->
            io:format("[autocompile] [info]: " ++ Format ++ "\n");
        _ ->
            ok
    end.

p_info(Format, Data) ->
    case application:get_env(autocompile, info) of
        {ok, true} ->
            io:format("[autocompile] [info]: " ++ Format ++ "\n", Data);
        _ ->
            ok
    end.

p_error(Format) ->
    case application:get_env(autocompile, error) of
        {ok, true} ->
            io:format("[autocompile] [error]: " ++ Format ++ "\n");
        _ ->
            ok
    end.

p_error(Format, Data) ->
    case application:get_env(autocompile, error) of
        {ok, true} ->
            io:format("[autocompile] [error]: " ++ Format ++ "\n", Data);
        _ ->
            ok
    end.
