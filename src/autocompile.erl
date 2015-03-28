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
    lager:info("Started"),
    {ok, #state{}}.

handle_call(Msg, _From, State) ->
    lager:error("Unexpected call: ~p", [Msg]),
    {reply, {error, unexpected_call}, State}.

handle_cast(Msg, State) ->
    lager:error("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info({_, {fs, file_event}, {Path, Events}}, State) ->
    lager:debug("File: ~s\n"
                "Events: ~p", [Path, Events]),
    case lists:member(modified, Events) of
        true ->
            lager:debug("File ~s was modified", [Path]),
            is_erl_file(Path) andalso recompile(Path);
        false -> ok
    end,
   {noreply, State};

handle_info(Msg, State) ->
    lager:error("Unexpected message: ~p", [Msg]),
    {noreply, State}.

terminate(Reason,  _State) ->
    lager:info("terminate: ~p", [Reason]).

code_change(_OldVsn, State, _Extra) ->
    lager:info("code_change"),
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
    lager:debug("Recompiling module ~w", [Module]),
    CO2 = kw_replace(outdir, filename:join(LibDir, "ebin"), CO),
    lager:debug("Compile options: ~p", [CO2]),
    case c:c(Path, CO2) of
        {ok, Module} ->
            lager:info("Module ~w was sucessfully recompiled", [Module]),
            ok;
        error ->
            lager:info("Module ~w could not be compiled", [Module]),
            error
    end.

extract_lib_dir_and_module_name(Path) ->
    {ok, RE} = re:compile("(.*)/src/([\\w\\d]+)\\.erl"),
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

kw_replace(Key, Value, Proplist) ->
    [{Key, Value} | proplists:delete(Key, Proplist)].

