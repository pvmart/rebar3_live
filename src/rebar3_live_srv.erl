-module(rebar3_live_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EVENTS, [created, closed, modified, moved, deleted, is_dir, undefined,
                 removed, renamed]).            % enotify events
-define(THR, 333).                              % throttle interval
-define(POS, 2).          % position of rebar state in rebar_agent's state tuple

-record(state, {cref,cl=[],lref,ls=[]}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    Dir = rebar_dir:root_dir(element(?POS, sys:get_state(rebar_agent))),
    enotify:start_link(Dir, ?EVENTS),
    rebar_log:log(info, "Live ready", []),
    {noreply, State};
handle_info(compile, State) ->
    do_compile(),
    {noreply, State};
handle_info(load, #state{ls=LS} = State) ->
    do_load(LS),
    {noreply, State#state{ls=[]}};
handle_info({Path, _Opts}, State) ->
    State1 =
        case path_filter(Path) of
            true  -> compile(Path, State);
            load  -> load(Path, State);
            false -> State
        end,
    {noreply, State1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

path_filter(P) ->
    filelib:is_file(P)
        andalso path_filter_name(filename:basename(P))
        andalso path_filter_ext(filename:extension(P)).

path_filter_name(".#" ++ _) -> false;
path_filter_name(_)         -> true.

path_filter_ext(".beam")    -> load;
path_filter_ext(".erl")     -> true;
path_filter_ext(".dtl")     -> true;
path_filter_ext(".ex")      -> true;
path_filter_ext(_)          -> false.

throttle(Msg, ok)   -> {ok, Tref} = timer:send_after(?THR, Msg), Tref;
throttle(Msg, Tref) -> timer:cancel(Tref), throttle(Msg, ok).

compile(Path, #state{cref=Cref} = State) ->
    rebar_log:log(info, "Live change in ~p", [Path]),
    State#state{cref=throttle(compile, Cref)}.

load(Path, #state{lref=Lref, ls=LS} = State) ->
    State#state{lref=throttle(load, Lref), ls=[Path|LS]}.

do_load([]) -> ok;
do_load([L|LS]) -> load(L), do_load(LS).

load(M) ->
    Module = list_to_atom(filename:basename(M, ".beam")),
    rebar_log:log(info, "Live reload module '~p'", [Module]),
    c:l(Module).

do_compile() ->
    CompileFun = fun(AgentState) ->
                         State1 = element(?POS, AgentState),
                         {ok, State2} = safe_compile(State1),
                         code:add_pathsa(rebar_state:code_paths(State2, all_deps)),
                         setelement(?POS, AgentState, State2)
                 end,
    sys:replace_state(rebar_agent, CompileFun).

-spec safe_compile(rebar_state:t()) -> {ok, rebar_state:t()}.
safe_compile(State) ->
    try rebar_core:do([compile], State) of
        {ok, S} -> {ok, S};
        _       -> {ok, State}
    catch
        rebar_abort -> {ok, State}
    end.
