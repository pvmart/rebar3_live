-module('rebar3_live_prv').

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'live').
-define(DEPS, [shell]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    LiveStart =
        fun(LiveStart) ->
                timer:send_after(200, check),
                receive
                    check ->
                        case whereis(rebar_agent) of
                            undefined -> LiveStart(LiveStart);
                            _Pid -> application:ensure_all_started(rebar3_live)
                        end
                end
        end,
    spawn(fun() -> LiveStart(LiveStart) end),
    ShellProviderOpts =
        [{config, undefined, "config", string,
          "Path to the config file to use. Defaults to the "
          "sys_config defined for relx, if present."},
         {name, undefined, "name", atom,
          "Gives a long name to the node."},
         {sname, undefined, "sname", atom,
          "Gives a short name to the node."}],
    ProviderOpts =
        [{name, ?PROVIDER},            % The 'user friendly' name of the task
         {module, ?MODULE},            % The module implementation of the task
         {bare, true},                 % The task can be run by the user, always true
         {deps, ?DEPS},                % The list of dependencies
         {example, "rebar3 rebar3_live"}, % How to use the plugin
         {opts, ShellProviderOpts},    % list of options understood by the plugin
         {short_desc, "A rebar plugin"},
         {desc, "A rebar plugin"}],
    Provider = providers:create(ProviderOpts),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
