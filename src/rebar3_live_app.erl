-module(rebar3_live_app).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% ------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, rebar3_live_sup}, ?MODULE, []).

stop(_State) ->
    ok.

%% ------------------------------------------------------------------
init([]) ->
    {ok, {{rest_for_one, 5, 10},
          [{rebar3_live,
            {rebar3_live_srv, start_link, []},
            permanent, 5000, worker, [rebar3_live_srv]}]}}.
