-module('rebar3_live').

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = 'rebar3_live_prv':init(State),
    {ok, State1}.
