-module(rebar3_tsung).

-export([init/1]).

-define(PLUGIN_MODS, [
    rebar3_tsung_prv
]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    lists:foldl(
        fun(Module, {ok, StateAcc}) ->
            Module:init(StateAcc)
        end, {ok, State}, ?PLUGIN_MODS).
