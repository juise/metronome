-module(metronome_util).

%% API
-export([period/0,
         timestamp/0,
         timestamp/1,
         inline/2,
         inliners/0,
         predefined/0]).

-include("metronome.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec period() -> non_neg_integer().
period() ->
    {ok, Period} = application:get_env(metronome, period),
    trunc(Period).

-spec timestamp() -> non_neg_integer().
timestamp() ->
    {MegaSecs, Secs, _} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

-spec timestamp(non_neg_integer()) -> non_neg_integer().
timestamp(Period) ->
    Timestamp = timestamp(),
    Timestamp - (Timestamp rem (Period div ?MS)) + (Period div ?MS).

-spec inline([metric()], [inliner()]) -> list().
inline(Metrics, Inliners) ->
    F = fun(Name) -> lists:foldl(fun({P, R}, Value) -> re:replace(Value, P, R, [global, {return, list}]) end, Name, Inliners) end,
    lists:map(fun(#metric{name = {Name, Timestamp}} = Metric) -> Metric#metric{name = {F(Name), Timestamp}} end, Metrics).

-spec inliners() -> [inliner()].
inliners() ->
    Inliners = application:get_env(metronome, inline, []),
    [{"%node%", inl_node()} | Inliners].

-spec predefined() -> metric_map().
predefined() ->
    application:get_env(metronome, predefined, []).

%% ===================================================================
%% Internal functions
%% ===================================================================

inl_node() ->
    [_, Node] = string:tokens(atom_to_list(node()), "@"),
    string:join(string:tokens(Node, "."), "_").

