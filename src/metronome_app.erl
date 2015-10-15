-module(metronome_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(any(), any()) -> no_return().
start(_StartType, _StartArgs) ->
    metronome_sup:start_link().

-spec stop(any()) -> 'ok'.
stop(_State) ->
    ok.

