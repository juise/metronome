-module(metronome_core).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {send_timer :: reference(),
                inliners   :: [inliner()]
}).

-include("metronome.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    erlang:process_flag(trap_exit, true),
    ?TAB = ets:new(?TAB, [named_table, ordered_set, public, {keypos, #metric.name}, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, #state{send_timer = start_interval(), inliners = metronome_util:inliners()}}.

handle_call(Msg, _From, State) ->
    lager:error("Unknown call message: '~p'", [Msg]),
    {reply, {error, unknown_msg}, State}.

handle_cast(Msg, State) ->
    lager:error("Unknown cast message: '~p'", [Msg]),
    {noreply, State}.

handle_info(send, #state{send_timer = TRef, inliners = Inliners} = State) ->
    catch erlang:cancel_timer(TRef),
    ok = update_predefined(),
    [metronome_graphite:send(metronome_util:inline(metronome:get(Type), Inliners)) andalso metronome:delete(Type) || Type <- ?METRICS],
    {noreply, State#state{send_timer = start_interval()}};

handle_info(Msg, State) ->
    lager:error("Unknown info message: '~p'", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{send_timer = TRef, inliners = Inliners} = State) ->
    catch erlang:cancel_timer(TRef),
    ok = update_predefined(),
    [metronome_graphite:send(metronome_util:inline(metronome:get(Type), Inliners)) andalso metronome:delete(Type) || Type <- ?METRICS],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec start_interval() -> reference().
start_interval() ->
    erlang:send_after(metronome_util:period(), self(), send).

update_predefined() ->
    F = fun(Name, MFA, Type) ->
            try execute(MFA) of
                Value when is_float(Value) orelse is_integer(Value) ->
                    metronome:update(Name, execute(MFA), Type);
                Value ->
                    lager:error("Fail to harvest predefined metric '~p', '~p' return bad value '~p'", [Name, MFA, Value]),
                    false
            catch
                C:R ->
                    lager:error("Fail to harvest predefined metric '~p', '~p' return exception '~p'", [Name, MFA, {C, R}]),
                    false
            end
        end,
    [F(Name, MFA, Type) || {Name, MFA, Type} <- metronome_util:predefined()],
    ok.

execute({F}) when is_function(F, 0) ->
    erlang:apply(F, []);
execute({F, E}) when is_function(F, 0) andalso is_atom(E) ->
    proplists:get_value(E, erlang:apply(F, []));
execute({M, F}) when is_atom(M) andalso is_atom(F) ->
    execute({M, F, []});
execute({M, F, E}) when is_atom(M) andalso is_atom(F) andalso is_atom(E) ->
    execute({M, F, [], E});
execute({M, F, A}) when is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    erlang:apply(M, F, A);
execute({M, F, A, E}) when is_atom(M) andalso is_atom(F) andalso is_list(A) andalso is_atom(E) ->
    proplists:get_value(E, erlang:apply(M, F, A)).

