-module(metronome_graphite).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([send/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {ready               :: boolean(),
                socket              :: gen_tcp:socket(),
                timeout             :: non_neg_integer(),
                reconnect_timer     :: reference()
}).

-define(RETRY_TIMEOUT, 500).

-include("metronome.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec send([metric()]) -> boolean().
send(Metrics) ->
    gen_server:call(?MODULE, {send, Metrics}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    erlang:process_flag(trap_exit, true),
    erlang:send_after(?RETRY_TIMEOUT, self(), connect),
    {ok, #state{ready = false, socket = undefined, timeout = ?RETRY_TIMEOUT, reconnect_timer = undefined}}.

handle_call({send, Metrics}, _From, #state{socket = Socket, ready = Ready} = State) when Socket == undefined orelse Ready == false ->
    {reply, false, State};

handle_call({send, Metrics}, _From, #state{socket = Socket} = State) ->
    case send(Socket, Metrics) of
        ok ->
            {reply, true, State};
        {error, Reason} ->
            erlang:send(self(), {tcp_error, Socket, Reason}),
            lager:error("Fail to send metrics into graphite due '~p'", [Reason]),
            {reply, false, State#state{ready = false}}
    end;

handle_call(Msg, _From, State) ->
    lager:error("Unknown call message: '~p'", [Msg]),
    {reply, {error, unknown_msg}, State}.

handle_cast(Msg, State) ->
    lager:error("Unknown cast message: '~p'", [Msg]),
    {noreply, State}.

handle_info({tcp_closed, Socket}, #state{socket = Socket, timeout = Timeout} = State) ->
    lager:error("Graphite connection closed by peer, try to reconnect again after ~p ms", [Timeout]),
    erlang:send_after(Timeout, self(), connect),
    {noreply, State#state{ready = false, socket = undefined, timeout = min(Timeout + 500, 10000)}};

handle_info({tcp_error, Socket, Reason}, #state{socket = Socket, timeout = Timeout} = State) ->
    lager:error("Graphite connection failed due '~p', try reconnect again after ~p ms", [Reason, Timeout]),
    erlang:send_after(Timeout, self(), connect),
    {noreply, State#state{ready = false, socket = undefined, timeout = min(Timeout + 500, 10000)}};

handle_info(connect, #state{socket=OldSocket, timeout=Timeout, reconnect_timer = RTRef} = State) ->
    catch gen_tcp:close(OldSocket),
    catch erlang:cancel_timer(RTRef),
    lager:info("Connecting to graphite"),
    try connect() of
        {ok, Socket} ->
            lager:info("Graphite connection established"),
            {noreply, State#state{ready = true, socket = Socket, timeout = 500, reconnect_timer = undefined}};
        {error, Reason} ->
            lager:error("Graphite connection failed due '~p', try reconnect again after ~p ms", [Reason, Timeout]),
            {noreply, State#state{ready = false, socket = undefined, timeout = min(Timeout + 500, 10000), reconnect_timer = erlang:send_after(Timeout, self(), connect)}}
    catch
        C:R ->
            lager:error("Graphite connection failed due '~p', try reconnect again after ~p ms", [{C, R}, Timeout]),
            {noreply, State#state{ready = false, socket = undefined, timeout = min(Timeout + 500, 10000), reconnect_timer = erlang:send_after(Timeout, self(), connect)}}
    end;

handle_info(Msg, State) ->
    lager:error("Unknown info message: '~p'", [Msg]),
    {noreply, State}.

terminate(Reason, #state{socket=Socket, reconnect_timer = RTRef} = State) ->
    catch gen_tcp:close(Socket),
    catch erlang:cancel_timer(RTRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

connect() ->
    [{ok, Host}, {ok, Port}] = [application:get_env(metronome, Env) || Env <- [graphite_host, graphite_port]],
    %% delay_send = flase -> don't queue up messages in erlang driver
    %% nodelay = true -> use TCP_NODELAY socket option for send messages immediately
    gen_tcp:connect(Host, Port, [{mode, binary}, {reuseaddr, true}, {delay_send, false}, {nodelay, true}, {send_timeout, 1000}, {send_timeout_close, true}]).

-spec send(gen_tcp:socket(), [metric()]) -> 'ok' | {'error', atom()}.
send(Socket, Metrics) ->
    gen_tcp:send(Socket, format(Metrics)).


-spec format([metric()]) -> iolist().
format(Metrics) ->
    lists:foldl(fun(#metric{name = {Name, Timestamp}, value = Value}, Acc) -> [to_list(Name), " ", to_list(Value), " ", to_list(Timestamp), "\r\n" | Acc] end, [], Metrics).

-spec to_list(float() | integer() | atom() | binary() | list()) -> list().
to_list(Value) when is_float(Value) ->
    float_to_list(Value, [{decimals, 6}, compact]);

to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);

to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);

to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);

to_list(Value) when is_list(Value) ->
    Value.

