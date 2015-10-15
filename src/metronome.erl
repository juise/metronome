-module(metronome).

-compile({no_auto_import, [get/1]}).

%% API
-export([start/0,
         stop/0]).

-export([get/1,
         get/2,
         get_counter/0,
         get_counter/1,
         get_gauge/0,
         get_gauge/1,
         get_meter/0,
         get_meter/1]).

-export([update/3,
         update_counter/2,
         update_gauge/2,
         update_meter/2]).

-export([delete/1,
         delete/2,
         delete_counter/1,
         delete_gauge/1,
         delete_meter/1]).

-export([system_status/0,
         system_status/1,
         process_status/1,
         processes_status/0]).

-define(COUNT, 5).

-include("metronome.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% ===================================================================
%% Application API
%% ===================================================================

-spec start() -> 'ok'.
start() ->
    {ok, _} = application:ensure_all_started(metronome),
    ok.

-spec stop() -> 'ok'.
stop() ->
    ok = application:stop(metronome).

%% ===================================================================
%% API functions
%% ===================================================================

-spec get_counter() -> [metric()].
get_counter() ->
    get(counter).

-spec get_counter(metric_name()) -> [metric()].
get_counter(Name) ->
    get(Name, counter).

-spec get_gauge() -> [metric()].
get_gauge() ->
    get(gauge).

-spec get_gauge(metric_name()) -> [metric()].
get_gauge(Name) ->
    get(Name, gauge).

-spec get_meter() -> [metric()].
get_meter() ->
    get(meter).

-spec get_meter(metric_name()) -> [metric()].
get_meter(Name) ->
    get(Name, meter).

-spec get(metric_type()) -> [metric()].
get(Type) ->
    Timestamp = metronome_util:timestamp(),
    ets:select(?TAB, ets:fun2ms(fun(#metric{name = {_, MetricTimestamp}, type = MetricType} = Metric) when Type == MetricType andalso
                                                                                                           Timestamp >= MetricTimestamp -> Metric end)).

-spec get(metric_name(), metric_type()) -> [metric()].
get(Name, Type) ->
    Timestamp = metronome_util:timestamp(),
    ets:select(?TAB, ets:fun2ms(fun(#metric{name = {MetricName, MetricTimestamp}, type = MetricType} = Metric) when Type == MetricType andalso
                                                                                                                    Name == MetricName andalso
                                                                                                                    Timestamp >= MetricTimestamp -> Metric end)).


-spec update(metric_name(), metric_value(), metric_type()) -> boolean().
update(Name, Value, counter) ->
    update_counter(Name, Value);

update(Name, Value, gauge) ->
    update_gauge(Name, Value);

update(Name, Value, meter) ->
    update_meter(Name, Value).

-spec update_counter(metric_name(), metric_value()) -> boolean().
update_counter(Name, Value) ->
    Timestamp = metronome_util:timestamp(metronome_util:period()),
    try
        ets:update_counter(?TAB, {Name, Timestamp}, {#metric.value, Value}) >= Value
    catch
        _C:_R ->
            ets:insert_new(?TAB, #metric{name = {Name, Timestamp}, value = Value, type = counter}) orelse
            (ets:update_counter(?TAB, {Name, Timestamp}, {#metric.value, Value}) >= Value)
    end.

-spec update_gauge(metric_name(), metric_value()) -> boolean().
update_gauge(Name, Value) ->
    Timestamp = metronome_util:timestamp(metronome_util:period()),
    try
        true = ets:update_element(?TAB, {Name, Timestamp}, {#metric.value, Value})
    catch
        _C:_R ->
            ets:insert(?TAB, #metric{name = {Name, Timestamp}, value = Value, type = gauge})
    end.

-spec update_meter(metric_name(), metric_value()) -> boolean().
update_meter(Name, Value) ->
    Timestamp = metronome_util:timestamp(metronome_util:period()),
    try
        ets:update_counter(?TAB, {Name, Timestamp}, {#metric.value, Value}) >= Value
    catch
        _C:_R ->
            ets:insert_new(?TAB, #metric{name = {Name, Timestamp}, value = Value, type = meter}) orelse
            (ets:update_counter(?TAB, {Name, Timestamp}, {#metric.value, Value}) >= Value)
    end.


-spec delete_counter(metric_name()) -> non_neg_integer().
delete_counter(Name) ->
    delete(Name, counter).

-spec delete_gauge(metric_name()) -> non_neg_integer().
delete_gauge(Name) ->
    delete(Name, gauge).

-spec delete_meter(metric_name()) -> non_neg_integer().
delete_meter(Name) ->
    delete(Name, meter).

-spec delete(metric_type()) -> non_neg_integer().
delete(Type) ->
    Timestamp = metronome_util:timestamp(),
    ets:select_delete(?TAB, ets:fun2ms(fun(#metric{name = {_, MetricTimestamp}, type = MetricType} = Metric) when Type == MetricType andalso
                                                                                                                  Timestamp >= MetricTimestamp -> true end)).
-spec delete(metric_name(), metric_type()) -> non_neg_integer().
delete(Name, Type) ->
    Timestamp = metronome_util:timestamp(),
    ets:select_delete(?TAB, ets:fun2ms(fun(#metric{name = {MetricName, MetricTimestamp}, type = MetricType} = Metric) when Type == MetricType andalso
                                                                                                                           Name == MetricName andalso
                                                                                                                           Timestamp >= MetricTimestamp -> true end)).


-spec system_status() -> [memory() | io() | run_queue() | process_count() | reductions() | garbage_collection() | context_switches()].
system_status() ->
    lists:foldl(fun(Metric, Metrics) -> system_status(Metric) ++ Metrics end, [], [memory, io, run_queue, process_count, reductions, garbadge_collection, context_switches]).

-spec system_status(vm_metric_name()) -> memory() | io() | run_queue() | process_count() | reductions() | garbage_collection() | context_switches().
system_status(memory) ->
    erlang:memory();

system_status(io) ->
   {Input, Output} = erlang:statistics(io),
   [Input, Output];

system_status(run_queue) ->
    RQ = erlang:statistics(run_queue),
    [{run_queue, RQ}];

system_status(process_count) ->
    Processes = erlang:system_info(process_count),
    [{process_count, Processes}];

system_status(reductions) ->
    {Reductions, _} = erlang:statistics(reductions),
    [{reductions, Reductions}];

system_status(garbadge_collection) ->
    {GC, Words, 0} = erlang:statistics(garbage_collection),
    [{gc_count, GC}, {words_reclaimed, Words}];

system_status(context_switches) ->
    {CSW, _} = erlang:statistics(context_switches),
    [{context_switches, CSW}].

-spec processes_status() -> [{atom(), byte() | word() | len()}].
processes_status() ->
    Processes = lists:filter(fun(X) -> X =/= undefined end, [process_status(Pid) || Pid <- erlang:processes()]),
    ProcessesByMemory = lists:sort(fun(X,Y) -> proplists:get_value(memory, X) > proplists:get_value(memory, Y) orelse
                                               proplists:get_value(message_queue_len, X) > proplists:get_value(message_queue_len, Y) end, Processes),
    lists:sublist(ProcessesByMemory, ?COUNT).

-spec process_status(pid()) -> process_info() | undefined.
process_status(Pid) ->
    case process_status_ll(Pid) of
        undefined ->
            undefined;
        ProcessInfo ->
            [GC, Binary, RN] = [proplists:get_value(Metric, ProcessInfo) || Metric <- [garbage_collection, binary, registered_name]],
            ProcessInfo1 = lists:foldl(fun(Metric, Acc) -> proplists:delete(Metric, Acc) end,  ProcessInfo, [garbage_collection, binary, registered_name]),

            GC1 = case GC of
                undefined ->
                    0;
                GC ->
                    proplists:get_value(minor_gcs, GC)
            end,

            Binary1 = case Binary of
                [] ->
                    0;
                Binary ->
                    lists:foldl(fun({_, Memory, _}, Total) -> Memory + Total end, 0, Binary)
            end,

            RN1 = case RN of
                [] ->
                    Pid;
                undefined ->
                    Pid;
                Name ->
                    Name
            end,

            [{process, RN1}, {binary, Binary1}, {garbage_collection, GC1} | ProcessInfo1]
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec process_status_ll(pid()) -> process_info() | undefined.
process_status_ll(Pid) ->
    erlang:process_info(Pid, [memory,               % bytes, memory = call stack + total_heap_size + C structures impl process
                              stack_size,           % words
                              heap_size,            % words
                              total_heap_size,      % words, total_heap_size = stack + heap_size(young gen) + heap_size(old gen)
                              message_queue_len,
                              garbage_collection,
                              binary,               % bytes, binary memory referred by the process
                              registered_name]).

