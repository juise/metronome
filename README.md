metronome
---------

Small, fast and sexy metrics processing system

Usage
-----

For creation or updating of a metrics just a call the update/3 function of the metronome module, having specified in parameters a name of a metric (Name), value (Value) and type (Type: counter, meter or gauge) or just use alias functions: update_counter/2, update_meter/2 or update_gauge/2 with the same set of parameters except the type:

```
1> metronome:update(<<”foo.bar.baz”>>, 1, counter).
 true
2> metronome.update_counter(<<”foo.bar.baz”>>, 1).
 true
 …
 and after 120 seconds
 …
3> metronome:update_counter(<<”foo.bar.baz”>>, 1).
 true
```

For viewing values of a metric <<”foo.bar.baz”>> just a call the get/2 function of the metronome module, having specified in parameters a name of a metric (Name) and type (Type: counter, meter or gauge) or just use alias functions: get_counter/1, get_meter/1 or get_gauge/1 with the same set of parameters except the type:

```
1> metronome:get(<<”foo.bar.baz”>>, counter).
 [#metric{name = {<<”foo.bar.baz”>>, 1443528270}, value = 2, type = counter},
  #metric{name = {<<”foo.bar.baz”>>, 1443528390}, value = 1, type = counter}]
2> metronome:get_counter(<<”foo.bar.baz”>>).
 [#metric{name = {<<”foo.bar.baz”>>, 1443528270}, value = 2, type = counter},
  #metric{name = {<<”foo.bar.baz”>>, 1443528390}, value = 1, type = counter}]
```

For viewing the accumulated metrics of a certain type just a call the get/1 function of the metronome module, having specified in parameters metric type (Type: counter, meter or gauge) or just use alias functions: get_counter/0, get_meter/0 or get_gauge/0:

```
1> metronome:get(counter).
 [#metric{name = {<<”foo.bar.baz”>>, 1443528270}, value = 2, type = counter},
  #metric{name = {<<”foo.bar.baz”>>, 1443528390}, value = 1, type = counter},
  #metric{name = {“foo.bar.baz”,     1443528780}, value = 3, type = counter},
  #metric{name = {<<”foo.bar”>>,     1443528780}, value = 9, type = counter}]
```

For remove the metric with name <<”foo.bar.baz”>> just a call the delete/2 function of the metronome module, having specified in parameters a name of a metric (Name) and type (Type: counter, meter or gage) or just use alias functions: delete_counter/1, delete_meter/1 or delete_gauge/1. As result the function return the count of removed metrics. For removal all metrics of the some type just a call the delete/1 function of the metronome module, having specified in parameters metric type (Type: counter, meter or gauge):

```
1> metronome:delete(<<”foo.bar.baz”>>, counter).
 2
2> metronome:delete(counter).
 2
3> metronome:delete(meter).
 0
```

Configuration
-------------

The sample basic metronome configuration, sys.config:

```
{metronome, [
    {period, 10000},
    {inline, [{“%local%”, “local”},
              {“%global%”, “global”}]},
  
    {graphite_host, “127.0.0.1”},
    {graphite_port, 2003},
    
    {predefined, [{“%local%.%node%.erlang.memory.ets.gauge”, {erlang, memory, [], ets}, gauge},
                  …
                  {“%local%.%node%.erlang.processes.gauge”, {erlang, system_info, [process_count]}, gauge},
                  …
                  {“%global%.%node%.erlang.gc.meter”, {metronome, system_status, [garbadge_collection], gc_count}, meter},
                  …
                  {“%local%.%ode%.erlang.lhttpc.conn.gauge”, {myapp, lhttpc_connections_cnt, []}, gauge},
                  …
                  {“%local%.%node%.erlang.cowboy.conn.gauge”, {fun() -> ranch_server:count_connections(http) end}, gauge}
    ]}
]}
```

Here parameter “period” defines a time interval (in milliseconds) through which metronome will transfer the accumulated metrics to target system. The “graphite_host” and “graphite_port” parameters define a host and port of target system respectively. Parameter “inline” allows to do any user substitutions in names of metrics. For substitution of a name of a host just use library substitution %node%. The predefined parameter allows to define metrics which values will automatically gathered by any user functions with the interval defined in the period parameter. The format describing a metrics in record “predefined” is the following:

```
{Name, {F}, Type}
{Name, {F, E}, Type}
```

or

```
{Name, {M, F, A}, Type}
{Name, {M, F, A, E}, Type}
```

where Name is metric name, M — module, F — function, A — parameters and E is proplist key, the which value should be used, if function F returns proplist.

By example, the function lhttpc_connections_cnt in myapp module may look like:

```
lhttpc_connections_cnt() ->
   Pids = [Pid || {_, Pid, _, _} <-supervisor:which_children(whereis(lhttpc_sup))],
   lists:foldl(fun(Pid, Conns) ->
                    try element(7, sys:get_state(Pid)) of
                        TableId ->
                            ets:info(TableId, size) + Conns
                        catch
                            _C:_R ->
                                Conns
                    end
               end, 0, Pids).
```


For more details see also [this article](https://medium.com/@askjuise/metronome-efac2a2bc550) about metronome.
