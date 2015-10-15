-type metric_name() ::          string() | list() | binary() | atom().
-type metric_value() ::         number().
-type metric_timestamp() ::     non_neg_integer().
-type metric_type() ::          'counter' | 'gauge' | 'meter'.

-record(metric, {name ::        {metric_name(), metric_timestamp()},
                 value ::       metric_value(),
                 type ::        metric_type()
}).

-type metric() ::               #metric{}.


-type metric_map() ::           [metric_spec()].

-type metric_spec() ::          {metric_name(), {F :: fun()                                         }, metric_type()} |
                                {metric_name(), {F :: fun(),                            E :: atom() }, metric_type()} |
                                {metric_name(), {M :: atom(), F :: atom()                           }, metric_type()} |
                                {metric_name(), {M :: atom(), F :: atom(), A :: list()              }, metric_type()} |
                                {metric_name(), {M :: atom(), F :: atom(), A :: list(), E :: atom() }, metric_type()}.

-type inliner_name() ::         string() | list() | binary() | atom().
-type inliner_value() ::        string() | list() | binary() | atom().
-type inliner() ::              {inliner_name(), inliner_value()}.

-type vm_metric_name() ::       'memory' | 'io' | 'run_queue' | 'process_count' | 'reductions' | 'garbage_collection' | 'context_switches'.

-type len() ::                  non_neg_integer().
-type word() ::                 non_neg_integer().

-type memory() ::               [{'total' |
                                  'processes' |
                                  'processes_used' |
                                  'system' |
                                  'atom' |
                                  'atom_used' |
                                  'binary' |
                                  'code' |
                                  'ets',                byte()}].

-type io() ::                   [{'input' |
                                  'output',             byte()}].

-type run_queue() ::            [{'run_queue',          len()}].

-type process_count() ::        [{'process_count',      len()}].

-type reductions() ::           [{'reductions',         len()}].

-type garbage_collection() ::   [{'gc_count' |
                                  'words_reclaimed',    len()}].

-type context_switches() ::     [{'context_switches',   len()}].

-type process_info() ::         [{'pid' |
                                  'binary' |
                                  'garbage_collection' |
                                  'memory' |
                                  'stack_size' |
                                  'heap_size' |
                                  'total_heap_size' |
                                  'message_queue_len',  pid() | byte() | word() | len()}].

-define(MS, 1000).
-define(MCS, 1000000).

-define(TAB, metronome_metrics).

-define(METRICS, [counter, gauge, meter]).
