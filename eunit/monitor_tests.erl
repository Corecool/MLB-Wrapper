-module(monitor_tests).

-include_lib("eunit/include/eunit.hrl").

lookup(Key,Pos) ->
    ets:lookup_element(monitorTab,Key,Pos).
    
monitor_init_test_() ->
    {spawn,
     {setup,
      fun() ->
	      monitor:start_link()
      end,
      fun(_) ->
	      monitor:stop()
      end,
      [?_assertEqual(0,lookup(cacheMiss,2)),
       ?_assertEqual(0,lookup(resMiss,2)),
       ?_assertEqual(0,lookup(notify,2))]
     }
    }.

sample_test_() ->
    {spawn,
     {setup,
      fun() ->
	      monitor:start_link(),
	      monitor:inc_cache_miss(),
	      monitor:inc_res_miss(),
	      monitor:inc_cache_miss(),
	      monitor:inc_notify(),
	      timer:sleep(500)
      end,
      fun(_) ->
	      monitor:stop()
      end,
      [?_assertEqual(2,lookup(cacheMiss,2)),
       ?_assertEqual(1,lookup(resMiss,2)),
       ?_assertEqual(1,lookup(notify,2))]
     }
    }.

reset_test_() ->
    {spawn,
     {setup,
      fun() ->
	      monitor:start_link(),
	      monitor:inc_cache_miss(),
	      monitor:inc_res_miss(),
	      monitor:inc_cache_miss(),
	      monitor:inc_notify(),
	      monitor:reset_counter(),
	      monitor:inc_notify(),
	      timer:sleep(500)
      end,
      fun(_) ->
	      monitor:stop()
      end,
      [?_assertEqual(0,lookup(cacheMiss,2)),
       ?_assertEqual(0,lookup(resMiss,2)),
       ?_assertEqual(1,lookup(notify,2))]
     }
    }.
