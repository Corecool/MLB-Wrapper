-module(monitor_tests).

-include_lib("eunit/include/eunit.hrl").

monitor_init_test_() ->
    {spawn,
     {setup,
      fun() ->
	      monitor:start_link()
      end,
      fun(_) ->
	      monitor:stop()
      end,
      [?_assertEqual(0,monitor:get_cache_miss()),
       ?_assertEqual(0,monitor:get_res_miss()),
       ?_assertEqual(0,monitor:get_notify_count())]
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
      [?_assertEqual(2,monitor:get_cache_miss()),
       ?_assertEqual(1,monitor:get_res_miss()),
       ?_assertEqual(1,monitor:get_notify_count())]
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
      [?_assertEqual(0,monitor:get_cache_miss()),
       ?_assertEqual(0,monitor:get_res_miss()),
       ?_assertEqual(1,monitor:get_notify_count())]
     }
    }.


