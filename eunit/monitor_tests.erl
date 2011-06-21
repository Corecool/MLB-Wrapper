-module(monitor_tests).

-include_lib("eunit/include/eunit.hrl").

wait() ->
    timer:sleep(500).

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
       ?_assertEqual(0,monitor:get_client_cache_miss()),
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
	      monitor:inc_client_cache_miss(),
	      monitor:inc_cache_miss(),
	      monitor:inc_notify(),
	      wait()
      end,
      fun(_) ->
	      monitor:stop(),
	      wait()
      end,
      [?_assertEqual(2,monitor:get_cache_miss()),
       ?_assertEqual(1,monitor:get_res_miss()),
       ?_assertEqual(1,monitor:get_client_cache_miss()),
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
	      monitor:inc_client_cache_miss(),
	      monitor:reset_counter(),
	      monitor:inc_client_cache_miss(),
	      monitor:inc_notify(),
	      wait()
      end,
      fun(_) ->
	      monitor:stop()
      end,
      [?_assertEqual(0,monitor:get_cache_miss()),
       ?_assertEqual(0,monitor:get_res_miss()),
       ?_assertEqual(1,monitor:get_client_cache_miss()),
       ?_assertEqual(1,monitor:get_notify_count())]
     }
    }.


