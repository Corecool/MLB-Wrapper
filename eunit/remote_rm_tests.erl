-module(remote_rm_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").

rm_monitor_test_() ->
    {spawn,
     {setup,
      fun() ->
	      monitor:start_link(),
	      remote_rm:start_link()
      end,
      fun(_) ->
	      remote_rm:stop(),
	      monitor:stop()
      end,
      ?_test(
	 begin
	     Res16 = #resource{id = 16},
	     Reply16 = remote_rm:find_res(Res16),
	     ?assertEqual(#resource{id = 16},
			  Reply16),
	     Res1001 = #resource{id = 1001},
	     Reply1001 = remote_rm:find_res(Res1001),
	     ?assertEqual(notexist,Reply1001),
	     ?assertEqual(1,monitor:get_res_miss())
	 end
	)
     }
    }.
