-module(client_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").

check_ramdom_seq_test_() ->
    {spawn,
     {setup,
      fun() ->
	      client:start_link(),
	      client:make_random_requests(1200,500)
      end,
      fun(_) ->
	      client:stop()
      end,
      ?_test(
	  begin
	      Reqs = client:get_random_requests(),
	      ?assertEqual(500,length(Reqs)),
	      Cond = lists:any(
		       fun(X) ->
			       X#resource.id < 1 orelse
				   X#resource.id > 1200 end,
		       Reqs),
	      ?assertEqual(false,Cond)
	  end)
     }
    }.

single_client_simulate_test_() ->
    {spawn,
     {setup,
      fun() ->
	      rm:start_link(),
	      lirs:start_link(100,0.6),
	      cache:start_link(),
	      client:start_link(),
	      client:make_random_requests(1200,10000)	  
      end,
      fun(_) ->
	      client:stop(),
	      cache:stop(),
	      lirs:stop(),
	      rm:stop()
      end,
      {timeout,30,
       ?_test(
	  begin
	      Resources = client:simulate(),
	      ?assertEqual(10000,length(Resources)),
	      ?assertEqual(
		 0,length(client:get_random_requests())),
	      timer:sleep(5000)
	  end)
      }
     }
    }.


