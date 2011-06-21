-module(client_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").

wait() ->
    timer:sleep(500).

check_ramdom_seq_test_() ->
    {spawn,
     {setup,
      fun() ->
	      client:start_link(),
	      client:make_random_requests(1200,100)
      end,
      fun(_) ->
	      client:stop(),
	      wait()
      end,
      ?_test(
	  begin
	      Reqs = client:get_random_requests(),
	      ?assertEqual(100,length(Reqs)),
	      Cond = lists:any(
		       fun(X) ->
			       X#resource.id < 1 orelse
				   X#resource.id > 1200 end,
		       Reqs),
	      ?assertEqual(false,Cond)
	  end)
     }
    }.
