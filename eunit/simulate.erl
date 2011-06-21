-module(simulate).

-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").

-define(CACHESIZE,300).
-define(LIRPERCENT,0.8).
-define(RANGE,1200).
-define(REQS,5000).

wait(MS) ->
    timer:sleep(MS).

single_client_simulate_test_() ->
    {spawn,
     {setup,
      fun() ->
	      monitor:start_link(),
	      remote_rm:start_link(),
	      lirs:start_link(?CACHESIZE,?LIRPERCENT),
	      cache:start_link(),
	      client:start_link(),
	      client:make_random_requests(?RANGE,?REQS)	  
      end,
      fun(_) ->
	      client:stop(),
	      cache:stop(),
	      lirs:stop(),
	      remote_rm:stop(),
	      monitor:stop(),
	      wait(500)
      end,
      {timeout,60,
       ?_test(
	  begin
	      Resources = client:simulate(),
	      ?assertEqual(?REQS,length(Resources)),
	      ?assertEqual(
		 0,length(client:get_random_requests())),
	      wait(3000)
	  end)
      }
     }
    }.
