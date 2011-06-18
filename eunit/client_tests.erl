-module(client_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").

-define(CACHESIZE,300).
-define(LIRPERCENT,0.8).
-define(RANGE,1200).
-define(REQS,50000).

%% check_ramdom_seq_test_() ->
%%     {spawn,
%%      {setup,
%%       fun() ->
%% 	      client:start_link(),
%% 	      client:make_random_requests(1200,100)
%%       end,
%%       fun(_) ->
%% 	      client:stop()
%%       end,
%%       ?_test(
%% 	  begin
%% 	      Reqs = client:get_random_requests(),
%% 	      ?assertEqual(100,length(Reqs)),
%% 	      Cond = lists:any(
%% 		       fun(X) ->
%% 			       X#resource.id < 1 orelse
%% 				   X#resource.id > 1200 end,
%% 		       Reqs),
%% 	      ?assertEqual(false,Cond)
%% 	  end)
%%      }
%%     }.

single_client_simulate_test_() ->
    {spawn,
     {setup,
      fun() ->
	      monitor:start_link(),
	      rm:start_link(),
	      lirs:start_link(?CACHESIZE,?LIRPERCENT),
	      cache:start_link(),
	      client:start_link(),
	      client:make_random_requests(?RANGE,?REQS)	  
      end,
      fun(_) ->
	      client:stop(),
	      cache:stop(),
	      lirs:stop(),
	      rm:stop(),
	      monitor:stop()
      end,
      {timeout,30,
       ?_test(
	  begin
	      Resources = client:simulate(),
	      ?assertEqual(?REQS,length(Resources)),
	      ?assertEqual(
		 0,length(client:get_random_requests())),
	      timer:sleep(3000)
	  end)
      }
     }
    }.


