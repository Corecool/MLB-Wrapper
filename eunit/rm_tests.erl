-module(rm_tests).
-include_lib("eunit/include/eunit.hrl").

-record(resource,{id,
		  name = "BoA",
		  description = "Best of Asia",
		  rating = "BoA, you are still my No.1"}).

create_resource(Num) when is_integer(Num) ->
    Seq = lists:seq(0,Num - 1),
    lists:foldr(fun(X,List) ->
			[#resource{id = X} | List] end,
		[],Seq).

init_resource_test_() ->
    {spawn,
     {setup,
      fun() ->
	      rm:start_link(10) end,
      fun(_) ->
	      gen_server:cast(rm,stop) end,
      [?_assertEqual(10,gen_server:call(rm,get_res_num)),
       ?_assertEqual(create_resource(10),
		     gen_server:call(rm,get_res))]
     }
    }.

add_resource_test_() ->
    {spawn,
     {setup,
      fun() ->
	      rm:start_link(0) end,
      fun(_) ->
	      gen_server:cast(rm,stop) end,
      ?_test(
	 begin
	     NewRes = #resource{id = 6},
	     gen_server:cast(rm,{add_res,NewRes}),
	     ?assertEqual(1,gen_server:call(
			      rm,get_res_num)),
	     ?assertEqual([NewRes],gen_server:call(
				   rm,get_res)),
	     gen_server:cast(rm,{add_res,NewRes}),
	     ?assertEqual(1,gen_server:call(
			      rm,get_res_num)),
	     ?assertEqual([NewRes],gen_server:call(
				   rm,get_res)),
	     AnotherRes = #resource{id = 0},
	     gen_server:cast(rm,{add_res,AnotherRes}),
	     ?assertEqual(2,gen_server:call(
			      rm,get_res_num)),
	     ?assertEqual([AnotherRes | [NewRes]],
			  gen_server:call(rm,get_res))
	 end
	)
     }
    }.
		
		      
		  
