-module(rm_tests).
-include_lib("eunit/include/eunit.hrl").

-record(resource,{id,
		  name = "BoA",
		  description = "Best of Asia",
		  rating = "BoA, you are still my No.1"}).

reload_resource() ->
    Seq = lists:seq(0,9),
    lists:foldr(fun(X,List) ->
			[#resource{id = X} | List]
		end,
		[],Seq).

init_resource_test_() ->
    {spawn,
     {setup,
      fun() ->
	      rm:start_link() end,
      fun(_) ->
	      gen_server:cast(rm,stop) end,
      [?_assertEqual(10,gen_server:call(rm,get_res_num)),
       ?_assertEqual(reload_resource(),
		     gen_server:call(rm,get_res))]
     }
    }.

add_resource_test_() ->
    {spawn,
     {setup,
      fun() ->
	      rm:start_link(),
	      gen_server:cast(rm,{add_res,#resource{id = 16}})
      end,
      fun(_) ->
	      gen_server:cast(rm,stop) end,
      ?_test(
	 begin
	     ReloadRes = reload_resource(),
	     ExistRes = #resource{id = 16},
	     ?assertEqual(11,gen_server:call(
			      rm,get_res_num)),
	     ?assertEqual([ExistRes| ReloadRes],
			  gen_server:call(rm,get_res)),
	     gen_server:cast(rm,{add_res,ExistRes}),
	     ?assertEqual(11,gen_server:call(
			      rm,get_res_num)),
	     ?assertEqual([ExistRes| ReloadRes],
			  gen_server:call(rm,get_res)),
	     AnotherRes = #resource{id = 18},
	     gen_server:cast(rm,{add_res,AnotherRes}),
	     ?assertEqual(12,gen_server:call(
			      rm,get_res_num)),
	     ?assertEqual([AnotherRes,ExistRes|ReloadRes],
			  gen_server:call(rm,get_res))
	 end
	)
     }
    }.

find_resource_test_() ->
    {spawn,
     {setup,
      fun() ->
	      rm:start_link() end,
      fun(_) ->
	      gen_server:cast(rm,stop) end,
      [?_assertEqual(#resource{id = 1},
		     gen_server:call(rm,{find_res,
					 #resource{id = 1}})),
       ?_assertEqual(notexist,
		     gen_server:call(rm,{find_res,
					 #resource{id = 10}}))
      ]
     }
    }.
		    
remove_resource_test_() ->			      
    {spawn,
     {setup,
      fun() ->
	      rm:start_link() end,
      fun(_) ->
	      gen_server:cast(rm,stop) end,
      ?_test(
	 begin
	     ?assertEqual(10,
			  gen_server:call(rm,get_res_num)),
	     gen_server:cast(rm,{remove_res,
				 #resource{id = 6}}),
	     ?assertEqual(9,
			  gen_server:call(rm,get_res_num)),
	     ?assertEqual(
		notexist,
		gen_server:call(rm,{find_res,
				    #resource{id = 6}})),
	     gen_server:cast(rm,{remove_res,
				 #resource{id = 6}}),
	     ?assertEqual(9,
			  gen_server:call(rm,get_res_num))
	 end)
     }
    }.

reload_resource_test_() ->	     
    {spawn,
     {setup,
      fun() ->
	      rm:start_link() end,
      fun(_) ->
	      gen_server:cast(rm,stop) end,
      ?_test(
	 begin
	     gen_server:cast(rm,{remove_res,
				 #resource{id = 6}}),
	     gen_server:cast(rm,{add_res,
				 #resource{id = 12}}),
	     gen_server:cast(rm,reload_res),
	     ?assertEqual(reload_resource(),
			  gen_server:call(rm,get_res))
	 end
	)
     }
    }.


      
			     
			      

    
		
		      
		  
