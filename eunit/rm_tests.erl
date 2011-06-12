-module(rm_tests).
-include_lib("eunit/include/eunit.hrl").

-record(resource,{id,
		  name = "BoA",
		  description = "Best of Asia",
		  rating = "BoA, you are still my No.1"}).



update_resource_test_() ->
    {spawn,
     {setup,
      fun() ->
	      {ok,Pid} = rm:start_link(),
	      gen_server:cast(
		rm,{update_res,
		    #resource{id = 16,name = "Ayumi"}}),
	      gen_server:cast(
		rm,{remove_res,
		    #resource{id = 13,name = "None"}}),
	      Pid
      end,
      fun(_Pid) ->
	      gen_server:cast(rm,stop)
      end,
      ?_test(
	 begin
	     Res16 = #resource{id = 16},
	     Reply16 = gen_server:call(rm,{find_res,Res16}),
	     ?assertEqual(#resource{id = 16,name = "Ayumi"},
			  Reply16),
	     Res13 = #resource{id = 13},
	     Reply13 = gen_server:call(rm,{find_res,Res13}),
	     ?assertEqual(notexist,Reply13),
	     gen_server:cast(rm,reload_res),
	     timer:sleep(200),
	     ?assertEqual(#resource{id = 16,name = "BoA"},
			 gen_server:call(
			   rm,{find_res,Res16})),
	     ?assertEqual(#resource{id = 13},
			  gen_server:call(
			    rm,{find_res,#resource{id = 13}}))
	 end
	)
     }
    }.



      
			     
			      

    
		
		      
		  
