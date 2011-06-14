-module(rm_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").

update_resource_test_() ->
    {spawn,
     {setup,
      fun() ->
	      {ok,Pid} = rm:start_link(),
	      rm:update_res(
		#resource{id = 16,name = "Ayumi"}),
	      rm:update_res(
		#resource{id = 13,name = "None"}),
	      Pid
      end,
      fun(_Pid) ->
	      rm:stop()
      end,
      ?_test(
	 begin
	     Res16 = #resource{id = 16},
	     Reply16 = rm:find_res(Res16),
	     ?assertEqual(#resource{id = 16,name = "Ayumi"},
			  Reply16),
	     Res13 = #resource{id = 13},
	     rm:remove_res(Res13),
	     Reply13 = rm:find_res(Res13),
	     ?assertEqual(notexist,Reply13),
	     rm:reload_res(),
	     timer:sleep(200),
	     ?assertEqual(#resource{id = 16,name = "BoA"},
			  rm:find_res(Res16)),
	     ?assertEqual(#resource{id = 13},
			  rm:find_res(Res13))
	 end
	)
     }
    }.



      
			     
			      

    
		
		      
		  
