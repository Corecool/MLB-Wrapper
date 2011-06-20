-module(rm_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").

-define(REMOTE,'node1@corecool-laptop').

remote_rm_test_() ->
    {spawn,
     {setup,
      fun() ->
	      rpc:call(?REMOTE,rm,start_link,[]),
	      rpc:cast(?REMOTE,rm,update_res,
		       [#resource{id = 16,name = "Ayumi"}])
      end,
      fun(_) ->
	      rpc:cast(?REMOTE,rm,stop,[])
      end,
      ?_test(
	 begin
	     Res16 = #resource{id = 16},
	     Reply16 = rpc:call(?REMOTE,rm,find_res,[Res16]),
	     ?assertEqual(#resource{id = 16,name = "Ayumi"},
			  Reply16),
	     rpc:cast(?REMOTE,rm,reload_res,[]),
	     timer:sleep(200),
	     ?assertEqual(#resource{id = 16,name = "BoA"},
			  rpc:call(
			    ?REMOTE,rm,find_res,[Res16]))
	 end
	)
     }
    }.

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

rm_monitor_test_() ->
    {spawn,
     {setup,
      fun() ->
	      monitor:start_link(),
	      rm:start_link()
      end,
      fun(_) ->
	      rm:stop(),
	      monitor:stop()
      end,
      ?_test(
	 begin
	     Res16 = #resource{id = 16},
	     Reply16 = rm:find_res(Res16),
	     ?assertEqual(#resource{id = 16},
			  Reply16),
	     Res1001 = #resource{id = 1001},
	     Reply1001 = rm:find_res(Res1001),
	     ?assertEqual(notexist,Reply1001),
	     ?assertEqual(1,monitor:get_res_miss())
	 end
	)
     }
    }.

      
			     
			      

    
		
		      
		  
