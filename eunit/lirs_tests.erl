-module(lirs_tests).
-include_lib("eunit/include/eunit.hrl").

-record(cacheItem,{id,status}).

lookup(Key) ->
    ets:lookup(lirsRam,Key).

%% lookup(Key,Pos) ->
%%     ets:lookup_element(lirsRam,Key,Pos).


init_test_() ->
    {spawn,
     {setup,
      fun() ->
	      lirs:start_link("/tmp/init") end,
      fun(_) ->
	      gen_server:cast(lirs,stop),
	      ?cmd("rm -f /tmp/init")
      end,
      [?_assertMatch([{lirQueue,[]}],lookup(lirQueue)),
       ?_assertMatch([{hirQueue,[]}],lookup(hirQueue)),
       ?_assertEqual([{conf,{size,5},{lirPercent,0.6}}],
		     lookup(conf))]
     }
    }.

first_lirs_test_() ->
    {spawn,
     {setup,
      fun() ->
	      lirs:start_link("/tmp/first"),
	      gen_server:cast(lirs,{visit,5}),
	      timer:sleep(100)
      end,
      fun(_) ->
	      gen_server:cast(lirs,stop),
	      ?cmd("rm -f /tmp/first")		  
      end,
      [?_assertEqual([{lirQueue,
		       [#cacheItem{id = 5, status = lir}]}],
		     lookup(lirQueue)),
       ?_assertEqual([{hirQueue,[]}],lookup(hirQueue)),
       ?_assertMatch([{5,lir}],lookup(5))]
      }
     }.

second_lirs_test_() ->
    {spawn,
     {setup,
      fun() ->
	      lirs:start_link("/tmp/second"),
	      gen_server:cast(lirs,{visit,1}),
	      gen_server:cast(lirs,{visit,5}),
	      gen_server:cast(lirs,{visit,1}),
	      timer:sleep(100)
      end,
      fun(_) ->
	      gen_server:cast(lirs,stop),
	      ?cmd("rm -f /tmp/second")		  
      end,
      [?_test(
	  begin
	      Res1 = #cacheItem{id = 1,status = lir},
	      Res5 = #cacheItem{id = 5,status = lir},
	      LirQueue = [Res1 | [Res5]], 
	      ?assertEqual([{lirQueue,LirQueue}],
			   lookup(lirQueue))
	  end),
       ?_assertEqual([{hirQueue,[]}],lookup(hirQueue)),
       ?_assertMatch([{5,lir}],lookup(5)),
       ?_assertMatch([{1,lir}],lookup(1))]
     }
    }.

third_lirs_test_() ->		    
    {spawn,
     {setup,
      fun() ->
     	      lirs:start_link("/tmp/third"),
     	      gen_server:cast(lirs,{visit,1}),
     	      gen_server:cast(lirs,{visit,2}),
     	      gen_server:cast(lirs,{visit,3}),
	      gen_server:cast(lirs,{visit,4}),
     	      timer:sleep(100)
      end,
      fun(_) ->
     	      gen_server:cast(lirs,stop),
     	      ?cmd("rm -f /tmp/third")		  
      end,
      [?_test(
	  begin
	      Res1 = #cacheItem{id = 1,status = lir},
	      Res2 = #cacheItem{id = 2,status = lir},
	      Res3 = #cacheItem{id = 3,status = lir},
	      Res4 = #cacheItem{id = 4,status = hir},
	      LirQueue = [Res4,Res3,Res2 | [Res1]], 
	      ?assertEqual([{lirQueue,LirQueue}],
			   lookup(lirQueue))
	  end),
       ?_test(
	  begin
	      Res4 = #cacheItem{id = 4,status = hir},
	      ?assertEqual([{hirQueue,[Res4]}],
			   lookup(hirQueue))
	  end),
       ?_test(
	  begin
	      ?assertMatch([{1,lir}],lookup(1)),
	      ?assertMatch([{2,lir}],lookup(2)),
	      ?assertMatch([{3,lir}],lookup(3)),
	      ?assertMatch([{4,hir}],lookup(4))
	  end)]
     }
    }.


