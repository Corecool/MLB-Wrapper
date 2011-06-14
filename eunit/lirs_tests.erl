-module(lirs_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/cacheItem.hrl").


lookup(Key) ->
    ets:lookup(lirsRam,Key).

init_test_() ->
    {spawn,
     {setup,
      fun() ->
	      lirs:start_link() end,
      fun(_) ->
	      lirs:stop()
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
	      lirs:start_link(),
	      lirs:visit_res(5),
	      timer:sleep(100)
      end,
      fun(_) ->
	      lirs:stop()
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
	      lirs:start_link(),
	      lirs:visit_res(1),
	      lirs:visit_res(5),
	      lirs:visit_res(1),
	      timer:sleep(100)
      end,
      fun(_) ->
	      lirs:stop()
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
     	      lirs:start_link(),
     	      lirs:visit_res(1),
	      lirs:visit_res(2),
	      lirs:visit_res(3),
	      lirs:visit_res(4),
     	      timer:sleep(100)
      end,
      fun(_) ->
     	      lirs:stop()
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


fourth_lirs_test_() ->
    {spawn,
     {setup,
      fun() ->
     	      lirs:start_link(),
     	      lirs:visit_res(5),
	      lirs:visit_res(7),
	      lirs:visit_res(8),
	      lirs:visit_res(5),
	      lirs:visit_res(3),
	      lirs:visit_res(8),
	      lirs:visit_res(6),
	      lirs:visit_res(6),
	      lirs:visit_res(2),
	      lirs:visit_res(7),
     	      timer:sleep(500)
      end,
      fun(_) ->
     	      lirs:stop()
      end,
      [?_test(
	  begin
	      Res2 = #cacheItem{id = 2,status = hir},
	      Res3 = #cacheItem{id = 3,
				status = non_resident},
	      Res5 = #cacheItem{id = 5,status = lir},
	      Res6 = #cacheItem{id = 6,status = lir},
	      Res7 = #cacheItem{id = 7,status = hir},
	      Res8 = #cacheItem{id = 8,status = lir},
	      LirQueue = [Res7,Res2,Res6,
			  Res8,Res3,Res5], 
	      ?assertEqual([{lirQueue,LirQueue}],
			   lookup(lirQueue))
	  end),
       ?_test(
	  begin
	      Res2 = #cacheItem{id = 2,status = hir},
	      Res7 = #cacheItem{id = 7,status = hir},
	      ?assertEqual([{hirQueue,[Res7,Res2]}],
			   lookup(hirQueue))
	  end),
       ?_test(
	  begin
	      ?assertMatch([{5,lir}],lookup(5)),
	      ?assertMatch([{2,hir}],lookup(2)),
	      ?assertMatch([{3,non_resident}],lookup(3)),
	      ?assertMatch([{6,lir}],lookup(6)),
	      ?assertMatch([{7,hir}],lookup(7)),
	      ?assertMatch([{8,lir}],lookup(8))
	  end)]
     }
    }.

%% 这组测试只是验证有初始值时LIRS能否正常工作
fivth_lirs_test_() ->
    {spawn,
     {setup,
      fun() ->
     	      lirs:start_link(test),
     	      lirs:visit_res(4),
	      lirs:visit_res(8),
	      lirs:visit_res(3),
	      lirs:visit_res(5),
	      lirs:visit_res(7),
	      lirs:visit_res(9),
	      lirs:visit_res(5),
     	      timer:sleep(100)
      end,
      fun(_) ->
     	      lirs:stop()
      end,
      [?_test(
	  begin
	      Res3 = #cacheItem{id = 3,status = lir},
	      Res5 = #cacheItem{id = 5,status = lir},
	      Res7 = #cacheItem{id = 7,
				status = non_resident},
	      Res8 = #cacheItem{id = 8,status = lir},
	      Res9 = #cacheItem{id = 9,status = hir},
	      LirQueue = [Res5,Res9,Res7,
			  Res3,Res8], 
	      ?assertEqual([{lirQueue,LirQueue}],
			   lookup(lirQueue))
	  end),
       ?_test(
	  begin
	      Res4 = #cacheItem{id = 4,status = hir},
	      Res9 = #cacheItem{id = 9,status = hir},
	      ?assertEqual([{hirQueue,[Res4,Res9]}],
			   lookup(hirQueue))
	  end),
       ?_test(
	  begin
	      ?assertMatch([{5,lir}],lookup(5)),
	      ?assertMatch([{2,non_resident}],lookup(2)),
	      ?assertMatch([{3,lir}],lookup(3)),
	      ?assertMatch([{6,non_resident}],lookup(6)),
	      ?assertMatch([{7,non_resident}],lookup(7)),
	      ?assertMatch([{8,lir}],lookup(8)),
	      ?assertMatch([{1,non_resident}],lookup(1)),
	      ?assertMatch([{4,hir}],lookup(4)),
	      ?assertMatch([{9,hir}],lookup(9))
	  end)]
     }
    }.
