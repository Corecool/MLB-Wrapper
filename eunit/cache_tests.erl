-module(cache_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/resource.hrl").
-include("../include/cacheItem.hrl").

lookup(Key) ->
    ets:lookup(lirsRam,Key).

wait() ->
    timer:sleep(500).

simple_resource_test_() ->
    {spawn,
     {setup,
      fun() ->
	      remote_rm:start_link(),
	      lirs:start_link(),
	      cache:start_link()
      end,
      fun(_) ->
	      cache:stop(),
	      lirs:stop(),
	      remote_rm:stop(),
	      wait()
      end,
      [?_test(
	  begin
	      Res = cache:visit_res(#resource{id = 999}),
	      ?assertEqual(#resource{id = 999,name = "BoA"},
			   Res)
	  end
	 ),
       ?_test(
	  begin
	      Res = cache:visit_res(#resource{id = 1001}),
	      ?assertEqual(notexist,Res)
	  end
	 )]
     }
    }.

resource_test_() ->
    {spawn,
     {setup,
      fun() ->
     	      remote_rm:start_link(),
	      lirs:start_link(),
	      cache:start_link(),
	      Res2 = #resource{id = 2},
	      Res3 = #resource{id = 3},
	      Res5 = #resource{id = 5},
	      Res6 = #resource{id = 6},
	      Res7 = #resource{id = 7},
	      Res8 = #resource{id = 8},
	      cache:visit_res(Res5),
	      cache:visit_res(Res7),
	      cache:visit_res(Res8),
	      cache:visit_res(Res5),
	      cache:visit_res(Res3),
	      cache:visit_res(Res8),
	      cache:visit_res(Res6),
	      cache:visit_res(Res6),
	      cache:visit_res(Res2),
	      cache:visit_res(Res7)
      end,
      fun(_) ->
     	      cache:stop(),
	      lirs:stop(),
	      remote_rm:stop(),
	      wait()
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
	  end),
       ?_test(
	  begin
	      ?assertMatch(true,ets:member(cacheTab,2)),
	      ?assertMatch(false,ets:member(cacheTab,3)),
	      ?assertMatch(true,ets:member(cacheTab,5)),
	      ?assertMatch(true,ets:member(cacheTab,6)),
	      ?assertMatch(true,ets:member(cacheTab,7)),
	      ?assertMatch(true,ets:member(cacheTab,8))
	  end)]
     }
    }.
