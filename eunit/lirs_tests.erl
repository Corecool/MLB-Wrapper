-module(lirs_tests).
-include_lib("eunit/include/eunit.hrl").

-record(cacheItem,{id,status}).

init_test_() ->
    {spawn,
     {setup,
      fun() ->
	      lirs:start_link("/tmp/init") end,
      fun(_) ->
	      gen_server:cast(lirs,stop),
	      ?cmd("rm -f /tmp/init")
      end,
      [?_assertMatch([{lirQueue,[]}],
		     ets:lookup(lirsRam,lirQueue)),
       ?_assertMatch([{hirQueue,[]}],
		     ets:lookup(lirsRam,hirQueue)),
       ?_assertEqual([{conf,{size,5},{lirPercent,0.6}}],
		     ets:lookup(lirsRam,conf))]
     }
    }.

first_element_test_() ->
    {spawn,
     {setup,
      fun() ->
	      lirs:start_link("/tmp/first"),
	      gen_server:cast(lirs,{visit,5})
      end,
      fun(_) ->
	      gen_server:cast(lirs,stop),
	      ?cmd("rm -f /tmp/first")		  
      end,
      [?_assertMatch([{lirQueue,
		       [#cacheItem{id = 5, status = lir}]}],
		     ets:lookup(lirsRam,lirQueue)),
       ?_assertMatch([{hirQueue,[]}],
		     ets:lookup(lirsRam,hirQueue)),
       ?_assertMatch([{5,lir}],ets:lookup(lirsRam,5))]
      }
     }.

		    
      
