-module(lirs_tests).
-include_lib("eunit/include/eunit.hrl").

-record(cache, {resList = [],hirList = [],lirList = []}).
-record(resource,{id,status = none,location = []}).

create_res() ->
    Seq = lists:seq(0,9),
    lists:foldl(fun(X,List) ->
			[#resource{id = X} | List]
		end,
		[],Seq).

init_test_() ->
    ResList = create_res(),
    State = #cache{resList = ResList,
			     hirList = [],
			     lirList = []},
    ?_assertMatch({ok,State},lirs:init(10)).

first_element_test_() ->
    {spawn,
     {setup,
      fun() ->
	      lirs:start_link(10)
      end,
      fun(_) ->
	      gen_server:cast(lirs,stop)
      end,
      ?_test(
	 begin
	     {Res,NewState} = gen_server:call(lirs,
					      #resource{id = 6}),
	     ExpectState = #cache{resList = create_res(),
				  hirList = [],
				  lirList = [Res]},
	     ?assertEqual(#resource{id = 6,status = lir,
				    location = [self()]},Res),
	     ?assertEqual(ExpectState,NewState)
	 end
	)}}.

    
    
    
   
