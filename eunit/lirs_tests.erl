-module(lirs_tests).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ?assert(1 + 1 =:= 2).

falied_test() ->
    ?assertEqual(3,1+1).

testset_test_() ->
    ?_assertEqual(5,3+2),
    ?_assertError(badarith, 10/0),
    ?_assertEqual(100,5).

