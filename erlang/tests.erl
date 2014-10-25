-module(tests).
-export([test_sum_fac/0]).

test_sum_fac() ->
    {ok, MR}  = mr:start(3),
    {ok, Sum} = mr:job(MR, 
                       fun(X) -> X end,
                       fun(X,Acc) -> X+Acc end,
                       0,
                       lists:seq(1,10)),
    {ok, Fac} = mr:job(MR, 
                       fun(X) -> X end,
                       fun(X,Acc) -> X*Acc end,
                       1,
                       lists:seq(1,10)),
    mr:stop(MR),
    {Sum, Fac}.
