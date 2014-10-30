%%%-------------------------------------------------------------------
%%% @author Ken Friis Larsen <kflarsen@diku.dk>
%%% @copyright (C) 2011, Ken Friis Larsen
%%% Created  : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%% Modified : Oct 2014 by Kasper Helweg Jonassen
%%%-------------------------------------------------------------------
-module(mr).
-export([start/1, stop/1, job/5]).

%%%% Interface
start(N) ->
    {Reducer, Mappers} = init(N),
    {ok, CPId} = register_coordinator(coord, Reducer, Mappers),
    {ok, CPId}.

stop(Pid) -> 
    rpc(Pid, stop).

job(CPid, MapFun, RedFun, RedInit, Data) -> 
    rpc(CPid, {dojob, MapFun, RedFun, RedInit, Data}).

%%%% Internal implementation
register_coordinator(A, Reducer, Mappers) ->
    CPId = spawn(fun() -> coordinator_loop(Reducer, Mappers) end),
    register(A, CPId),
    {ok, CPId}.

init(N) -> 
    Reducer =  spawn( fun()-> reducer_loop(none) end),
    Mappers = [spawn( fun()-> mapper_loop(Reducer, none) end) || _N <- lists:seq(1, N)],
    {Reducer, Mappers}.

%% synchronous communication
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).

%% asynchronous communication
async(Pid, Msg) ->
    Pid ! Msg.

stop_async(Pid) ->
    async(Pid, stop).

data_async(Pid, D) ->
    async(Pid, {data, D}).

%%% Coordinator
coordinator_loop(Reducer, Mappers) ->
    receive
	{From, stop} ->
	    io:format("~p stopping~n", [self()]),
	    lists:foreach(fun stop_async/1, Mappers),
	    stop_async(Reducer),
	    reply_ok(From);
	{From, {dojob, MapFun, RedFun, RedInit, Data}} ->
	    %% Dodgy stuff
	    lists:foreach(fun(Pid) -> rpc(Pid, {init, MapFun})     
			  end, Mappers),
	    rpc(Reducer, {init, RedFun, RedInit, 3}),
	    send_data(Mappers, Data),
	    coordinator_loop(Reducer, Mappers);
	{From, {mapresult, Result}} ->
	    aasd;
	{From, {reduceresult, Result}} -> 
	    {ok, Result}
    end.

send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).

%%% Reducer
reducer_loop(Fun) ->
    receive
	stop -> 
	    io:format("Reducer ~p stopping~n", [self()]),
	    ok;
	{From, {init, RedFun, Acc, NumMappers}} ->
	    From ! {self(), ok},
	    {EVal, Result} = gather_data_from_mappers(RedFun, Acc, NumMappers),
	    case EValof
		ok -> coord ! {self(), {reduceresult, Result}};
		reducer_loop(Fun);
	Unknown ->
	    io:format("unknown message: ~p~n",[Unknown]), 
	    reducer_loop(Fun)
    end.

gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
	stop -> 
	    io:format("Reducer ~p stopping~n", [self()]),
	    ok;
	{From, {result, Result}} -> 
	    io:write(Acc),
	    Parres = Fun(Result, Acc),
	    case Missing of
		-10 -> {ok, Acc};
		_ -> gather_data_from_mappers(Fun, Parres, Missing - 1)
	    end
    end.

%%% Mapper
mapper_loop(Reducer, Fun) ->
    receive
	stop -> 
	    io:format("Mapper ~p stopping~n", [self()]),
	    ok;
	{From, {init, MapFun}} ->
	    From ! {self(), ok},
	    mapper_loop(Reducer, MapFun);
	{data, Data} ->
	    Result = Fun(Data),
	    Reducer ! {self(), {result, Result}},
	    mapper_loop(Reducer, Fun);
	Unknown ->
	    io:format("unknown message: ~p~n",[Unknown]), 
	    mapper_loop(Reducer, Fun)
    end.
