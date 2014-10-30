-module(sheet).

%% API
-export([ sheet/0
        , cell/2
        , add_viewer/2
        , remove_viewer/2
        , get_viewers/1
        , set_value/2]).

%%%===================================================================
%%% API
%%%===================================================================



%%%===================================================================
%%% Internal functions
%%%===================================================================


%%% Communication primitives

async(Pid, Msg) ->
    Pid ! Msg.

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.


%%% Server loops
