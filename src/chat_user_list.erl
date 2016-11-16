-module(chat_user_list).

-export([init/0, add/3, remove/1, rename/2, list/0,getMembers/1,getPids/1]).

-record(ets_online, {pid,username,rid}). % dummy record for now

-define(ETS_ONLINE, ?MODULE).

init() ->
    ets:new(?ETS_ONLINE, [{keypos,#ets_online.pid}, named_table, public, set]).

add(Pid,Username,Rid) ->
    ets:insert(?ETS_ONLINE, #ets_online{pid=Pid,username=Username,rid=Rid}).

remove(Username) ->
    ets:delete(?ETS_ONLINE, Username).

rename(Old, New) ->
    case ets:lookup(?ETS_ONLINE, Old) of
        [{Old, UserState}] ->
            ets:delete(?ETS_ONLINE, Old),
            ets:insert(?ETS_ONLINE, {New, UserState});
        [] ->
            ok
    end.

list() ->
    [Username || {Username, _} <- ets:tab2list(?ETS_ONLINE)].

getMembers(Rid) ->
    L = ets:match(?ETS_ONLINE,#ets_online{pid='$1',username='$2',rid=Rid,_='_'}),
    io:format("getMembers ~p ~n",[L]), 
    % Users = [ {Pid,Name} || [Pid,Name] <- L],
    % Users = [ {Name} || [_,Name] <- L],
    Users = [ Name || [_,Name] <- L],
    {ok,Users}.

getPids(Rid) ->
    L = ets:match(?ETS_ONLINE,#ets_online{pid='$1',username='$2',rid=Rid,_='_'}),
    io:format("getMembers ~p ~n",[L]), 
    % Users = [ {Pid,Name} || [Pid,Name] <- L],
    % Users = [ {Name} || [_,Name] <- L],
    Users = [ Pid || [Pid,_] <- L],
    {ok,Users}.