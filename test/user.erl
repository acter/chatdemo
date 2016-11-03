-module(user).
-export([add/3, list/0]).

-define(ETS_ONLINE, ?MODULE).
-record(ets_online, {
        nickname = none,
        pid = 0
    }).

init() ->
    ets:new(?ETS_ONLINE, [public, named_table]).

add(Pid,Username) ->
    ets:insert(?ETS_ONLINE, #ets_online{ nickname = Username,pid =Pid}).

list() ->
	L = ets:tab2list(?ETS_ONLINE).





%%
rd(users, {pid,name}).
Users=#users{pid=101,name=lajabs}.
%%创建
ets:new(users,[public,set,named_table,{keypos, #users.uid}]).
ets:insert(users,Users).
ets:tab2list(users).
%%更新
ets:update_element(users,102,{#users.name, hello}).
%%查找
ets:lookup(users,102).
%%删除
ets:delete(users,102).