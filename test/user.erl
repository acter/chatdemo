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
rd(users, {pid,name,rid}).
Users=#users{pid=101,name=lajabs,rid=11}.
Users2=#users{pid=102,name=lajabs11,rid=11}.
ets:insert(users,Users2).
%%创建
ets:new(users,[public,set,named_table,{keypos, #users.pid}]).
ets:insert(users,Users).
ets:tab2list(users).
%%更新
ets:update_element(users,102,{#users.name, hello}).
%%查找
ets:lookup(users,102).
%%删除
ets:delete(users,102).

%%匹配
L = ets:match(users,#users{pid='$1',name='$2',rid=11}).

L22 = [ {Pid,Name} || [Pid,Name] <- L].


ets:match(users,#users{pid='$1',name=lajabs11}).

ets:match(users,#users{name=lajabs11,pid='$1'}).