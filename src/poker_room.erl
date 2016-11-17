-module(poker_room).
-behaviour(gen_server).

-export([start_link/0, send_all_msg/2,send_priv_msg/3]).

-export([ login/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).



%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_all_msg(Rid,Message) ->
    gen_server:cast(?SERVER, {send_all_msg, Rid, Message}).

send_priv_msg(UserName, Rid,Message) ->
    gen_server:cast(?SERVER, {send_priv_msg, UserName,Rid, Message}).

login(Pid,UserName,Rid) ->
    gen_server:cast(?SERVER, {login, Pid,UserName,Rid}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
          % {"/", poker_handler, []}
          {"/",cowboy_static,{priv_file,poker,"index.html"}},
          {"/websocket",websocket_handler,[]},
          {"/static/[...]",cowboy_static,{priv_dir,poker,"static"}}
        ]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    {ok,[]}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({login, Pid,UserName,Rid}, State) ->
    io:format("login init ~p ~n",[UserName]), 
    do_login(Pid,UserName,Rid),
    %%去通知本频道在线人员
    notice_all(UserName,Rid),
    %%添加到ets表
    chat_user_list:add(Pid,UserName,Rid),
    {noreply, State};

handle_cast({online, Pid}, State) ->
    io:format("online init ~n"), 
    Msg = #{<<"msgid">> => 1006,
                <<"data">> => chat_user_list:list()},

    Pid ! {send_message, self(), jsx:encode(Msg)},
    {noreply, State};

handle_cast({send_all_msg, Rid, Message}, State) ->
    do_send_all_msg(Rid,Message),
    {noreply, State};

handle_cast({send_priv_msg, UserName,Rid, Message}, State) ->
    do_send_priv_msg(UserName, Rid,Message),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(chat).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

do_send_all_msg(Rid, Message) ->
    {ok,OtherPids}  = chat_user_list:getPids(Rid),
    lists:foreach(
      fun(OtherPid) ->
              OtherPid ! {send_message, self(), Message}
      end, OtherPids).

do_send_priv_msg(UserName,Rid, Message) ->
    OtherPid = chat_user_list:getPid(UserName,Rid),
    io:format("do_send_priv_msg init ~p ~p ~n",[OtherPid,Message]),

    [Opid] = OtherPid,
    
    Opid ! {send_message, self(), Message}.
    % case OtherPid == ok of
    %     false ->
    %         OtherPid ! {send_message, self(), Message}
    % end.

do_login(Pid,UserName,Rid) ->
    io:format("do_login init ~p ~n",[UserName]), 
    {ok,Users} = chat_user_list:getMembers(Rid),
    io:format("Members ~p ~n",[Users]), 
    Meg = #{<<"msgid">> => 1002,
                <<"data">> => Users},
    Pid ! {send_message, self(), jsx:encode(Meg)}.

notice_all(UserName,Rid) ->
    {ok,OtherPids}  = chat_user_list:getPids(Rid),
    io:format("OtherPids ~p ~n",[OtherPids]), 
    Meg = #{<<"msgid">> => 1006,
                <<"data">> => UserName},
    lists:foreach(
      fun(OtherPid) ->
              io:format("OtherPid ~p ~n",[OtherPid]), 
              OtherPid ! {send_message, self(), jsx:encode(Meg)}
      end, OtherPids).