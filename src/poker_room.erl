-module(poker_room).
-behaviour(gen_server).

-export([start_link/0, register/1, unregister/1, send_message/2]).

-export([getOnline/1, login/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {clients=[]}).

% -record(user, {users=[]}).


%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(Pid) ->
    gen_server:cast(?SERVER, {register, Pid}).

unregister(Pid) ->
    gen_server:cast(?SERVER, {unregister, Pid}).

send_message(Pid, Message) ->
    gen_server:cast(?SERVER, {send_message, Pid, Message}).

login(Pid,UserName) ->
    gen_server:cast(?SERVER, {login, Pid,UserName}).

getOnline(Pid) ->
    gen_server:cast(?SERVER, {online, Pid}).
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
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({register, Pid}, State = #state{clients = Clients}) ->
    {noreply, State#state{clients = [Pid|Clients]}};
handle_cast({unregister, Pid}, State = #state{clients = Clients}) ->
    {noreply, State#state{clients  = Clients -- [Pid]}};

handle_cast({login, Pid,UserName}, State) ->
    io:format("login init ~p ~n",[UserName]), 
    do_login(Pid,UserName),
    % put(Pid,{ok,UserName}),
    chat_user_list:add(UserName),
    {noreply, State};

% handle_cast({online, Pid}, State = #state{clients = Clients}) ->
%     io:format("online init ~n"), 
%     OtherPids = Clients -- [Pid],
%     {noreply, State};

% handle_cast({online, Pid}, State = #state{clients = Clients}) ->
handle_cast({online, Pid}, State) ->
    io:format("online init ~n"), 
    % OtherPids = Clients -- [Pid],
    % Players = [],
    % Users =  lists:foldl(
    %   fun(OtherPid,[]) ->
    %         {ok,Name} = get(OtherPid),
    %         io:format("Name init ~p ~n",[Name]),
    %         Players++[Name]
    %   end,Players, OtherPids),
    % io:format("online init ~p ~n",Users), 
    % Users = ["a","b","c"],
    Msg = #{<<"msgid">> => 1006,
                <<"data">> => chat_user_list:list()},

    Pid ! {send_message, self(), jsx:encode(Msg)},
    {noreply, State};
handle_cast({send_message, Pid, Message}, State) ->
    do_send_message(Pid, Message, State),
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

do_send_message(Pid, Message, #state{clients = Clients}) ->
    OtherPids = Clients -- [Pid],
    lists:foreach(
      fun(OtherPid) ->
              OtherPid ! {send_message, self(), Message}
      end, OtherPids).

do_login(Pid,UserName) ->
    io:format("do_login init ~p ~n",[UserName]), 
    Meg = #{<<"msgid">> => 1002,
                <<"data">> => <<"Welcome to cowboy_websocket">>},
    Pid ! {send_message, self(), jsx:encode(Meg)}.

% do_get_online(Pid,OtherPids,#user{users = Users}) ->
%     io:format("do_get_online init  ~n"), 
%     Users1 =  lists:foldl(
%       fun(OtherPid,[]) ->
%             {ok,Name} = get(OtherPid),
%             io:format("Name init ~p ~n",[Name]),
%             Users++[Name]
%       end,Users, OtherPids),
%     Msg = #{<<"msgid">> => 1006,
%                 <<"data">> => Users1},
%     Pid ! {send_message, self(), jsx:encode(Msg)},