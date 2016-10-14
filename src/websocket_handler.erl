-module(websocket_handler).  
-behaviour(cowboy_http_handler).  
-behaviour(cowboy_websocket_handler).  
-export([init/3, handle/2, terminate/3]).  
-export([  
    websocket_init/3, websocket_handle/3,  
    websocket_info/3, websocket_terminate/3  
]).  
  

init({tcp, http}, _Req, _Opts) ->
    io:format("web init ~n"), 
    {upgrade, protocol, cowboy_websocket}.  
  
handle(_, State) ->  
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),  
    {ok, Req2, State}.  
  
websocket_init(_TransportName, Req, _Opts) -> 
    io:format("websocket_init ~n"), 
    poker_room:register(self()),
    {ok, Req, undefined_state}.  
  
% login(Pid) ->
%     Message = #{<<"msgid">> => 1002,
%                 <<"data">> => "Welcome to cowboy_websocket"},
%     self() ! {send_message, Pid,Message}.

websocket_handle({text, Msg}, Req, _State) -> 
    io:format("websocket_handle ~p ~n",[Msg]), 
    io:format("websocket_handle ~p ~n",[jsx:is_json(Msg)]), 
    #{<<"msgid">> := MsgId,
      <<"data">> := Data} = jsx:decode(Msg, [return_maps]),

    NewState = case MsgId of
        1001 ->
            #{<<"username">> := UserName} = Data,
            io:format("login: ~p ~n",[UserName]), 
            poker_room:login(self(),UserName);
        1003 ->
            io:format("login 1003 ~n"), 
            poker_room:send_message(self(),Data)
    end,
    
    {ok, Req, NewState}.

websocket_info({send_message,_ServerPid, Msg}, Req, State) ->  
    io:format("chat ~n"),  
    {reply, {text, Msg}, Req, State};   

websocket_info({timeout, _Ref, Msg}, Req, State) ->  
    {reply, {text, Msg}, Req, State};  
  
websocket_info(_Info, Req, State) ->  
    lager:debug("websocket info"),  
    {ok, Req, State, hibernate}.  
  
websocket_terminate(_Reason, _Req, _State) ->  
    io:format("websocket_terminate ~n"),
    poker_room:unregister(self()), 
    ok.  
  
terminate(_Reason, _Req, _State) -> 
    io:format("terminate ~n"),
    % chat_room:unregister(self()), 
    ok.  