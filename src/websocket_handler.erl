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
    % Message = #{<<"msgid">> => 0,
    %             <<"data">> => "Welcome to cowboy_websocket"},
    % io:format("websocket_init ~p ~n",[jsx:is_json(jsx:encode(Message))]), 
    % self() ! {send_message, self(),Message},
    {ok, Req, undefined_state}.  
  
websocket_handle({text, Data}, Req, State) -> 
    io:format("websocket_handle ~p ~n",[Data]), 
    io:format("websocket_handle ~p ~n",[jsx:is_json(Data)]), 
    poker_room:send_message(self(),Data),
    {ok, Req, State}.

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