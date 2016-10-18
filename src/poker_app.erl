%%%-------------------------------------------------------------------
%% @doc poker public API
%% @end
%%%-------------------------------------------------------------------

-module(poker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%%====================================================================
%% API
%%====================================================================


start(_StartType, _StartArgs) ->
    io:format("start ~n"), 
 	% Dispatch = cowboy_router:compile([
  %       {'_', [
  %       	% {"/", poker_handler, []}
  %       	{"/",cowboy_static,{priv_file,poker,"index.html"}},
  %       	{"/websocket",websocket_handler,[]},
  %           {"/static/[...]",cowboy_static,{priv_dir,poker,"static"}}
  %       ]}
  %   ]),
  %   cowboy:start_http(my_http_listener, 100, [{port, 8080}],
  %       [{env, [{dispatch, Dispatch}]}]
  %   ),
    chat_user_list:init(),
    poker_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
