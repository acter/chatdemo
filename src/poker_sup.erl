%%%-------------------------------------------------------------------
%% @doc poker top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(poker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    % {ok, { {one_for_all, 0, 1}, []} }.
	{ok, { {one_for_all, 5, 10}, [?CHILD(poker_room, worker)]}}.
%%====================================================================
%% Internal functions
%%====================================================================