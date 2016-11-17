-module(test).
-export([main/1]).

main([UserName]) ->
	% I = list_to_integer(atom_to_list(UserName)),
    io:format("login: ~p ~n",[UserName]),
     io:format("login222: ~p ~n",[<<"*">>]),
    if
        UserName == '*' ->
            io:format("bbb= ~n");
        true ->
            io:format("aaa= ~n")
    end,
    init:stop().
