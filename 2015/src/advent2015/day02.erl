% -*- mode: erlang -*-

-module(day02).
-export([readlines/1,compute_line/1,compute/3,compute_paper/3,compute_ribbon/3]).

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try compute_all_lines(Device, 0, 0)
    after file:close(Device)
    end.

compute_all_lines(Device, Paper, Ribbon) ->
    case io:get_line(Device, "") of
        eof  -> [Paper,Ribbon];
        Line -> [P,R] = compute_line(Line),
		compute_all_lines(Device, Paper+P, Ribbon+R)
    end.

compute_line(Line) ->
    [X,Y,Z] = lists:map(fun(X) ->
				 {Int, _} = string:to_integer(X), Int end,
			string:tokens(Line, "x")),
    if
	X>=Y, X>=Z -> compute(Y,Z,X) ;
	Y>=X, Y>=Z -> compute(X,Z,Y) ;
	true -> compute(X,Y,Z)
    end.

compute(X,Y,Z) ->
    [compute_paper(X,Y,Z), compute_ribbon(X,Y,Z)].

compute_paper(Small1, Small2, Large) ->
    3*Small1*Small2 + 2*Small1*Large + 2*Small2*Large.

compute_ribbon(Small1, Small2, Large) ->
    2*(Small1+Small2) + Small1*Small2*Large.


	    
