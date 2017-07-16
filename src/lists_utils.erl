-module(lists_utils).



-export([shuffle/1]).
-export([round_robin/2]).
-export([rotate_right/2]).
-export([rotate_right_with/2]).




%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% From https://erlangcentral.org/wiki/index.php/RandomShuffle
%% @end
%% -----------------------------------------------------------------------------
shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).



%% -----------------------------------------------------------------------------
%% @doc
%% > round_robin(a, [a,b,c]).
%% b
%% > round_robin(b, [a,b,c]).
%% c
%% > round_robin(c, [a,b,c]).
%% a
%% > round_robin(d, [a,b,c]).
%% a
%% @end
%% -----------------------------------------------------------------------------
-spec round_robin(any(), list()) -> any().

round_robin(_, []) ->
    [];
round_robin(X, L) ->
    round_robin(L, X, L).


%% @private
round_robin([X], X, L) ->
    hd(L);

round_robin([X|T], X, _) ->
    hd(T);

round_robin([_|T], X, L) ->
    round_robin(T, X, L);

round_robin([], _, L) ->
    hd(L).    


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec rotate_right(any(), list()) -> list().

rotate_right(_, []) ->
    [];

rotate_right(X, [X|T]) ->
    lists:append(T, [X]);

rotate_right(X, L) ->
    rotate_right(L, X, []).


%% @private
rotate_right([X] = S, X, Acc) ->
    lists:append(lists:reverse(Acc), S);

rotate_right([X|T], X, Acc) ->
    lists:append([T, lists:reverse(Acc), [X]]);

rotate_right([H|T], X, Acc) ->
   rotate_right(T, X, [H|Acc]);

rotate_right([], _, Acc) ->
    lists:reverse(Acc).


%% -----------------------------------------------------------------------------
%% @doc
%% Example:
%% > lists_utils:rotate_right_with(fun(X) -> X == a end, [a,b,c]).
%% [b,c,a]
%% @end
%% -----------------------------------------------------------------------------
-spec rotate_right_with(fun((term()) -> boolean()), list()) -> list().

rotate_right_with(_, []) ->
    [];

rotate_right_with(Pred, L) ->
    rotate_right_with(L, Pred, []).


%% @private
rotate_right_with([H|T], Pred, Acc) ->
    case Pred(H) of
        true ->
             lists:append([T, lists:reverse(Acc), [H]]);
        false ->
            rotate_right_with(T, Pred, [H|Acc])
    end;

rotate_right_with([], _, Acc) ->
    lists:reverse(Acc).





%% =============================================================================
%% PRIVATE
%% =============================================================================


%% @private
randomize(1, List) ->
    randomize(List);

randomize(T, List) ->
    lists:foldl(
        fun(_E, Acc) -> randomize(Acc) end, 
        randomize(List), 
        lists:seq(1, (T - 1))).


%% @private
randomize(List) ->
    D = lists:map(fun(A) -> {rand:uniform(), A} end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.