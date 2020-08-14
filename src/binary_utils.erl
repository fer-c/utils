-module(binary_utils).


-export([join/1]).
-export([join/2]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc Returns a binary with the elements of `BinList' separated by the string
%% the default separator `$\31'.
%% @end
%% -----------------------------------------------------------------------------
-spec join(BinList :: [binary()]) -> binary().

join(BinList) ->
    join(BinList, <<$\31>>).


%% -----------------------------------------------------------------------------
%% @doc Returns a binary with the elements of `BinList' separated by the string
%% separator `Separator'.
%% @end
%% -----------------------------------------------------------------------------
-spec join(BinList :: [binary()], Seprarto :: binary()) -> binary().

join([], _) ->
  <<>>;

join([Part], _) ->
  Part;

join([H|T], Sep) ->
    lists:foldl(
        fun(Value, Acc) -> <<Acc/binary, Sep/binary, Value/binary>> end,
        H, T
    ).