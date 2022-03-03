-module(binary_utils).


-export([join/1]).
-export([join/2]).
-export([is_string/1]).


%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_string(Bin :: binary()) -> boolean() | no_return().

is_string(<<_/utf8, Rest/binary>>) ->
    is_string_b(Rest);

is_string(Term) when is_binary(Term) ->
    false;

is_string(_) ->
    error(badarg).


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





%% =============================================================================
%% PRIVATE
%% =============================================================================


%% @private
is_string_b(<<_/utf8, Rest/binary>>) ->
    is_string_b(Rest);

is_string_b(<<_, _/binary>>) ->
    false;

is_string_b(<<>>) ->
    true.



