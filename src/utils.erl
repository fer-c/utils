-module(utils).

%% API exports
-export([bit_reverse/1]).






%% =============================================================================
%% API
%% =============================================================================

-spec bit_reverse(integer()) -> integer().

bit_reverse(N) ->
    L = lists:reverse([X || <<X:1/big-unsigned-integer>> <= <<N/integer>>]),
    <<Res:8/integer>> = << <<X:1/big-unsigned-integer>> || X <- L>>,
    Res.
