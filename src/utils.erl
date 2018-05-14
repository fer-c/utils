-module(utils).

%% API exports
-export([binary_rotate/1]).






%% =============================================================================
%% API
%% =============================================================================

-spec binary_rotate(integer()) -> integer().

binary_rotate(N) ->
    binary_rotate(N, 0).




%% =============================================================================
%% PRIVATE
%% =============================================================================




%% @private
binary_rotate(N, Acc) when N > 0 ->
    binary_rotate((N bsr 1), (((Acc bsl 1) bor (N band 1))));

binary_rotate(_, Acc) ->
    Acc.

