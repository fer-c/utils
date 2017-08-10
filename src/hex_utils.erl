-module(hex_utils).
-export([bin_to_hexstr/1,hexstr_to_bin/1]).

%% From Steve Vinoski (http://necrobious.blogspot.co.uk/2008/03/binary-to-hex-string-back-to-binary-in.html)

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).


%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%% -----------------------------------------------------------------------------
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));

hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).