-module(base32).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([encode/1]).
-export([encode/2]).
-export([decode/1]).



%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
encode(Term) ->
    encode(Term, #{}).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
encode(Bin, Opts) when is_binary(Bin), is_map(Opts) ->
    % Len = (byte_size(Bin) div 5) * 5,
    % <<Whole:Len/binary, Rest/binary>> = Bin,
    Padding = maps:get(padding, Opts, true),
    Case = case maps:get(lowercase, Opts, false) of
        true -> lowercase;
        false -> uppercase
    end,
    encode(Bin, Case, Padding, <<>>);

encode(Str, Opts) when is_list(Str) ->
    encode(list_to_binary(Str), Opts).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
decode(Str) when is_list(Str) ->
    decode(list_to_binary(Str));

decode(Bin) when is_binary(Bin) ->
    decode(Bin, <<>>).




%% =============================================================================
%% PRIVATE
%% =============================================================================



%% @private
encode(<<N:5, Rest/bitstring>>, Case, Padding, Acc0) ->
    Acc = <<Acc0/binary, (encode_int(N, Case))>>,
    encode(Rest, Case, Padding, Acc);

encode(<<N:4>>, Case, Padding, Acc0) ->
    Acc = <<Acc0/binary, (encode_int(N bsl 1, Case))>>,
    maybe_padding(Acc, Padding, <<"===">>);

encode(<<N:3>>, Case, Padding, Acc0) ->
    Acc = <<Acc0/binary, (encode_int(N bsl 2, Case))>>,
    maybe_padding(Acc, Padding, <<"======">>);

encode(<<N:2>>, Case, Padding, Acc0) ->
    Acc = <<Acc0/binary, (encode_int(N bsl 3, Case))>>,
    maybe_padding(Acc, Padding, <<"=">>);

encode(<<N:1>>, Case, Padding, Acc0) ->
    Acc = <<Acc0/binary, (encode_int(N bsl 4, Case))>>,
    maybe_padding(Acc, Padding, <<"====">>);

encode(<<>>, _, _, Acc) ->
    Acc.


%% @private
encode_int(N, lowercase)
when is_integer(N) andalso N >= 0 andalso N =< 25 ->
    N + $a;

encode_int(N, uppercase)
when is_integer(N) andalso N >= 0 andalso N =< 25 ->
    N + $A;

encode_int(N, _)
when is_integer(N) andalso N >= 26 andalso N =< 31 ->
    N + 24.

%% @private
maybe_padding(Acc, true, Str) ->
    <<Acc/binary, Str/binary>>;

maybe_padding(Acc, false, _) ->
    Acc.


%% @private
decode(<<C, "======">>, Acc) ->
    <<Acc/bitstring, ((decode_char(C)) bsr 2):3>>;

decode(<<C, "====">>, Acc) ->
    <<Acc/bitstring, ((decode_char(C)) bsr 4):1>>;

decode(<<C, "===">>, Acc) ->
    <<Acc/bitstring, ((decode_char(C)) bsr 1):4>>;

decode(<<C, "=">>, Acc) ->
    <<Acc/bitstring, ((decode_char(C)) bsr 3):2>>;

decode(<<C, Rest/binary>>, Acc) ->
    decode(Rest, <<Acc/bitstring, (decode_char(C)):5>>);

decode(<<>>, Acc) ->
    Acc.


%% @private
decode_char(C) when C >= $2 andalso C =< $7 ->
    C - 24;

decode_char(C) when C >= $a andalso C =< $z ->
    C - $a;

decode_char(C) when C >= $A andalso C =< $Z ->
    C - $A.


%% =============================================================================
%% EUNIT
%% =============================================================================


-ifdef(TEST).

cases() ->
    [
        %% RFC 4648 examples
        {<<"">>, <<"">>},
        {<<"f">>, <<"MY======">>},
        {<<"fo">>, <<"MZXQ====">>},
        {<<"foo">>, <<"MZXW6===">>},
        {<<"foob">>, <<"MZXW6YQ=">>},
        {<<"fooba">>, <<"MZXW6YTB">>},
        {<<"foobar">>, <<"MZXW6YTBOI======">>}
    ].


upper_encode_test_() ->
    [ ?_assertEqual(Out, encode(In)) || {In, Out} <- cases()].


upper_decode_test_() ->
    [ ?_assertEqual(In, decode(Out)) || {In, Out} <- cases()].


lower_encode_test_() ->
    [
        ?_assertEqual(string:lowercase(Out), encode(In, #{lowercase => true}))
        || {In, Out} <- cases()
    ].


lower_decode_test_() ->
    [
        ?_assertEqual(In, decode(string:lowercase(Out)))
        || {In, Out} <- cases()
    ].

nopad_encode_test_() ->
    [
        ?_assertEqual(
            binary:replace(Out, <<"=">>, <<>>, [global]),
            encode(In, #{padding => false})
        )
        || {In, Out} <- cases()].


-endif.