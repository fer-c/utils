-module(inet_utils).

-type peername()    ::  {inet:ip_address(), inet:port_number()}.

-export([peername_to_binary/1]).
-export([binary_to_peername/1]).
-export([binary_to_ip_address/1]).
-export([integer_to_ip_address/1]).
-export([ip_address_to_binary/1]).
-export([ip_address_to_integer/1]).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec peername_to_binary(peername()) -> binary().

peername_to_binary({IP, Port}) when Port >= 0, Port =< 65535 ->
    case inet:ntoa(IP) of
        {error, einval} ->
            exit(badarg);
        Str ->
            <<
                (list_to_binary(Str))/binary,
                $:, (integer_to_binary(Port))/binary
            >>
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec binary_to_peername(binary()) -> peername().

binary_to_peername(Bin) ->
    case binary:split(Bin, <<$:>>) of
        [IP, Port] ->
            {binary_to_ip_address(IP), binary_to_port(Port)};
        _ ->
            exit(badarg)
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
binary_to_ip_address(Bin) ->
    case inet:parse_address(binary_to_list(Bin)) of
        {ok, IP} -> IP;
        _ -> exit(badarg)
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
integer_to_ip_address(N) ->
    {N bsr 24, (N band 16711680) bsr 16, (N band 65280) bsr 8, N band 255}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
ip_address_to_binary(IP) ->
    case inet:ntoa(IP) of
        {error, einval} ->
            exit(badarg);
        Str ->
            list_to_binary(Str)
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
ip_address_to_integer({A, B, C, D}) ->
    (A * 16777216) + (B * 65536) + (C * 256) + (D).





%% =============================================================================
%% PRIVATE
%% =============================================================================


binary_to_port(Bin) ->
    case binary_to_integer(Bin) of
        N when N >= 0 andalso N =< 65535 ->
            N;
        _ ->
            exit(badarg)
    end.
