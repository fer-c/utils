-module(error_maps).


-type error_map() :: #{
    code => atom(),
    message => binary(), % localisable
    description => binary(), % for developer in EN
    errors => [#{
        code => atom(),
        key => binary(), %  optional
        value => term(), %  optional
        message => binary(), % localisable
        description => binary() % for developer in EN
    }]
}.

-export_type([error_map/0]).
-export([new/3]).
-export([add_error/5]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec new(atom(), binary(), binary()) -> error_map().

new(Code, Msg, Desc) ->
    #{
        code => Code,
        message => Msg,
        description => Desc
    }.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec add_error(map(), atom(), atom() | binary(), binary(), binary()) -> 
    error_map().

add_error(Map, Code, Prop, Msg, Desc) ->
    L = maps:get(errors, Map, []),
    E = #{
        code => Code,
        key => Prop,
        message => Msg,
        description => Desc  
    },
    Map#{errors => [E|L]}.

