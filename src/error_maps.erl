%% =============================================================================
%% error_maps - standard representation of errors using maps
%%
%% Copyright (c) 2016-1027 Leapsight. All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% =============================================================================
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

