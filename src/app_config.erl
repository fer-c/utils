%% =============================================================================
%%  app_config.erl -
%%
%%  Copyright (c) 2016-2019 Ngineo Limited t/a Leapsight. All rights reserved.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-module(app_config).

-define(ERROR, '$error_badarg').
-define(OPTS_KEY, '$app_config_opts').

-export([get/2]).
-export([get/3]).
-export([init/1]).
-export([init/2]).
-export([set/3]).

-compile({no_auto_import, [get/1]}).




%% =============================================================================
%% CALLBACKS
%% =============================================================================



-callback will_set(Key :: key_value:key(), Value :: any()) ->
    ok | {ok, NewValue :: any()} | {error, Reason :: any()}.


-callback on_set(Key :: key_value:key(), Value :: any()) -> ok.


-optional_callbacks([on_set/2]).
-optional_callbacks([will_set/2]).



%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
init(App) ->
    %% Init from environment
    Config0 = application:get_all_env(App),
    Config1 = [{priv_dir, priv_dir(App)} | Config0],
    %% We initialise the config, caching all values as code
    %% We set configs at first level only
    _ = [set(App, Key, Value) || {Key, Value} <- Config1],
    ok.


%% -----------------------------------------------------------------------------
%% @doc Initilises and app config with options.
%% Options are:
%%
%% * callback_mod - the callback module implement some or all of the callbacks
%% @end
%% -----------------------------------------------------------------------------
init(App, Opts) when is_map(Opts) andalso map_size(Opts) == 0 ->
    init(App);

init(App, #{callback_mod := _} = Opts) ->
    ok = do_set(App, ?OPTS_KEY, Opts),
    init(App).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get(App :: atom(), Key :: list() | atom() | tuple()) -> term().

get(App, Key) ->
    get(App, Key, ?ERROR).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get(
    App :: atom(), Key :: list() | atom() | tuple(), Default :: term()) ->
    term().

get(App, [H|T], Default) ->
    Result = case get(App, H, Default) of
        Term when is_map(Term) orelse is_list(Term) ->
            key_value:get(T, Term, Default);
        _ ->
            %% We cannot get(T) from a term which is neither a map nor a list
            Default
    end,
    maybe_badarg(Result);

get(App, Key, Default) when is_tuple(Key) ->
    get(App, tuple_to_list(Key), Default);

get(App, Key, Default) ->
    %% persistent_term:get({App, Key}, undefined).
    maybe_badarg(persistent_term:get({App, Key}, Default)).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec set(App :: atom(), Key :: key_value:key(), Default :: term()) ->
    ok | no_return().

set(App, Key, Value) when is_tuple(Key) ->
    set(App, tuple_to_list(Key), Value);

set(_, [], _)  ->
    error(badarg);

set(App, Key, Value)
when is_atom(App) andalso
(is_atom(Key) orelse is_binary(Key) orelse is_list(Key)) ->
    case maybe_will_set(App, Key, Value) of
        ok ->
            ok = do_set(App, Key, Value),
            maybe_on_set(App, Key, Value);
        {ok, NewValue} ->
            ok = do_set(App, Key, NewValue),
            maybe_on_set(App, Key, NewValue);
        {error, Reason} ->
            error(Reason)
    end.



%% =============================================================================
%% PRIVATE
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Returns the app's priv dir
%% @end
%% -----------------------------------------------------------------------------
priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            filename:join(
                [filename:dirname(code:which(?MODULE)), "..", "priv"]);
        Val ->
            Val
    end.


maybe_badarg(?ERROR) ->
    error(badarg);

maybe_badarg(Term) ->
    Term.


%% @private
do_set(App, [Key], Value) ->
    do_set(App, Key, Value);

do_set(App, [H|T], Value) ->
    KVTerm = key_value:set(T, Value, get(App, H, [])),
    do_set(App, H, KVTerm);

do_set(App, Key, Value) ->
    application:set_env(App, Key, Value),
    persistent_term:put({App, Key}, Value).


%% @private
maybe_will_set(App, Key, Value) ->
    case erlang:get({?MODULE, events_enabled}) of
        false ->
            ok;
        _ ->
            will_set(App, Key, Value)
    end.


%% @private
will_set(App, Key, Value) ->
    %% We disable events to avoid endless loops
    _ = put({?MODULE, events_enabled}, false),

    try get(App, ?OPTS_KEY, undefined) of
        #{callback_mod := Mod} ->
            case erlang:function_exported(Mod, will_set, 2) of
                true ->
                    Mod:will_set(Key, Value);
                false ->
                    ok
            end;
        _ ->
            ok
    after
        %% We disable events to avoid endless loops
        _ = put({?MODULE, events_enabled}, true)
    end.


%% @private
maybe_on_set(App, Key, Value) ->
    case erlang:get({?MODULE, events_enabled}) of
        false ->
            ok;
        _ ->
            on_set(App, Key, Value)
    end.


%% @private
on_set(App, Key, Value) ->
    %% We disable events to avoid endless loops
    %% in case Mod:on_set/2 calls our functions
    _ = put({?MODULE, events_enabled}, false),

    try get(App, ?OPTS_KEY, undefined) of
        #{callback_mod := Mod} ->
            case erlang:function_exported(Mod, on_set, 2) of
                true ->
                    Mod:on_set(Key, Value);
                false ->
                    ok
            end;
        _ ->
            ok
    after
        %% We re-enable events
        _ = put({?MODULE, events_enabled}, true)
    end.

