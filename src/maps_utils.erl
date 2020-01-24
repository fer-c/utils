%% =============================================================================
%% maps_utils - a collection of utilities functions for erlang maps.
%%
%% Copyright (c) 2016-1027 Ngineo Limited t/a Leapsight. All rights reserved.
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
-module(maps_utils).



%% =============================================================================
%% VALIDATION
%% =============================================================================
-define(ALLOW_NULL, true).
-define(ALLOW_UNDEFINED, false).

-define(INVALID_DATA_CODE, invalid_data).
-define(INVALID_DATA_MSG(N),
    iolist_to_binary([<<"There were ">>, term_to_iolist(N), <<" validation errors.">>])
).
-define(INVALID_DATA_DESC(N),
    iolist_to_binary([<<"There were ">>, term_to_iolist(N), <<" validation errors.">>])
).

-define(ERROR_INVALID_DATA(L, M), #{
    code => ?INVALID_DATA_CODE,
    message => M,
    description => ?INVALID_DATA_DESC(length(L)),
    errors => L
}).

-define(MISSING_REQUIRED_VALUE_MSG(K),
    iolist_to_binary([
        <<"A value for '">>, term_to_iolist(K), <<"' is required.">>])
).

-define(ERROR_MISSING_REQUIRED_VALUE(K, M), #{
    code => missing_required_value,
    key => K,
    message => M,
    description => iolist_to_binary([
        <<"A value for '">>, term_to_iolist(K), <<"' is required.">>])
}).


-define(INVALID_DATATYPE_MSG(K, DT),
    iolist_to_binary([
        <<"The value for '">>, term_to_iolist(K),
        <<"' is not a of type '">>, term_to_iolist(DT), <<"'">>
    ])
).

-define(ERROR_INVALID_DATATYPE(K, DT, M), #{
    code => invalid_datatype,
    key => K,
    message => M,
    description => iolist_to_binary([
        <<"The value for '">>, term_to_iolist(K),
        <<"' is not a of type '">>, term_to_iolist(DT), <<"'.">>
    ])
}).


-define(INVALID_VALUE_MSG(K, V),
    iolist_to_binary([
        <<"The value '">>, term_to_iolist(V),
        <<"' for '">>, term_to_iolist(K), <<"' is not valid.">>])
).

-define(ERROR_INVALID_VALUE(K, V, M), #{
    code => invalid_value,
    key => K,
    value => V,
    message => M,
    description => iolist_to_binary([
        <<"The value '">>, term_to_iolist(V),
        <<"' for '">>, term_to_iolist(K), <<"' did not pass the validator.">>])
}).

-type base_datatype()          ::   atom
                                    | binary
                                    | bitstring
                                    | boolean
                                    | float
                                    | function
                                    | integer
                                    | list
                                    | map
                                    | neg_integer
                                    | non_neg_integer
                                    | number
                                    | pid
                                    | port
                                    | pos_integer
                                    | reference
                                    | string
                                    | timeout
                                    | tuple
                                    | {function, N :: non_neg_integer()}
                                    | {record, Name :: atom()}.

-type compound()                ::  [base_datatype()]
                                    | {in, [base_datatype()]}
                                    | {not_in, [base_datatype()]}.

-type datatype()                ::  base_datatype()
                                    | compound()
                                    | {list, base_datatype() | compound()}.

-type validator_fun()               ::  fun((term()) ->
                                        {ok, any()}
                                        | {merge, fun((map()) -> map())}
                                        | boolean()
                                        | error
                                    ).

-type validator()               ::  {list, validator_fun()}
                                    | {list, entry_spec()}
                                    | {map, {base_datatype(), validator_fun()}}
                                    | {map, {base_datatype(), entry_spec()}}
                                    | {map, {
                                        base_datatype(),
                                        [validator_fun() | entry_spec()]}}
                                    | [
                                        validator_fun()
                                        | entry_spec()
                                        | {list, entry_spec()}
                                    ]
                                    | validator_fun()
                                    | entry_spec().


-type entry_spec()              ::  #{
                                        key => any(), %% a new name for the Key
                                        alias => any(),
                                        required => boolean(),
                                        allow_undefined => boolean() | remove,
                                        allow_null => boolean() | remove,
                                        default => term() | fun(() -> any()), %% only if required
                                        datatype => datatype() | [datatype()],
                                        validator => validator()
                                    }.
-type map_spec()                ::  #{term() => entry_spec()}.

-type update_validator()        ::  [entry_update_spec()]
                                    | entry_update_spec()
                                    | fun((term()) -> boolean()).

-type entry_update_spec()       ::  #{
                                        key => any(), %% a new name for the Key
                                        alias => any(),
                                        update_validator => update_validator()
                                    }.
-type map_update_spec()         ::  #{term() => entry_update_spec()}.

-type error_fun()   :: fun((Key :: term(), Cnt :: integer()) -> binary()).
-type key_error_fun() :: fun((Key :: term(), Type :: datatype()) -> binary()).

-type formatters() ::  #{
    invalid_data => error_fun(),
    invalid_value => key_error_fun(),
    invalid_datatype => key_error_fun(),
    missing_required_value => key_error_fun()
}.

-type validation_opts() ::  #{
    atomic => boolean(),
    keep_unknown => boolean(),
    unknown_label_validator => fun((term()) -> boolean()),
    labels => binary | atom | existing_atom | attempt_atom,
    error_code => atom(),
    error_formatters => formatters()
}.

-type path()    ::  [term()].


-export_type([datatype/0]).
-export_type([validator/0]).
-export_type([entry_spec/0]).
-export_type([map_spec/0]).


-export([append/3]).
-export([append_list/3]).
-export([append_list_path/3]).
-export([append_path/3]).
-export([collect/2]).
-export([get_path/2]).
-export([get_path/3]).
-export([is_any_key/2]).
-export([are_all_keys/2]).
-export([get_any/2]).
-export([get_any/3]).
-export([put_path/3]).
-export([remove_path/2]).
-export([split/2]).
-export([validate/2]).
-export([validate/3]).
-export([validate_update/3]).
-export([validate_update/4]).
-export([with_paths/2]).
-export([without_paths/2]).
-export([merge/3]).
-export([to_property_list/1]).




%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% A wrapper for {@link maps:get/2} that supports key paths in the form
%% of a list of keys.
%%
%% Example:
%% <code language=erlang>
%% 42 =:= maps_utils:get_path([foo, bar], #{foo => #{bar => 42}}).
%% </code>
%% @end
%% -----------------------------------------------------------------------------
-spec get_path(Path :: path(), Map :: map()) -> Value :: term().

get_path(Path, Map) ->
    get_path(Path, Map, '$badkey').


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_path(Path :: path(), Map :: map(), Default :: term()) ->
    Value :: term().

get_path([], _, _) ->
    error({badkey, []});

get_path(Path, Map, Default)  when is_list(Path) ->
    do_get_path(Path, Map, Default).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_any_key(list(), map()) -> boolean().

is_any_key([H|T], M) ->
    maps:is_key(H, M) orelse is_any_key(T, M);

is_any_key([], _) ->
    false.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec are_all_keys(list(), map()) -> boolean().

are_all_keys(L, M) ->
    sets:is_subset(sets:from_list(L), sets:from_list(maps:keys(M))).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_any(list(), map()) -> term().

get_any(Keys, Map) ->
    case do_get_any(Keys, Map) of
        '$error' ->
            error({badkeys, Keys});
        Val ->
            Val
    end.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_any(list(), map(), term()) -> term().

get_any(Keys, Map, Default) ->
    case do_get_any(Keys, Map) of
        '$error' ->
            Default;
        Val ->
            Val
    end.




%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec put_path(Key :: [term()], Value :: term(), Map :: map()) -> map().

put_path([], _, _) ->
     error({badkey, []});

put_path(Path, Value, Map) when is_list(Path) ->
    do_put_path(Path, Value, Map).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec remove_path(Key :: path(), Map :: map()) -> map().

remove_path([], _) ->
     error({badkey, []});

remove_path(Path, Map) when is_list(Path) ->
    do_remove_path(Path, Map).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec with_paths(PathList :: [path()], Map :: map()) -> map().

with_paths([], _) ->
    #{};

with_paths(Ps, M) when is_list(Ps), is_map(M) ->
    Fun = fun(P, Acc) ->
        case get_path(P, M, '$error') of
            '$error' ->
                Acc;
            Val ->
                put_path(P, Val, Acc)
        end
    end,
    lists:foldl(Fun, #{}, Ps);

with_paths(Ps, M) ->
    erlang:error(error_type(M), [Ps, M]).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec without_paths(PathList :: [path()], Map :: map()) -> map().

without_paths([], M) ->
    M;

without_paths(Ps, M) when is_list(Ps), is_map(M) ->
    Fun = fun(P, Acc) ->
      remove_path(P, Acc)
    end,
    lists:foldl(Fun, M, Ps);

without_paths(Ps, M) ->
    erlang:error(error_type(M), [Ps, M]).


%% -----------------------------------------------------------------------------
%% @doc
%% This function appends a new Value to the current list of values associated
%% with Key.
%% @end
%% -----------------------------------------------------------------------------
append(Key, Value, Map) ->
    case maps:get(Key, Map, []) of
        Values when is_list(Values) ->
            maps:update(Key, [Value|Values], Map);
        Prev ->
            maps:put(Key, [Value, Prev], Map)
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
append_path([], _, _) ->
     error({badkey, []});

append_path(Path, Value, Map) when is_list(Path) ->
    do_append_list_path(Path, [Value], Map).


%% -----------------------------------------------------------------------------
%% @doc
%% This function appends a list of values Values to the current list of values
%% associated with Key. An exception is generated if the initial value
%% associated with Key is not a list of values.
%% @end
%% -----------------------------------------------------------------------------
append_list(Key, Values, Map) when is_list(Values) ->
    case maps:get(Key, Map, []) of
        OldValues when is_list(OldValues) ->
            maps:update(Key, lists:append(OldValues, Values), Map);
        Val ->
            error({badlist, Val})
    end;

append_list(Key, Values, Map) ->
    error({badarg, [Key, Values, Map]}).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
append_list_path([], _, _) ->
     error({badkey, []});

append_list_path(Path, Value, Map) when is_list(Path) ->
    do_append_list_path(Path, Value, Map).


%% -----------------------------------------------------------------------------
%% @doc
%% Collects the values for Keys preserving its order
%% @end
%% -----------------------------------------------------------------------------
-spec collect(Keys :: list(), Map :: map()) -> list().

collect(Keys, Map) ->
    L = [begin
        case maps:find(K, Map) of
            {ok, V} -> V;
            error -> not_found
        end
    end || K <- Keys],
    lists:filter(fun(not_found) -> false; (_) -> true end, L).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec split(list(), map()) -> {map(), map()}.

split(L, Map) ->
    {maps:with(L, Map), maps:without(L, Map)}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec merge(
    fun((Key :: any(), Value1 :: any(), Value2 :: any()) -> Value :: term()), map(),
    map()
) -> map().

merge(Fun, A, B) ->
    maps:from_list(
        orddict:to_list(
        orddict:merge(Fun,
                        orddict:from_list(maps:to_list(A)),
                        orddict:from_list(maps:to_list(B))))).


%% -----------------------------------------------------------------------------
%% @doc Returns a property list representation of a map. As opposed to
%% maps:to_list/1 this function works recursively, turning each value of type
%% map() into a property list.
%% @end
%% -----------------------------------------------------------------------------
-spec to_property_list(Map :: map()) -> [proplists:property()].

to_property_list(Map) ->
    to_property_list(maps:next(maps:iterator(Map)), []).

%% @private
to_property_list({K, V, I}, Acc) when is_map(V) ->
    NewAcc = [{K, to_property_list(V)} | Acc],
    to_property_list(maps:next(I), NewAcc);

to_property_list({K, V, I}, Acc) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true ->
            to_property_list(maps:next(I), [{K, V} | Acc]);
        false ->
            NewAcc = [{K, [to_property(X) || X <- V]} | Acc],
            to_property_list(maps:next(I), NewAcc)
    end;

to_property_list({K, V, I}, Acc) ->
    to_property_list(maps:next(I), [{K, V} | Acc]);

to_property_list(none, Acc) ->
    Acc.

%% @private
to_property({K, V}) ->
    {K, to_property_list(V)};

to_property(Term) when is_map(Term) ->
    to_property_list(Term);

to_property(Term) ->
    Term.

%% -----------------------------------------------------------------------------
%% @doc
%% Calls validate/3 with the defaults options.
%% @end
%% -----------------------------------------------------------------------------
-spec validate(Map :: map(), Spec :: map_spec()) -> ValidMap :: map().

validate(Map0, Spec) when is_map(Spec) ->
    validate(Map0, Spec, #{}).


%% -----------------------------------------------------------------------------
%% @doc
%% Calls validate_update/4 with the defaults options.
%% @end
%% -----------------------------------------------------------------------------
-spec validate_update(Map :: map(), Map :: map(), Spec :: map_update_spec()) -> ok | no_return().

validate_update(Map0, Changes, Spec) when is_map(Spec) ->
    validate_update(Map0, Changes, Spec, #{}).


%% -----------------------------------------------------------------------------
%% @doc
%% Returns a new map.
%% If any validation failes ir will do so with an exception with Reason of
%% type error_map:error_map().
%%
%% For example:
%% <code>
%% #{code => invalida_data,
%%    description => <<"Multiple validation errors">>,
%%    errors => [
%%        #{
%%            code => missing_required_value,
%%            description => <<"A value for 'foo' is required.">>,
%%            message => <<"A value for 'foo' is required.">>,
%%            key => foo
%%        },
%%        #{
%%            code => invalid_datatype,
%%            description => <<"The value for 'bar' is not a of type 'map'">>,
%%            message => <<"The value for 'bar' is not a of type 'map'">>,
%%            key => bar}],
%%            message => <<"Multiple validation errors">>
%%        }
%% }
%% <\code>
%%
%% Error codes
%% * invalid_data - the general error code use when aggregating multiple errors
%% * missing_required_value -  a value for a required key was not found (and no default
%% value was provided)
%% * invalid_datatype - the value provided does not match the datatype specified
%% * invalid_value - the value failed the validator provided
%% @end
%% -----------------------------------------------------------------------------
-spec validate(InMap :: map(), Spec :: map_spec(), validation_opts()) ->
    OutMap :: map() | no_return().

validate(InMap0, Spec, Opts) when is_map(Spec), is_map(Opts) ->
    case do_validate(InMap0, Spec, Opts) of
        {error, Reason} ->
            error(Reason);
        Out ->
            maybe_cleanup(InMap0, Spec, Opts, Out)
    end.


maybe_cleanup(_In, _Spec, #{keep_unknown := false}, Out) ->
    %% We just returned the validated map, ignoring unknown keys in In map
    Out;

maybe_cleanup(
    In, Spec, #{keep_unknown := true, unknown_label_validator := Fun}, Out) when is_function(Fun, 1) ->
    Unknowns = lists:subtract(maps:keys(In), maps:keys(Out)),
    InvalidUnknowns = lists:filter(fun(X) -> not Fun(X) end, Unknowns),
    maps:merge(cleanup(In, Spec, InvalidUnknowns), Out);

maybe_cleanup(In, Spec, #{keep_unknown := true}, Out) ->
    maps:merge(cleanup(In, Spec, []), Out);

maybe_cleanup(_In, _Spec, _, Out) ->
    %% We default to keep_unknown => false
    Out.


%% @private
cleanup(In, Spec, ToRemove0) ->
    CollectAliases = fun
        (_, #{alias := Alias}, Acc) ->
            [Alias|Acc];
        (_, _, Acc) ->
            Acc
    end,
    ToRemove1 = maps:fold(CollectAliases, ToRemove0, Spec),
    maps:without(ToRemove1, In).

%% @private
do_validate(Map0, Spec, Opts) when is_map(Spec), is_map(Opts) ->
    Err0 = case maps:get(atomic, Opts, true) of
        true -> [];
         _ -> false
    end,

    Res =  maps:fold(fun validate_fold_fun/3, {Map0, #{}, Err0, Opts}, Spec),

    case Res of
        {Map0, Map1, Err0, _} ->
            Map1;
        {Map0, _, [E], _} ->
            {error, E};
        {Map0, _, L, _} ->
            {error, invalid_data_error(L, Opts)}
    end.

%% -----------------------------------------------------------------------------
%% @doc
%% Returns if the validation succeeded.
%%
%% @end
%% -----------------------------------------------------------------------------
-spec validate_update(Map :: map(), Changes :: map(), Spec :: map_update_spec(), validation_opts()) ->
    ok | no_return().

validate_update(Map0, Changes, Spec, Opts) when is_map(Changes), is_map(Spec), is_map(Opts) ->
    Res = validate_update_error_list(Map0, Changes, Spec, Opts),
    case Res of
        [] ->
            ok;
        [{error, E}] ->
            error(E);
        [{error, E}|T] ->
            case maps:get(atomic, Opts, true) of
                true -> error(E);
                _ ->  error(invalid_data_error([{error, E}|T], Opts))
            end
    end.

validate_update_error_list(Map0, Changes, Spec, Opts) when is_map(Changes), is_map(Spec), is_map(Opts) ->
    maps:fold(fun(K, V, Accum) -> Accum++validate_update_key(K, V, Map0, Changes, Opts) end, [], Spec).






%% =============================================================================
%% PRIVATE
%% =============================================================================


%% @private
do_get_path([Key], Map, Default) ->
    maybe_get(Key, Map, Default);

do_get_path([H|T], Map, Default) ->
    case maybe_get(H, Map, Default) of
        Default -> Default;
        Val -> do_get_path(T, Val , Default)
    end;

do_get_path([], Map, _) ->
    Map;

do_get_path(Key, Map, Default) ->
    maybe_get(Key, Map, Default).


%% @private
maybe_get(Key, Map, Default) ->
    case maps:get(Key, Map, Default) of
        '$badkey' ->
            error({badkey, Key});
        Value ->
            Value
    end.


%% @private
do_put_path([Key], Value, Map) ->
    maps:put(Key, Value, Map);

do_put_path([H|T], Value, Map) when is_map(Map) ->
    maps:put(H, do_put_path(T, Value, maps:get(H, Map, #{})), Map);

do_put_path([H|_], _, Term) when is_list(H) ->
    error({badmap, Term});

do_put_path([], _, Map) ->
    Map;

do_put_path(Key, Value, Map) ->
    maps:put(Key, Value, Map).


%% @private
do_append_list_path([Key], Value, Map) ->
    append_list(Key, Value, Map);

do_append_list_path([H|T], Value, Map) when is_map(Map) ->
    maps:put(H, do_append_list_path(T, Value, maps:get(H, Map, #{})), Map);

do_append_list_path([H|_], _, Term) when is_list(H) ->
    error({badmap, Term});

do_append_list_path([], _, Map) ->
    Map;

do_append_list_path(Key, Value, Map) ->
    append_list(Key, Value, Map).


%% @private
do_remove_path([Key], Map) ->
    maps:remove(Key, Map);

do_remove_path([H|T], Map) when is_map(Map) ->
    case maps:find(H, Map) of
        {ok, Val} when is_map(Val) ->
            maps:put(H, do_remove_path(T, Val), Map);
        {ok, _} ->
            Map;
        error ->
            Map
    end;

do_remove_path([H|_], Term) when is_list(H) ->
    error({badmap, Term});

do_remove_path([], Map) ->
    Map;

do_remove_path(Key, Map) ->
    maps:remove(Key, Map).


%% @private
do_get_any([H|T], Map) ->
    case maps:get(H, Map, '$error') of
        '$error' ->
            do_get_any(T, Map);
        Val ->
            Val
    end;

do_get_any([], _) ->
    '$error'.


%% =============================================================================
%% PRIVATE : MAP VALIDATION
%% =============================================================================


%% private
b2o(Term, binary) ->
    Term;
b2o(Term, atom) ->
    binary_to_atom(Term, utf8);
b2o(Term, existing_atom) ->
    binary_to_existing_atom(Term, utf8);
b2o(Term, attempt_atom) ->
    try
        binary_to_existing_atom(Term, utf8)
    catch
        _:_ ->
            Term
    end.

%% private
l2o(Term, binary) ->
    unicode:characters_to_binary(Term, utf8);
l2o(Term, atom) ->
    list_to_atom(Term);
l2o(Term, existing_atom) ->
    list_to_existing_atom(Term);
l2o(Term, attempt_atom) ->
    try
        list_to_existing_atom(Term)
    catch
        _:_ ->
            Term
    end.

%% private
i2o(Term, Opt) ->
    b2o(integer_to_binary(Term), Opt).


%% @private
maybe_rename_key(_, #{key := Key}, _) ->
    %% This overrides any label global option
    Key;

maybe_rename_key(K, _, #{labels := Opt}) when is_binary(K) ->
    b2o(K, Opt);

maybe_rename_key(K, _, #{labels := Opt}) when is_list(K) ->
    l2o(K, Opt);

maybe_rename_key(K, _, #{labels := binary}) when is_atom(K) ->
    atom_to_binary(K, utf8);

maybe_rename_key(K, _, #{labels := _Atom}) when is_atom(K) ->
    K;

maybe_rename_key(K, _, #{labels := Opt}) when is_integer(K) ->
    i2o(K, Opt);

maybe_rename_key(K, _, _) ->
    K.


%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Calls validate_key/4 for every K in the Spec in any order.
%% @end
%% -----------------------------------------------------------------------------
validate_fold_fun(K, KSpec, {In, Out, Err, Opts}) when is_map(KSpec) ->

    case validate_key(K, In, KSpec, Opts) of
        {ok, Val} ->
            %% Maybe we rename the key
            NewKey = maybe_rename_key(K, KSpec, Opts),
            {In, maps:put(NewKey, Val, Out), Err, Opts};

        {merge, Fun} when is_function(Fun, 1) ->
            %% Maybe we rename the key
            NewKey = maps:get(key, KSpec, K),
            Val = Fun(maps:get(NewKey, Out, undefined)),
            {In, maps:put(NewKey, Val, Out), Err, Opts};

        {error, Reason} when is_list(Err) ->
            %% atomic is true
            {In, Out, [Reason|Err], Opts};

        {error, Reason} ->
            error(Reason);

        remove ->
            {In, maps:without([K], Out), Err, Opts};

        not_found ->
            {In, Out, Err, Opts}
    end;

validate_fold_fun(K, KSpec, _) ->
    error(badarg, [K, KSpec]).


%% @private
validate_key(K, In, KSpec, Opts) ->

    case find(K, In, KSpec) of
        {ok, null} ->
            NewKSpec = maps:merge(#{allow_null => ?ALLOW_NULL}, KSpec),
            maybe_allow(K, null, NewKSpec, Opts);

        {ok, undefined} ->
            NewKSpec = maps:merge(
                #{allow_undefined => ?ALLOW_UNDEFINED}, KSpec),
            maybe_allow(K, undefined, NewKSpec, Opts);

        {ok, V} ->
            case is_valid_datatype(V, KSpec) of
                true ->
                    maybe_eval(K, V, KSpec, Opts);
                false ->
                    Reason = invalid_datatype_error(
                        K, maps:get(datatype, KSpec), Opts),
                    {error, Reason}
            end;

        error ->
            maybe_get_default(K, KSpec, Opts)
    end.

%% @private
validate_update_key(K, KSpec, In, Changes, Opts) ->
    case find(K, Changes, KSpec) of
        {ok, VChange} ->
            case find(K, In, KSpec) of
                {ok, V} -> maybe_eval_update(K, V, KSpec, VChange, Opts);
                _ -> []
            end;
        _ -> []
    end.


find(K1, Map, Spec) ->
    case {maps:find(K1, Map), Spec} of
        {error, #{alias := K2}} ->
            maps:find(K2, Map);
        {error, _} ->
            error;
        {OK, _} ->
            OK
    end.


%% @private
maybe_allow(_, null, #{allow_null := true}, _) ->
    {ok, null};

maybe_allow(_, undefined, #{allow_undefined := true}, _) ->
    {ok, undefined};

maybe_allow(_, null, #{allow_null := remove}, _) ->
    remove;

maybe_allow(_, undefined, #{allow_undefined := remove}, _) ->
    remove;

maybe_allow(K, null, #{allow_null := false} = KSpec, Opts) ->
    maybe_eval(K, null, KSpec, Opts);

maybe_allow(K, undefined, #{allow_undefined := false} = KSpec, Opts) ->
    maybe_eval(K, undefined, KSpec, Opts).





%% @private
maybe_get_default(_, #{required := true, default := F}, _)
when is_function(F, 0) ->
    {ok, F()};

maybe_get_default(_, #{required := true, default := V}, _) ->
    {ok, V};

maybe_get_default(K, #{required := true}, Opts) ->
    {error, missing_required_value_error(K, Opts)};

maybe_get_default(_, _, _) ->
    not_found.


%% @private
maybe_eval_update(K, V, #{update_validator := Fun}, Change, Opts) when is_function(Fun, 2) ->
    case Fun(V, Change) of
        true ->
            [];
        false ->
            [{error, invalid_value_error(K, Change, Opts)}];
        _ ->
            error({invalid_validator_return_value, K})
    end;

maybe_eval_update(_, V, #{update_validator := Spec}, Changes, Opts) when is_map(V), is_map(Spec) ->
    validate_update_error_list(V, Changes, Spec, Opts);

maybe_eval_update(_, V, #{update_validator := Spec}, Changes, Opts) when is_map(V), is_list(Spec) ->
    FoldFun = fun (S, Accum) -> Accum++validate_update_error_list(V, Changes, S, Opts) end,
    lists:foldl(FoldFun, [], Spec);

maybe_eval_update(_, _, #{update_validator := Spec}, _Changes, _Opts) when not is_list(Spec) andalso not is_map(Spec) ->
   error(badarg, Spec); %Invalid Spec

maybe_eval_update(_, V, #{update_validator := _}, _Changes, _Opts) ->
   error(badarg, V); %No deberia pasar pues V se valida antes

maybe_eval_update(_, _, _, _, _) ->
    [].

%% @private
maybe_eval(K, V, KSpec, Opts) ->
    try
        do_maybe_eval(K, V, KSpec, Opts)
    catch
        throw:Reason ->
            {error, Reason}
    end.


%% @private
do_maybe_eval(K, V, #{validator := {list, _}}, Opts) when not is_list(V) ->
    throw(invalid_datatype_error(K, list, Opts));

do_maybe_eval(K, V, #{validator := {list, Fun}}, Opts)
when is_list(V), is_function(Fun, 1) ->

    Inner = fun(E, Acc) ->
        case Fun(E) of
            {ok, Val} -> [Val|Acc];
            true -> [E|Acc];
            false -> throw(invalid_value_error(K, V, Opts));
            error -> throw(invalid_value_error(K, V, Opts));
            _ ->
                error({invalid_validator_return_value, K})
        end
    end,

    try
        {ok, lists:reverse(lists:foldl(Inner, [], V))}
    catch
        throw:Error when is_map(Error) ->
            {error, Error}
    end;

do_maybe_eval(K, V, #{validator := {map, {KeyType, Fun}}}, Opts)
when is_map(V), is_function(Fun, 1) ->

    Inner = fun(Key, Value) ->
        is_valid_datatype(Key, #{datatype => KeyType}) orelse
        throw(invalid_value_error(K, V, Opts)),
        case Fun(Value) of
            {ok, NewValue} -> NewValue;
            true -> Value;
            false -> throw(invalid_value_error(K, V, Opts));
            error -> throw(invalid_value_error(K, V, Opts));
            _ ->
                error({invalid_validator_return_value, K})
        end
    end,

    try
        {ok, maps:map(Inner, V)}
    catch
        throw:Error when is_map(Error) ->
            {error, Error}
    end;

do_maybe_eval(K, V, #{validator := {list, Spec}}, Opts)
when is_list(V), is_map(Spec) ->

    Inner = fun
        (E, Acc) when is_map(E) ->
            case do_validate(E, Spec, Opts) of
                {error, Reason} ->
                    throw(Reason);
                Val ->
                    [Val | Acc]
            end;
        (_, _) ->
            throw(invalid_value_error(K, V, Opts))
    end,

    try
        {ok, lists:reverse(lists:foldl(Inner, [], V))}
    catch
        throw:Error when is_map(Error) ->
            {error, Error}
    end;

do_maybe_eval(K, V, #{validator := {map, {KeyType, Spec}}}, Opts)
when is_map(V), is_map(Spec) ->

    Inner = fun(Key, Value) ->
        is_valid_datatype(Key, #{datatype => KeyType}) orelse
        throw(invalid_value_error(K, V, Opts)),

        case do_validate(Value, Spec, Opts) of
            {error, Reason} ->
                throw(Reason);
            NewValue ->
                NewValue
        end
    end,

    try
        {ok, maps:map(Inner, V)}
    catch
        throw:Error when is_map(Error) ->
            {error, Error}
    end;

do_maybe_eval(K, V, #{validator := {map, {KeyType, Specs}}}, Opts)
when is_map(V), is_list(Specs) ->

    Inner = fun(Key, Value) ->
        is_valid_datatype(Key, #{datatype => KeyType}) orelse
        throw(invalid_value_error(K, V, Opts)),

        case validates_any(Key, Value, Specs, Opts) of
            {error, Reason} ->
                throw(Reason);
            {ok, NewValue} ->
                NewValue
        end
    end,

    try
        {ok, maps:map(Inner, V)}
    catch
        throw:TReason when is_map(TReason) ->
            {error, TReason}
    end;

do_maybe_eval(K, V, #{validator := Fun}, Opts) when is_function(Fun, 1) ->
    case Fun(V) of
        {ok, _} = OK ->
            OK;
        {merge, Merge} = OK when is_function(Merge, 1) ->
            OK;
        true ->
            {ok, V};
        false ->
            {error, invalid_value_error(K, V, Opts)};
        error ->
            {error, invalid_value_error(K, V, Opts)};
        _ ->
            error({invalid_validator_return_value, K})
    end;

do_maybe_eval(_, V, #{validator := Spec}, Opts) when is_map(V), is_map(Spec) ->
    case do_validate(V, Spec, Opts) of
        {error, _} = Error ->
            Error;
        Val ->
            {ok, Val}
    end;

do_maybe_eval(K, V, #{validator := ValidatorList}, Opts)
when is_list(ValidatorList) ->
    validates_any(K, V, ValidatorList, Opts);

do_maybe_eval(K, _, #{validator := Spec}, Opts) when is_map(Spec) ->
    %% Value is not a map
    {error, invalid_datatype_error(K, map, Opts)};

do_maybe_eval(K, V, #{validator := _} = Spec, Opts) ->
   error(badarg, [K, V, Spec, Opts]);

do_maybe_eval(K, null, _, Opts) ->
    {error, invalid_value_error(K, null, Opts)};

do_maybe_eval(K, undefined, _, Opts) ->
    {error, invalid_value_error(K, undefined, Opts)};

do_maybe_eval(_, V, _, _) ->
    {ok, V}.


%% @private
is_valid_datatype(_, #{datatype := []}) ->
    false;

is_valid_datatype(V, #{datatype := [H|T]}) ->
    is_valid_datatype(V, #{datatype => H})
    orelse is_valid_datatype(V, #{datatype => T});

is_valid_datatype(V, #{datatype := boolean}) when is_boolean(V) ->
    true;

is_valid_datatype(V, #{datatype := atom}) when is_atom(V) ->
    true;

is_valid_datatype(V, #{datatype := integer}) when is_integer(V) ->
    true;

is_valid_datatype(V, #{datatype := pos_integer})
when is_integer(V) andalso V > 0 ->
    true;

is_valid_datatype(V, #{datatype := neg_integer})
when is_integer(V) andalso V < 0 ->
    true;

is_valid_datatype(V, #{datatype := non_neg_integer})
when is_integer(V) andalso V >= 0 ->
    true;

is_valid_datatype(V, #{datatype := timeout})
when V =:= infinity orelse (is_integer(V) andalso V > 0) ->
    true;

is_valid_datatype(V, #{datatype := float}) when is_float(V) ->
    true;

is_valid_datatype(V, #{datatype := number}) when is_number(V) ->
    true;

is_valid_datatype(V, #{datatype := function}) when is_function(V) ->
    true;

is_valid_datatype(V, #{datatype := {function, N}}) when is_function(V, N) ->
    true;

is_valid_datatype(V, #{datatype := pid}) when is_pid(V) ->
    true;

is_valid_datatype(V, #{datatype := port}) when is_port(V) ->
    true;

is_valid_datatype(V, #{datatype := reference}) when is_reference(V) ->
    true;

is_valid_datatype(V, #{datatype := list}) when is_list(V) ->
    true;

is_valid_datatype(V, #{datatype := string}) when is_list(V) ->
    true;

is_valid_datatype(V, #{datatype := binary}) when is_binary(V) ->
    true;

is_valid_datatype(V, #{datatype := bitstring}) when is_bitstring(V) ->
    true;

is_valid_datatype(V, #{datatype := map}) when is_map(V) ->
   true;

is_valid_datatype(V, #{datatype := {record, Tag}}) when is_atom(Tag) ->
    is_record(V, Tag);

is_valid_datatype(V, #{datatype := tuple}) when is_tuple(V) ->
    true;

is_valid_datatype(V, #{datatype := {in, L}}) ->
    lists:member(V, L);

is_valid_datatype(V, #{datatype := {not_in, L}}) ->
    not lists:member(V, L);

is_valid_datatype(V, #{datatype := {list, {in, L}}}) when is_list(V) ->
    A = sets:from_list(V),
    B = sets:from_list(L),
    sets:size(A) == sets:size(sets:intersection([A, B]));

is_valid_datatype(V, #{datatype := {list, {not_in, L}}}) when is_list(V) ->
    A = sets:from_list(V),
    B = sets:from_list(L),
    sets:is_disjoint(A, B);

is_valid_datatype(V, #{datatype := {list, Type}}) when is_list(V) ->
    lists:all(
        fun(X) -> is_valid_datatype(X, #{datatype => Type}) end, V);


is_valid_datatype(_, #{datatype := T}) ->
    true = is_datatype(T),
    false;

is_valid_datatype(_, _) ->
    %% No spec
    true.


term_to_iolist(Term) when is_binary(Term) ->
    Term;

term_to_iolist(Term) ->
    io_lib:format("~p", [Term]).

%% @private
is_datatype(atom) -> true;
is_datatype(binary) -> true;
is_datatype(bitstring) -> true;
is_datatype(boolean) -> true;
is_datatype(float) -> true;
is_datatype(function) -> true;
is_datatype(integer) -> true;
is_datatype(list) -> true;
is_datatype(map) -> true;
is_datatype(neg_integer) -> true;
is_datatype(non_neg_integer) -> true;
is_datatype(number) -> true;
is_datatype(pid) -> true;
is_datatype(port) -> true;
is_datatype(pos_integer) -> true;
is_datatype(reference) -> true;
is_datatype(string) -> true;
is_datatype(timeout) -> true;
is_datatype(tuple) -> true;
is_datatype({function, N}) when is_integer(N), N >= 0 -> true;
is_datatype({record, Tag}) when is_atom(Tag) -> true;
is_datatype({in, L}) when is_list(L) -> true;
is_datatype({list, X}) -> is_datatype(X);
is_datatype({not_in, L}) when is_list(L) -> true;
is_datatype(L) when is_list(L) -> lists:all(fun is_datatype/1, L);
is_datatype(T) -> error({invalid_datatype, T}).




%% @private
invalid_data_error(L, #{error_formatters := #{invalid_data := Fun}}) ->
    ?ERROR_INVALID_DATA(L, Fun(length(L)));

invalid_data_error(L, _) ->
    ?ERROR_INVALID_DATA(L, ?INVALID_DATA_MSG(length(L))).


%% @private
missing_required_value_error(
    K, #{error_formatters := #{missing_required_value := Fun}}) ->
    ?ERROR_MISSING_REQUIRED_VALUE(K, Fun(K));

missing_required_value_error(K, _) ->
    ?ERROR_MISSING_REQUIRED_VALUE(K, ?MISSING_REQUIRED_VALUE_MSG(K)).


%% @private
invalid_datatype_error(
    K, DT, #{error_formatters := #{invalid_datatype := Fun}}) ->
    ?ERROR_INVALID_DATATYPE(K, DT, Fun(K, DT));

invalid_datatype_error(K, DT, _) ->
    ?ERROR_INVALID_DATATYPE(K, DT, ?INVALID_DATATYPE_MSG(K, DT)).


%% @private
invalid_value_error(K, V, #{error_formatters := #{invalid_value := Fun}}) ->
    ?ERROR_INVALID_VALUE(K, V, Fun(K, V));

invalid_value_error(K, V, _) ->
    ?ERROR_INVALID_VALUE(K, V, ?INVALID_VALUE_MSG(K, V)).

%% From maps.erl
error_type(M) when is_map(M) -> badarg;
error_type(V) -> {badmap, V}.


%% @private
validates_any(K, V, Validators, Opts) ->
    Fun = fun
        (Spec, error) when is_map(Spec) ->
            try
                Val = validate(V, Spec, Opts),
                %% We stop iterating as we found a valid value
                throw({true, Val})
            catch
                error:_ ->
                    %% We carry on iterating
                    error
            end;
        (Fun, error) when is_function(Fun, 1) ->
            case Fun(V) of
                {ok, Val} ->
                    throw({true, Val});
                true ->
                    throw({true, V});
                _ ->
                    error
            end;
        ({list, _} = Validator, error) ->
            case do_maybe_eval(K, V, #{validator => Validator}, Opts) of
                {ok, Val} ->
                    throw({true, Val});
                _ ->
                    error
        end
    end,

    try
        error = lists:foldl(Fun, error, Validators),
        throw(invalid_datatype_error(K, V, Opts))
    catch
        throw:{true, Val} ->
            {ok, Val}
    end.