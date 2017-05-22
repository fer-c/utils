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

-type base_datatype()          ::   boolean
                                    | integer
                                    | pos_integer
                                    | neg_integer
                                    | timeout
                                    | float
                                    | number
                                    | atom
                                    | binary
                                    | bitstring
                                    | tuple
                                    | map
                                    | list
                                    | string 
                                    | {record, atom()}                  
                                    | {in, list()}
                                    | {one_of, [base_datatype()]}
                                    | function
                                    | {function, N :: non_neg_integer()}
                                    | pid | reference | port.

-type datatype()                ::  base_datatype()
                                    | {list, base_datatype()}.

-type validator()               ::  fun((term()) -> 
                                        {ok, any()} | boolean() | error
                                    ) 
                                    | entry_spec(). %% nested maps

-type entry_spec()              ::  #{
                                        key => any(), %% a new name for the Key
                                        required => boolean(),
                                        allow_undefined => boolean() | remove,
                                        allow_null => boolean() | remove,
                                        default => term(), %% only if required
                                        datatype => datatype() | [datatype()], 
                                        validator => validator()
                                    }.
-type map_spec()                ::  #{term() => entry_spec()}.

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
    error_code => atom(),
    error_formatters => formatters()
}.



-export_type([datatype/0]).
-export_type([validator/0]).
-export_type([entry_spec/0]).
-export_type([map_spec/0]).


-export([validate/2]).
-export([validate/3]).
-export([append/3]).
-export([append_list/3]).
-export([collect/2]).
-export([split/2]).


%% =============================================================================
%% API
%% =============================================================================

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
%% This function appends a list of values Values to the current list of values
%% associated with Key. An exception is generated if the initial value
%% associated with Key is not a list of values.
%% @end
%% -----------------------------------------------------------------------------
append_list(Key, Values, Map) when is_list(Values) ->
    case maps:get(Key, Map, []) of
        OldValues when is_list(OldValues) ->
            maps:update(Key, lists:append(OldValues, Values), Map);
        _ ->
            error(badarg)
    end.


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
%% Calls validate/3 with the defaults options.
%% @end
%% -----------------------------------------------------------------------------
-spec validate(Map :: map(), Spec :: map_spec()) -> ValidMap :: map().

validate(Map0, Spec) when is_map(Spec) ->
    validate(Map0, Spec, #{}).


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
-spec validate(Map :: map(), Spec :: map_spec(), validation_opts()) -> 
    NewMap :: map().

validate(Map0, Spec, Opts) when is_map(Spec), is_map(Opts) ->
    case do_validate(Map0, Spec, Opts) of
        {error, Reason} ->
            error(Reason);
        Val -> 
            Val
    end.


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




%% =============================================================================
%% PRIVATE : MAP VALIDATION
%% =============================================================================



%% @private
validate_fold_fun(K, KSpec, {In, Out, Err, Opts}) when is_map(KSpec) ->
    
    case validate_key(K, In, KSpec, Opts) of
        {ok, Val} ->
            %% Maybe we rename the key
            NewKey = maps:get(key, KSpec, K),
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

    case maps:find(K, In) of
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
maybe_get_default(_, #{required := true, default := V}, _) -> 
    {ok, V};

maybe_get_default(K, #{required := true}, Opts) -> 
    {error, missing_required_value_error(K, Opts)};

maybe_get_default(_, _, _) -> 
    not_found.


%% @private
maybe_eval(K, V, KSpec, Opts) ->
    try
        do_maybe_eval(K, V, KSpec, Opts)
    catch
        throw:Reason ->
            {error, Reason}
    end.


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

do_maybe_eval(K, V, #{validator := {list, Spec}}, Opts) 
when is_list(V), is_map(Spec) ->

    Inner = fun
        (E, Acc) when is_map(E) ->
            case validate(E, Spec) of
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

do_maybe_eval(K, V, #{validator := Fun}, Opts) when is_function(Fun, 1) ->
    case Fun(V) of
        {ok, _} = OK -> 
            OK;
        {merge, MergeFun} = OK when is_function(MergeFun, 1) -> 
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
    case validate(V, Spec, Opts) of
        {error, _} = Error ->
            Error;
        Val ->
            {ok, Val}
    end;

do_maybe_eval(K, _, #{validator := Spec}, Opts) when is_map(Spec) ->
    {error, invalid_datatype_error(K, map, Opts)};

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

is_valid_datatype(V, #{datatype := timeout}) 
when V =:= infinity orelse (is_integer(V) andalso V > 0) ->
    true;

is_valid_datatype(V, #{datatype := float}) when is_float(V) ->
    true;

is_valid_datatype(V, #{datatype := number}) when is_integer(V) orelse is_float(V) ->
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
    A == sets:intersection([A, B]);

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

term_to_iolist(Term) when is_list(Term) ->
    Term;
    
term_to_iolist(Term) ->
    io_lib:format("~p", [Term]).

%% @private
is_datatype(atom) -> true;
is_datatype(binary) -> true;
is_datatype(bitstring) -> true;
is_datatype(boolean) -> true;
is_datatype(float) -> true;
is_datatype(integer) -> true;
is_datatype(list) -> true;
is_datatype(map) -> true;
is_datatype(number) -> true;
is_datatype(pid) -> true;
is_datatype(port) -> true;
is_datatype(reference) -> true;
is_datatype(string) -> true;
is_datatype(tuple) -> true;
is_datatype({in, L}) when is_list(L) -> true;
is_datatype({not_in, L}) when is_list(L) -> true;
is_datatype({record, Tag}) when is_atom(Tag) -> true;
is_datatype({list, T}) -> is_datatype(T);                 
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
