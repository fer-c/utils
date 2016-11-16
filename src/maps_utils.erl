-module(maps_utils).



-define(ERROR_MISSING_REQUIRED_VALUE(Param),{
    invalid_data, 
    iolist_to_binary(["A value for '", term_to_iolist(K), "' is required."])
}).

-define(ERROR_INVALID_DATATYPE(K, DT), {
    invalid_data, 
    iolist_to_binary([
        "The value for '", term_to_iolist(K), 
        "' is not a of type '", term_to_iolist(DT), "'"
    ])
}).

-define(ERROR_INVALID_VALUE(K, V), {
    invalid_data, 
    iolist_to_binary(["The value '", term_to_iolist(V), 
    "' for '", term_to_iolist(K), "' is not valid'"])
}).


-type base_datatype()          ::   boolean
                                    | integer
                                    | float
                                    | number
                                    | atom
                                    | string
                                    | binary
                                    | bitstring
                                    | tuple
                                    | map
                                    | list
                                    | string 
                                    | {record, atom()}                  | {in, list()}
                                    | {not_in, list()}
                                    | pid | reference | port.

-type datatype()                ::  base_datatype()
                                    | {list, base_datatype()}.

-type validator()               ::  fun((term()) -> {ok, any()} | error).
-type entry_spec()              ::  #{
                                        key => any(),
                                        required => boolean(),
                                        default => term(),
                                        datatype => datatype(),
                                        validator => validator()
                                    }.
-type map_spec()                ::  #{term() => entry_spec()}.


-export_type([datatype/0]).
-export_type([validator/0]).
-export_type([entry_spec/0]).
-export_type([map_spec/0]).


-export([validate/2]).
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
%% Throws
%% * {missing_required_key, Key :: term()}
%% * {invalid_datatype, Key :: term(), Value :: term()}
%% * {invalid, Key :: term(), Value :: term()}
%% @end
%% -----------------------------------------------------------------------------
-spec validate(Map :: map(), Spec :: map_spec()) -> ValidMap :: map().

validate(Map0, Spec) when is_map(Spec) ->
   {Map0, Map1} =  maps:fold(
       fun validate_fold_fun/3, {Map0, #{}}, Spec),
   Map1.




%% =============================================================================
%% PRIVATE : MAP VALIDATION
%% =============================================================================



%% @private
validate_fold_fun(K, Spec, {In, Out}) when is_map(Spec) ->
    NewKey = maps:get(key, Spec, K),
    case maps:find(K, In) of
        {ok, Val} ->
            {In, maps:put(NewKey, validate(K, Val, Spec), Out)};
        error ->
            IsReq = is_required(Spec),
            Default = default(Spec),
            {In, maybe_add_key(IsReq, NewKey, Default, Out)}
    end;

validate_fold_fun(K, Spec, _) ->
    error(badarg, [K, Spec]).


%% @private
validate(K, V, Spec) ->
    case is_valid_datatype(V, Spec) of
        true -> 
            maybe_eval(K, V, Spec);
        false ->
            error(?ERROR_INVALID_DATATYPE(K, maps:get(datatype, Spec)))
    end.


%% @private
maybe_add_key(true, K, no_default, _) ->
    error(?ERROR_MISSING_REQUIRED_VALUE(K));

maybe_add_key(true, K, Default, Map) ->
    maps:put(K, Default, Map);

maybe_add_key(false, _, _, Map) ->
    Map.


%% @private
is_required(#{required := V}) when is_boolean(V) -> V;
is_required(_) -> false.


%% @private
default(#{default := V}) -> V;
default(_) -> no_default.


%% @private
maybe_eval(K, V, #{validator := {list, Fun}}) when is_list(V), is_function(Fun, 1) ->
    Inner = fun(E, Acc) ->
        case Fun(E) of
            {ok, Val} -> [Val|Acc];
            true -> [E|Acc];
            false -> error(?ERROR_INVALID_VALUE(K, V));
            error -> error(?ERROR_INVALID_VALUE(K, V))
        end
    end,
    lists:reverse(lists:foldl(Inner, [], V));  

maybe_eval(K, V, #{validator := {list, Spec}}) when is_list(V), is_map(Spec) ->
    Inner =  fun
        (E, Acc) when is_map(E) ->
            [validate(E, Spec) | Acc];
        (_, _) ->
            error(?ERROR_INVALID_VALUE(K,V))
    end,
    lists:reverse(lists:foldl(Inner, [], V));   

maybe_eval(K, V, #{validator := Fun}) when is_function(Fun, 1) ->
    case Fun(V) of
        {ok, Val} -> Val;
        true -> V;
        false -> error(?ERROR_INVALID_VALUE(K, V));
        error -> error(?ERROR_INVALID_VALUE(K, V))
    end;


maybe_eval(_, V, #{validator := Spec}) when is_map(V), is_map(Spec) ->
    validate(V, Spec);



maybe_eval(_, V, _) ->
    V.


%% @private
is_valid_datatype(V, #{datatype := boolean}) when is_boolean(V) ->
    true;

is_valid_datatype(V, #{datatype := atom}) when is_atom(V) ->
    true;

is_valid_datatype(V, #{datatype := integer}) when is_integer(V) ->
    true;

is_valid_datatype(V, #{datatype := float}) when is_float(V) ->
    true;

is_valid_datatype(V, #{datatype := number}) when is_integer(V) orelse is_float(V) ->
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


is_valid_datatype(_, #{datatype := _}) ->
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
