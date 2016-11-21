-module(rbac).


-define(KEY, leap_rbac_ctxt).


-type ctxt()  :: #{
    account_id => binary(),
    user_id => binary(),
    roles => list()
}.


-export_type([ctxt/0]).

-export([init_ctxt/3]).
-export([get_ctxt/0]).
-export([has_role/1]).
-export([has_any_role/1]).
-export([terminate/0]).
-export([add_audit_info/2]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% Initialises an authorization context storing it in the process dictionary.
%% @end
%% -----------------------------------------------------------------------------
-spec init_ctxt(
    ClientId :: binary(), UserId :: binary(), Roles :: list()) -> ok.

init_ctxt(AccId, UserId, Roles)
when is_binary(AccId), is_binary(UserId), is_list(Roles) ->
    _ = put(?KEY, #{account_id => AccId, user_id => UserId, roles => Roles}),
    ok.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec terminate() -> ok.

terminate() ->
    _ = erase(?KEY),
    ok.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_ctxt() -> map() | undefined.

get_ctxt() ->
    get(?KEY).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec has_role(term()) -> boolean().

has_role(Role) ->
    case get(?KEY) of
        #{roles := Roles} -> lists:member(Role, Roles);
        _ -> false
    end.

has_any_role(L) when is_list(L) ->
    case get(?KEY) of
        #{roles := All} ->
            sets:size(
                sets:intersection(sets:from_list(L), sets:from_list(All))) > 0;
        _ ->
            false
    end.


%% @doc add autit fields based on the operation to the map
add_audit_info(create, Map) when is_map(Map) ->
    Timestamp = calendar:universal_time(),
     case get(?KEY) of
        #{user_id := UserId} ->
            Map#{created_at => Timestamp, created_by => UserId};
        _ ->
            Map#{created_at => Timestamp}
    end;

add_audit_info(update, Map) when is_map(Map) ->
    Timestamp = calendar:universal_time(),
    case get(?KEY) of
        #{user_id := UserId} ->
            Map#{updated_at => Timestamp, updated_by => UserId};
        _ ->
            Map#{updated_at => Timestamp}
    end.
