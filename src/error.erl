-module(error).

-export([new/2]).
-export([new/3]).
-export([new/4]).
-export([new/5]).


% [
%     [
%         {
%             "title": "Invalid username",
%             "type": "invalid_input",
%             "description": "The value 'foo' for 'username' did not pass the validator.",
%             "source":"com.thing.account",
%             "traceid": 1234567890,
%             "meta":{
%                 "resource":"com.thing.account",
%                 "property":"username",
%                 "value":"foo",
%                 "about": "https://www.thing.com/system/account#username"
%             }
%         }
%     ]
% ],


%% =============================================================================
%% API
%% =============================================================================


new({missing_required_value = Type, Key0}, Opts) ->
    Key = term_to_iolist(Key0),
    Msg = <<"Missing a required value.">>,
    Desc = iolist_to_binary([
        <<"A value for property '">>, Key,
        <<"' is required.">>]
    ),
    Details = #{
        resource => maps:get(resource, Opts, undefined),
        property => iolist_to_binary(Key)
    },
    new(Type, Msg, Desc, Details);

new({invalid_datatype = Type, Key0, Value0, Datatype0}, Opts) ->
    Key = term_to_iolist(Key0),
    Value = term_to_iolist(Value0, safe),
    Datatype = term_to_iolist(Datatype0),
    Msg = <<"Invalid datatype.">>,
    Desc = iolist_to_binary([
        <<"The value for property '">>, Key,
        <<"' is not a of type '">>, Datatype,
        <<"'.">>
    ]),
    Details = #{
        resource => maps:get(resource, Opts, undefined),
        property => iolist_to_binary(Key),
        value => iolist_to_binary(Value)
    },
    new(Type, Msg, Desc, Details);

new({invalid_value = Type, Key0, Value0}, Opts) ->
    Value = term_to_iolist(Value0, safe),
    Key = term_to_iolist(Key0),
    Msg = <<"Invalid value.">>,
    Desc = iolist_to_binary([
        <<"The value '">>, Value,
        <<"' for property '">>, Key,
        <<"' did not pass one or more validations.">>]
    ),
    Details = #{
        resource => maps:get(resource, Opts, undefined),
        property => iolist_to_binary(Key),
        value => iolist_to_binary(Value)
    },
    new(Type, Msg, Desc, Details).



new(Type, Title, Description) ->
    #{
        type => Type,
        title => Title,
        description => Description
    }.


new(Type, Title, Description, Details) ->
    new(Type, Title, Description, Details, undefined).


new(Type, Title, Description, Details, Meta) ->
    #{
        type => Type,
        title => Title,
        description => Description,
        details => Details,
        meta => Meta
    }.



term_to_iolist(Term) ->
    term_to_iolist(Term, normal).


term_to_iolist(Term, safe) when is_tuple(Term) ->
    <<"<<tuple>>">>;

term_to_iolist(Term, safe) when is_map(Term) ->
    <<"<<map>>">>;

term_to_iolist(Term, _) when is_binary(Term) ->
    Term;

term_to_iolist(Term, _) ->
    io_lib:format("~p", [Term]).