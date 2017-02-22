-module(maps_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).



require_fails_1_test() ->
    ?assertError(
        #{code := missing_required_value},
        maps_utils:validate(#{}, #{x => #{required => true}})
    ).

require_fails_2_test() ->
    ?assertError(
        #{code := missing_required_value},
        maps_utils:validate(#{y => 3}, #{x => #{required => true}})
    ).

require_1_test() ->
    Map = #{x => 3},
    ?assertEqual(
        Map,
        maps_utils:validate(Map, #{x => #{required => true}})
    ).

require_2_test() ->
    Map = #{x => 3},
    ?assertEqual(
        Map,
        maps_utils:validate(Map, #{
            x => #{required => true},
            y => #{required => false}
        })
    ).

require_skips_test() ->
    Map = #{x => 3, y => 1},
    ?assertEqual(
        #{x => 3},
        maps_utils:validate(Map, #{ x => #{required => true}})
    ).

datatype_1_test() ->
    ?assertError(
        #{code := invalid_datatype},
        maps_utils:validate(
            #{x => 1}, 
            #{x => #{datatype => boolean}}
        )
    ).

datatype_format_1_test() ->
    Msg = <<"Foo">>,
    ?assertError(
        #{code := invalid_datatype, message := Msg},
        maps_utils:validate(
            #{x => 1}, 
            #{x => #{datatype => boolean}},
             #{error_formatters => #{
                invalid_datatype => fun(_, _) -> Msg end
            }}
        )
    ).
   

validator_1_test() ->
    ?assertError(
        #{code := invalid_value},
        maps_utils:validate(
            #{x => true}, 
            #{x => #{validator => fun erlang:is_integer/1}}
        )
    ).

transform_1_test() ->
    ?assertError(
        {invalid_validator_return_value, x},
        maps_utils:validate(
            #{x => 1}, 
            #{x => #{validator => fun(X) -> X*2 end}}
        )
    ).

transform_2_test() ->
    ?assertEqual(
        #{x => 2},
        maps_utils:validate(
            #{x => 1}, 
            #{x => #{validator => fun(X) -> {ok, X*2} end}}
        )
    ).

transform_3_test() ->
    ?assertEqual(
        #{x => 1},
        maps_utils:validate(
            #{x => 1}, 
            #{x => #{validator => fun(_X) -> true end}}
        )
    ).

transform_4_test() ->
    ?assertError(
        {invalid_datatype, foo},
        maps_utils:validate(
            #{x => 1}, 
            #{x => #{datatype => foo}}
        )
    ).


formatters() ->
    ?assertError(
        {invalid_datatype, foo},
        maps_utils:validate(
            #{x => 1}, 
            #{x => #{datatype => foo}}
        )
    ).

null_1_test() ->
    ?assertEqual(
        #{foo => null},
        maps_utils:validate(
            #{foo => null}, 
            #{foo => #{
                allow_null => true,
                datatype => binary, 
                required => false, 
                default => <<"foo">>,
                validator => fun(_) -> {ok, <<"bar">>} end
                }
            }
        )
    ).

null_2_test() ->
    ?assertEqual(
        #{foo => <<"bar">>},
        maps_utils:validate(
            #{foo => null}, 
            #{foo => #{
                allow_null => false,
                datatype => binary, 
                required => false, 
                default => <<"foo">>,
                validator => fun(_) -> {ok, <<"bar">>} end
                }
            }
        )
    ).

null_3_test() ->
    ?assertError(
        #{code := invalid_value},
        maps_utils:validate(
            #{foo => null}, 
            #{foo => #{
                allow_null => false,
                datatype => binary, 
                required => false, 
                default => <<"foo">>
                }
            }
        )
    ).

null_4_test() ->
    ?assertEqual(
        #{},
        maps_utils:validate(
            #{foo => null}, 
            #{foo => #{
                allow_null => remove,
                datatype => binary, 
                required => false, 
                default => <<"foo">>,
                validator => fun(_) -> {ok, <<"bar">>} end
                }
            }
        )
    ).


undefined_1_test() ->
    ?assertEqual(
        #{foo => undefined},
        maps_utils:validate(
            #{foo => undefined}, 
            #{foo => #{
                allow_undefined => true,
                datatype => binary, 
                required => false, 
                default => <<"foo">>,
                validator => fun(_) -> {ok, <<"bar">>} end
                }
            }
        )
    ).

undefined_2_test() ->
    ?assertEqual(
        #{foo => <<"bar">>},
        maps_utils:validate(
            #{foo => undefined}, 
            #{foo => #{
                allow_undefined => false,
                datatype => binary, 
                required => false, 
                default => <<"foo">>,
                validator => fun(_) -> {ok, <<"bar">>} end
                }
            }
        )
    ).

undefined_3_test() ->
    ?assertError(
        #{code := invalid_value},
        maps_utils:validate(
            #{foo => undefined}, 
            #{foo => #{
                allow_undefined => false,
                datatype => binary, 
                required => false, 
                default => <<"foo">>
                }
            }
        )
    ).

undefined_4_test() ->
    ?assertEqual(
        #{},
        maps_utils:validate(
            #{foo => undefined}, 
            #{foo => #{
                allow_undefined => remove,
                datatype => binary, 
                required => false, 
                default => <<"foo">>,
                validator => fun(_) -> {ok, <<"bar">>} end
                }
            }
        )
    ).

merge_test() ->
    Result = maps_utils:validate(
        #{foo => 1, bar => 2}, 
        #{
            foo => #{
                key => foobar,
                required => true, 
                validator => 
                    fun(V) -> 
                        {merge, fun
                            (undefined) ->
                                #{bar => V};
                            (X) ->
                                maps:merge(#{foo => V}, X)
                            end
                        } 
                    end
            },
            bar => #{
                key => foobar,
                required => true, 
                validator => 
                    fun(V) -> 
                        {merge, fun
                            (undefined) ->
                                #{bar => V};
                            (X) ->
                                maps:merge(#{bar => V}, X)
                            end
                        } 
                    end
            }
        }
    ),
    ?assertEqual(#{foobar => #{foo => 1, bar => 2}}, Result).

   