%% =============================================================================
%% maps_utils_tests - test suite.
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

   
   merge_2_test() ->
    Result = maps_utils:validate(
        #{foo => 1, bar => 2}, 
        #{
            foo => #{
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
                required => true}
        }
    ),
    ?assertEqual(#{foo => #{bar => 1}, bar => 2}, Result).


alias_1_test() ->
    ?assertEqual(
        #{foo => 1},
        maps_utils:validate(
            #{foo => 1}, 
            #{foo => #{
                alias => <<"foo">>
            }}
        )
    ).

alias_2_test() ->
    ?assertEqual(
        #{foo => 1},
        maps_utils:validate(
            #{<<"foo">> => 1}, 
            #{foo => #{
                alias => <<"foo">>
            }}
        )
    ).

or_1_test() ->
    Spec1 = #{
        foo => #{required => true}
    },
    Spec2 = #{
        bar => #{required => true}
    },
    Spec = #{
        field => #{
            datatype => map, 
            required => true, 
            validator => [Spec1, Spec2]
        }
    },
    ?assertEqual(
        #{field => #{bar => 1}},
        maps_utils:validate(
            #{field => #{bar => 1}},
            Spec
        )
    ).

or_2_test() ->
    Spec1 = #{
        foo => #{required => true}
    },
    Spec2 = #{
        bar => #{required => true}
    },
    Spec = #{
        field => #{
            datatype => map, 
            required => true, 
            validator => [Spec1, Spec2]
        }
    },
    ?assertEqual(
        #{field => #{foo => 1}},
        maps_utils:validate(
            #{field => #{foo => 1}},
            Spec
        )
    ).

in_test() ->
    Spec = #{
        field => #{
            datatype => {in, [1,2,3]}
        }
    },
    ?assertEqual(
        #{field => 1},
        maps_utils:validate(
            #{field => 1},
            Spec
        )
    ).

all_in_test() ->
    Spec = #{
        field => #{
            datatype => {list, {in, [1,2,3]}}
        }
    },
    ?assertError(
        #{code := invalid_datatype},
        maps_utils:validate(
            #{field => [4]},
            Spec
        )
    ).

all_in_2_test() ->
    Spec = #{
        field => #{
            datatype => {list, {in, [1,2,3]}}
        }
    },
    ?assertError(
        #{code := invalid_datatype},
        maps_utils:validate(
            #{field => foo},
            Spec
        )
    ).

default_fun_test() ->
    Spec = #{
        field => #{
            datatype => integer, 
            required => true, 
            default => fun() -> 1 end
        }
    },
    ?assertEqual(
        #{field => 1},
        maps_utils:validate(
            #{},
            Spec
        )
    ).

update_validator_1_test() ->
    ?assertError(
        #{code := invalid_value},
        maps_utils:validate_update(
            #{x => 1}, #{x => 2},
            #{x => #{update_validator => fun (_, _) -> false end}}
        )
    ).

update_transform_1_test() ->
    ?assertError(
        {invalid_validator_return_value, x},
        maps_utils:validate_update(
            #{x => 1}, #{x => 1},
            #{x => #{update_validator => fun(X, _) -> X*2 end}}
        )
    ).

update_validator_2_test() ->
    ?assertEqual(
        ok,
        maps_utils:validate_update(
            #{x => 1}, #{x => 2},
            #{x => #{update_validator => fun(_, _) -> true end}}
        )
    ).

update_validator_3_test() ->
    ?assertError(
        #{code := invalid_value},
        maps_utils:validate_update(
            #{x => 1, y => 2}, #{x => 2, y =>3},
            #{x => #{update_validator => fun (_, _) -> false end},
              y => #{update_validator => fun (_, _) -> false end}}
        )
    ).

update_validator_multiple_test() ->
    ?assertError(
        #{code := invalid_data},
        maps_utils:validate_update(
            #{x => 1, y => 2}, #{x => 2, y =>3},
            #{x => #{update_validator => fun (_, _) -> false end},
              y => #{update_validator => fun (_, _) -> false end}},
            #{atomic => false}
        )
    ).

update_validator_list_test() ->
    SubSpec = #{y => #{update_validator => fun (_, _) -> false end}},
    ?assertEqual(
        ok,
        maps_utils:validate_update(
            #{x => [#{y => 1}]}, #{x => [#{y => 2}]},
            #{x => #{validator => {list, SubSpec}}}
        )
    ).

update_validator_nested_test() ->
    SubSpec = #{y => #{update_validator => fun (_, _) -> true end}},
    ?assertEqual(
        ok,
        maps_utils:validate_update(
            #{x => #{y => 1}}, #{x => #{y => 2}},
            #{x => #{update_validator => SubSpec}}
        )
    ).

update_validator_nested_2_test() ->
    SubSpec = #{y => #{update_validator => fun (_, _) -> true end}},
    SubSpec2 = #{y => #{update_validator => fun (_, _) -> false end}},
    ?assertError(
        #{code := invalid_value},
        maps_utils:validate_update(
            #{x => #{y => 1}}, #{x => #{y => 2}},
            #{x => #{update_validator => [SubSpec, SubSpec2]}}
        )
    ).

update_validator_nochange_test() ->
    ?assertEqual(
        ok,
        maps_utils:validate_update(
            #{x => 1}, #{},
            #{x => #{}}
        )
    ).

update_validator_nospec_test() ->
    ?assertEqual(
        ok,
        maps_utils:validate_update(
            #{}, #{x => 2},
            #{x => #{}}
        )
    ).
