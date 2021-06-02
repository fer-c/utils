%% =============================================================================
%% maps_utils_tests - test suite.
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

keep_unknown_fields_1_test() ->

    ?assertEqual(
        #{x => 2, y => <<"non_validated">>},
        maps_utils:validate(
            #{x => 1, y => <<"non_validated">>},
            #{x => #{validator => fun(X) -> {ok, X*2} end}},
            #{keep_unknown => true}
        )
    ).

keep_unknown_alias_fields_1_test() ->
    ?assertEqual(
        #{x => 2, y => <<"non_validated">>},
        maps_utils:validate(
            #{<<"x">> => 1, y => <<"non_validated">>},
            #{
                x => #{
                    alias => <<"x">>,
                    validator => fun(X) -> {ok, X*2} end
                }
            },
            #{keep_unknown => true}
        )
    ).

keep_unknown_alias_fields_2_test() ->
    Pattern = element(2, re:compile("_[a-z0-9_]{3,}")),
    Validator = fun
        (X) when is_binary(X) ->
            re:run(X, Pattern) /= nomatch;
        (X) when is_atom(X) ->
            re:run(atom_to_binary(X, utf8), Pattern) /= nomatch
    end,

    ?assertEqual(
        #{x => 2},
        maps_utils:validate(
            #{<<"x">> => 1, y => <<"non_validated">>},
            #{
                x => #{
                    alias => <<"x">>,
                    validator => fun(X) -> {ok, X*2} end
                }
            },
            #{keep_unknown => true, unknown_label_validator => Validator}
        )
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
    ?assertError(
        {x, 1, boolean},
        maps_utils:validate(
            #{x => 1},
            #{x => #{datatype => boolean}},
            #{error_formatter => fun
                ({invalid_datatype, K, V, DT}) ->
                    {K, V, DT}
                end
            }
        )
    ).


validator_1_test() ->
    ?assertError(
        {x, true},
        maps_utils:validate(
            #{x => true},
            #{x => #{validator => fun erlang:is_integer/1}},
            #{error_formatter => fun
                ({invalid_value, K, V}) ->
                    {K, V}
                end
            }
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
            #{<<"foo">> => 1},
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

get_path_empty_test() ->
    ?assertError(
        {badkey, []},
        maps_utils:get_path([], #{}, undefined)
    ).

get_path_nested_test() ->
    Map = #{ field => #{ field => pepe}},
    ?assertEqual(
        pepe,
        maps_utils:get_path([field, field], Map, undefined)
    ).

get_path_default_test() ->
    Map = #{ },
    ?assertEqual(
        undefined,
        maps_utils:get_path([field, field], Map, undefined)
    ).

put_path_empty_test() ->
    ?assertError(
        {badkey, []},
        maps_utils:put_path([], undefined, #{})
    ).

put_path_invalid_test() ->
    ?assertError(
        {badmap, []},
        maps_utils:put_path([[field], field], undefined, [])
    ).

put_path_nested_test() ->
    Map = #{ field => #{ field => pepe}},
    ?assertEqual(
        Map,
        maps_utils:put_path([field, field], pepe, #{})
    ).

remove_path_empty_test() ->
    ?assertError(
        {badkey, []},
        maps_utils:remove_path([], #{})
    ).

remove_path_nested_test() ->
    Map = #{ field => #{ field => pepe}},
    ?assertEqual(
        #{field => #{}},
        maps_utils:remove_path([field, field], Map)
    ).

remove_path_invalid_test() ->
    ?assertError(
        {badmap, []},
        maps_utils:remove_path([[field], field], [])
    ).

with_paths_empty_test() ->
    ?assertEqual(
        #{},
        maps_utils:with_paths([], #{})
    ).

with_paths_2_test() ->
    Map = #{ field => #{ field => pepe}, field2 => hola, field3 => nada},
    ?assertEqual(
        maps:remove(field3, Map),
        maps_utils:with_paths([[field, field], [field2]], Map)
    ).

with_paths_3_test() ->
    Map = #{},
    ?assertEqual(
        #{},
        maps_utils:with_paths([[field, field], [field2]], Map)
    ).

with_paths_4_test() ->
    Map = #{},
    ?assertEqual(
        #{},
        maps_utils:with_paths([[a, b]], Map)
    ).

with_paths_5_test() ->
    Map = #{a => 1},
    ?assertEqual(
        Map,
        maps_utils:without_paths([[a, b]], Map)
    ).

with_paths_invalid_type_test() ->
    ?assertError(
        badarg,
        maps_utils:with_paths(#{}, #{})
    ).

without_paths_empty_test() ->
    Map = #{ field => #{ field => pepe}, field2 => hola, field3 => nada},
    ?assertEqual(
        Map,
        maps_utils:without_paths([], Map)
    ).

without_paths_2_test() ->
    Map = #{ field => #{ field => pepe}, field2 => hola, field3 => nada},
    ?assertEqual(
        #{field => #{}, field3 => nada},
        maps_utils:without_paths([[field, field], [field2]], Map)
    ).

without_paths_3_test() ->
    Map = #{},
    ?assertEqual(
        Map,
        maps_utils:without_paths([[field, field], [field2]], Map)
    ).

without_paths_4_test() ->
    Map = #{a => 1},
    ?assertEqual(
        Map,
        maps_utils:without_paths([[a, b]], Map)
    ).

without_paths_5_test() ->
    Map = #{},
    ?assertEqual(
        Map,
        maps_utils:without_paths([[a, b]], Map)
    ).

without_paths_invalid_type_test() ->
    ?assertError(
        badarg,
        maps_utils:without_paths(#{}, #{})
    ).

append_path_empty_test() ->
    ?assertError(
        {badkey, []},
        maps_utils:append_path([], hola, #{})
    ).

append_path_test() ->
    Map = #{ field => #{ field => []}},
    ?assertEqual(
        #{ field => #{ field => [pepe]}},
        maps_utils:append_path([field, field], pepe, Map)
    ).

append_path_not_list_test() ->
    Map = #{ field => #{ field => pepe}},
    ?assertError(
        {badlist, pepe},
        maps_utils:append_path([field, field], pepe, Map)
    ).

append_list_not_list_test() ->
    Map = #{ field => [] },
    ?assertError(
        {badarg, _},
        maps_utils:append_list(field, pepe, Map)
    ).

append_list_path_empty_list_test() ->
    ?assertError(
        {badkey, []},
        maps_utils:append_list_path([], undefined, #{})
    ).

append_list_path_test() ->
    Map = #{ field => #{field => []}},
    ?assertEqual(
        #{ field => #{ field => [pepe, juan]}},
        maps_utils:append_list_path([field, field], [pepe, juan], Map)
    ).

append_list_path_invalid_test() ->
    ?assertError(
        {badmap, []},
        maps_utils:append_list_path([[field], field], undefined, [])
    ).

collect_test() ->
    Map = #{ field => #{field => pepe}, field2 => pepito},
    ?assertEqual(
        [#{field => pepe}, pepito],
        maps_utils:collect([field, asdf, field2], Map)
    ).

split_test() ->
    Map = #{ field => #{field => pepe}, field2 => pepito},
    ?assertEqual(
        { #{field2 => pepito}, #{ field => #{field => pepe}} },
        maps_utils:split([field2], Map)
    ).

validate_atomic_test() ->
    ?assertError(
        #{ code := invalid_datatype},
        maps_utils:validate(
            #{x => 1, y => 1},
            #{x => #{datatype => binary}, y => #{datatype => binary}},
            #{atomic => false}
        )
    ).

validate_two_errors_test() ->
    ?assertError(
        #{ code := invalid_data, errors := [_X, _Y] },
        maps_utils:validate(
            #{x => 1, y => 1},
            #{x => #{datatype => binary}, y => #{datatype => binary}}
        )
    ).

rename_key_test() ->
    ?assertEqual(
        #{x => 1},
        maps_utils:validate(
            #{<<"x">> => 1},
            #{<<"x">> => #{key => x}}
        )
    ).

rename_key_labels_bin_to_atom_test() ->
    ?assertEqual(
        #{x => 1},
        maps_utils:validate(
            #{<<"x">> => 1},
            #{<<"x">> => #{}},
            #{labels => atom}
        )
    ).

validate_fun_list_test() ->
    ?assertEqual(
        #{x => [1]},
        maps_utils:validate(
            #{x => [1]},
            #{x => #{validator => {list, fun(X) -> X>0 end}}}
        )
    ).

validate_fun_list_error_test() ->
    ?assertError(
        {x, 1},
        maps_utils:validate(
            #{x => 1},
            #{x => #{validator => {list, fun(X) -> X>0 end}}},
            #{error_formatter => fun
                ({invalid_value, K, V}) ->
                    {K, V}
                end
            }
        )
    ).

validate_list_spec_error_test() ->
    SubSpec = #{y => #{datatype => binary}},
    ?assertError(
        #{code := invalid_datatype},
        maps_utils:validate(
            #{x => [#{y => 1}]},
            #{x => #{validator => {list, SubSpec}}}
        )
    ).

validate_nested_spec_error_test() ->
    SubSpec = #{y => #{datatype => binary}},
    ?assertError(
        #{code := invalid_datatype},
        maps_utils:validate(
            #{x => #{y => 1}},
            #{x => #{validator => SubSpec}}
        )
    ).

validate_mutiple_spec_1_test() ->
    SubSpec = #{y => #{datatype => binary}},
    SubSpec2 = #{y => #{datatype => integer}},
    ?assertError(
        {x, #{y := "asdasd"}},
        maps_utils:validate(
            #{x => #{y => "asdasd"}},
            #{x => #{validator => [SubSpec, SubSpec2]}},
            #{error_formatter => fun
                ({invalid_value, K, V}) ->
                    {K, V}
                end
            }
        )
    ).

validate_mutiple_spec_2_test() ->
    SubSpec = #{y => #{datatype => binary}},
    SubSpec2 = #{y => #{datatype => integer}},
    ?assertEqual(
        #{x => #{y => <<"asdasd">>}},
        maps_utils:validate(
            #{x => #{y => <<"asdasd">>}},
            #{x => #{validator => [SubSpec, SubSpec2]}}
        )
    ).

validate_mutiple_spec_3_test() ->
    SubSpec = #{y => #{datatype => binary}},
    SubSpec2 = #{y => #{datatype => integer}},
    ?assertEqual(
        #{x => #{y => 1}},
        maps_utils:validate(
            #{x => #{y => 1}},
            #{x => #{validator => [SubSpec, SubSpec2]}}
        )
    ).

validate_map_multiple_spec_1_test() ->
    Spec1 = #{
        foo => #{required => true, datatype => binary}
    },
    Spec2 = #{
        bar => #{required => true, datatype => integer}
    },
    Map = #{
        x => #{
            a => #{foo => <<"any">>},
            b => #{bar => 1}
        }
    },
    Spec = #{
        x => #{
            validator => {map, {atom, [Spec1, Spec2]}}}
    },

    ?assertEqual(Map, maps_utils:validate(Map, Spec)).


validate_map_multiple_spec_2_test() ->
    Spec1 = #{
        foo => #{datatype => binary}
    },
    Spec2 = #{
        bar => #{datatype => integer}
    },
    Map = #{
        x => #{
            %% key should be atom so it will fail
            <<"a">> => #{foo => <<"any">>},
            b => #{bar => 1}
        }
    },
    Spec = #{
        x => #{
            validator => {map, {atom, [Spec1, Spec2]}}}
    },

    ?assertError(_, maps_utils:validate(Map, Spec)).


validate_map_multiple_spec_3_test() ->
    Spec1 = #{
        type => #{required => true, datatype => {in, [dog]}}
    },
    Spec2 = #{
        type => #{required => true, datatype => {in, [cat]}}
    },
    Map = #{
        x => #{
            %% any should be binary
            a => #{type => dog},
            b => #{type => horse}
        }
    },
    Spec = #{
        x => #{
            validator => {map, {atom, [Spec1, Spec2]}}}
    },

    ?assertError(_, maps_utils:validate(Map, Spec)).


validate_not_in_datatype_test() ->
    ?assertError(
        #{code := invalid_datatype},
        maps_utils:validate(
            #{x => [3]},
            #{x => #{datatype => {list, {not_in, [3]}}}}
        )
    ).

validate_fun_list_spec_error_test() ->
    SubSpec = #{y => #{datatype => binary}},
    ?assertError(
        {x, #{y := 1}},
        maps_utils:validate(
            #{x => #{y => 1}},
            #{x => #{validator => {list, SubSpec}}},
            #{error_formatter => fun
                ({invalid_value, K, V}) ->
                    {K, V}
                end
            }
        )
    ).

validate_invalid_validator_test() ->
    ?assertError(
        _,
        maps_utils:validate_update(
            #{x => 1},
            #{x => #{validator => {list, binary}}}
        )
    ).


%%  UPDATE (TO BE RE-CONSIDERED)
validate_update_update_test() ->
    ?assertEqual(
        ok,
        maps_utils:validate_update(
            #{x => 1}, #{x => 2},
            #{x => #{update_validator => fun (X,Y) -> X=<Y end}}
        )
    ).

validate_update_nested_invalid_update_test() ->
    ?assertError(
        #{code := invalid_value},
        maps_utils:validate_update(
            #{x => #{y => 1}}, #{x => #{y => 2}},
            #{x => #{update_validator => #{y => #{update_validator => fun (X,Y) -> X >= Y end}}}}
        )
    ).

validate_update_invalid_update_test() ->
    ?assertError(
        #{code := invalid_value},
        maps_utils:validate_update(
            #{x => 1}, #{x => 2},
            #{x => #{update_validator => fun (X,Y) -> X >= Y end}}
        )
    ).

validate_update_invalid_validator_test() ->
    ?assertError(
        _,
        maps_utils:validate(
            #{x => 1}, #{x => 2},
            #{x => #{update_validator => 3}}
        )
    ).

validate_complicated_1_test() ->
    ItemSpec = #{
        bar => #{
            required => true,
            datatype => boolean
        }
    },
    Spec1 = #{
        foo => #{
            required => true,
            datatype => [binary, {list, map}],
            validator => [fun erlang:is_binary/1, {list, ItemSpec}]
        }
    },
    ?assertEqual(
        #{foo => <<"bar">>},
        maps_utils:validate(#{foo => <<"bar">>}, Spec1)
    ),
    ?assertEqual(
        #{foo => [#{bar => true}]},
        maps_utils:validate(#{foo => [#{bar => true}]}, Spec1)
    ).

validate_list_test() ->
    Spec = #{
        foo => #{
            datatype => [
                {list, {in,[<<"all">>,all,<<"anonymous">>,anonymous]}},
                {list, binary}
            ]
        }
    },
    ?assertEqual(
        #{foo => [all]},
        maps_utils:validate(#{foo => [all]}, Spec)
    ),
    ?assertEqual(
        #{foo => [<<"all">>]},
        maps_utils:validate(#{foo => [<<"all">>]}, Spec)
    ),
    ?assertEqual(
        #{foo => [<<"foo">>]},
        maps_utils:validate(#{foo => [<<"foo">>]}, Spec)
    ).

validate_list_1_test() ->
    Spec = #{
        foo => #{
            datatype => [
                {in, [<<"all">>, all]},
                {list, binary}
            ]
        }
    },
    ?assertEqual(
        #{foo => all},
        maps_utils:validate(#{foo => all}, Spec)
    ),
    ?assertEqual(
        #{foo => <<"all">>},
        maps_utils:validate(#{foo => <<"all">>}, Spec)
    ),
    ?assertEqual(
        #{foo => [<<"foo">>]},
        maps_utils:validate(#{foo => [<<"foo">>]}, Spec)
    ).


validate_list_2_test() ->
    Spec = #{
        foo => #{
            datatype => [
                {in, [any, all]},
                binary
            ]
        }
    },
    ?assertEqual(
        #{foo => any},
        maps_utils:validate(#{foo => any}, Spec)
    ),
    ?assertEqual(
        #{foo => <<"foo">>},
        maps_utils:validate(#{foo => <<"foo">>}, Spec)
    ).