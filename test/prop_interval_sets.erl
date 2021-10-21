-module(prop_interval_sets).
-include_lib("proper/include/proper.hrl").
-compile(export_all).

prop_test() ->
    ?FORALL(IntervalSet, list(element()),
        begin
            IntervalSet
        end
    ).

%% GENERATORS

element() ->
    {integer()}.