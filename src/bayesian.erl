-module(bayesian).
-export([start/1, start/0, stop/1]).
-export([summary/1, field/2, export/1]).
-export([train/2, predict/2]).

% Bayesian Naive Classifier

-record(bnc, {count, data, fields}).

start() ->
    start(#bnc{count = 0, data = #{}, fields = []}).
start([]) -> 
    start();
start(Import) ->
    spawn(fun() -> init(Import) end).

init(Import) ->
    loop(Import).

train(Model, {Item, Category}) when is_map(Item) ->
    rpc(Model, {train, {Item, Category}});
train(Model, [{Item, _} = Head | Tail]) when is_map(Item) ->
    [train(Model, {I, C}) || {I, C} <- [Head | Tail]].

predict(Model, Item) ->
    rpc(Model, {predict, Item}).

field(Model, Field) when is_tuple(Field)->
    rpc(Model, {field, Field}).

summary(Model) ->
    rpc(Model, summary).

export(Model) ->
    rpc(Model, export).

stop(Model) ->
    rpc(Model, stop).

rpc(PID, Q) ->
    PID ! {self(), Q},
    receive
        {bnc, Reply} -> Reply
    after 900 -> timeout
    end.

loop(#bnc{count = Count, data = Weights, fields = Fields} = Model) ->
    receive
        {From, {train, {New, Catagory}}} ->
            {NewFields,NewWeights}=diffuse(New,Fields),
            From ! {bnc, ok},
            case maps:get(Catagory, Weights, none) of
                none ->
                    loop(#bnc{
                        count = Count + 1, 
                        fields= NewFields,
                        data = maps:put(Catagory, NewWeights, Weights)
                    });
                OldWeights ->
                    loop(#bnc{
                        count = Count + 1,
                        fields= NewFields,
                        data = maps:update(Catagory, update(NewWeights, OldWeights), Weights)
                    })
            end;
        {From, {predict, Item}} ->
            Sum = [{K, score(Item, maps:get(K, Weights)) / Count} || K <- maps:keys(Weights)],
            From ! {bnc, lists:reverse(lists:keysort(2,Sum))},
            loop(Model);
        {From, {field, Field}} ->
            Key = element(1,Field),
            case lists:keyfind(Key, 1, Fields) of
              false -> From ! {bnc, added}, loop(Model#bnc{fields=[Field|Fields]});
              _Found-> From ! {bnc, replaced}, loop(Model#bnc{fields=lists:keyreplace(Key,1,Fields,Field)})
            end;
        {From, summary} ->
            From ! {bnc, #{count => Count, catagories => maps:keys(Weights)}},
            loop(Model);
        {From, export} ->
            From ! {bnc, Model},
            loop(Model);
        {From, stop} ->
            From ! {bnc, stopped}
    end.

diffuse(Item, Fields) ->
    Iter = maps:iterator(Item),
    diffuse(maps:next(Iter), Fields, #{}).
diffuse(none, Fields, Acc) ->
  {Fields,Acc};
diffuse({K, V, Iter}, Fields, Acc) ->
    case lists:keyfind(K,1,Fields) of
      false -> diffuse(maps:next(Iter),[{K,catagory}|Fields], maps:merge(Acc, maps:put(K, maps:put(V, 1, #{}), #{})));
      {K,ignore}-> diffuse(maps:next(Iter), Fields, Acc);
      {K,Fun} when is_function(Fun)-> diffuse(maps:next(Iter), Fields, maps:merge(Acc, maps:put(K, maps:put(Fun(V), 1, #{}), #{})));
      _ -> diffuse(maps:next(Iter), Fields, maps:merge(Acc, maps:put(K, maps:put(V, 1, #{}), #{})))
    end.

update(Map1, Weights) when is_map(Map1) ->
    Iter = maps:iterator(Map1),
    update(maps:next(Iter), Weights);
update(none, Weights) ->
    Weights;
update({K, Incr, Iter}, Weights) ->
    [Fld] = maps:keys(Incr),
    case maps:get(K, Weights, none) of
        none ->
            update(maps:next(Iter), maps:put(K, maps:put(Fld, 1, #{}), Weights));
        Found ->
            case maps:get(Fld, Found, none) of
                none ->
                    update(maps:next(Iter), maps:put(K, maps:put(Fld, 1, Found), Weights));
                Count ->
                    update(
                        maps:next(Iter),
                        maps:update(K, maps:merge(Found, maps:put(Fld, Count + 1, #{})), Weights)
                    )
            end
    end.

score(Item, Weights) ->
    Iter = maps:iterator(Item),
    score(maps:next(Iter), Weights, 0).
score(none, _Weights, Sum) ->
    Sum;
score({K, V, Iter}, Weights, Sum) ->
    N = maps:get(V, maps:get(K, Weights, #{}), 0),
    score(maps:next(Iter), Weights, Sum + N).
