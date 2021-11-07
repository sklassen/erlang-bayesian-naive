-module(bayesian).
-export([start/1, start/0, stop/1]).
-export([summary/1, export/1]).
-export([train/2, predict/2]).
-export([difuse/1]).

% Bayesian Naive Classifier

-record(bnc,{count,data}).

start([]) -> start().
start() ->
    spawn(fun() -> init() end).

init() ->
    loop(#bnc{count=0,data=#{}}).

summary(Model) ->
    rpc(Model, summary).

train(Model, {Item, Category}) when is_map(Item) ->
    rpc(Model, {train, {Item,Category}});

train(Model, [{Item, _}=Head|Tail]) when is_map(Item) ->
    [rpc(Model, {train, {I,C}}) || {I,C} <- [Head|Tail]].


predict(Model, Item) ->
    rpc(Model, {predict, Item}).

export(Model) ->
    rpc(Model, export).

stop(Model) ->
    rpc(Model, stop).

rpc(PID, Q) ->
    PID ! {self(), Q},
    receive
        {bnc, Reply} -> Reply
    after 600 -> timeout
    end.

loop(#bnc{count=Count,data=Weights}=Model) ->
    receive
        {From, {train, {New,Catagory}}} ->
            NewWeights=difuse(New),
            From ! {bnc, ok},
            case maps:get(Catagory,Weights,none) of
              none -> loop(#bnc{count=Count+1,data=maps:put(Catagory,NewWeights,Weights)});
              OldWeights -> loop(#bnc{count=Count+1,data=maps:update(Catagory,merge(NewWeights,OldWeights),Weights)})
            end;
        {From, {predict, Item}} ->
            Sum=[{K,score(Item,maps:get(K,Weights))/Count}||K<-maps:keys(Weights)],
            From ! {bnc, lists:reverse(lists:keysort(2,Sum))},
            loop(Model);
        {From, summary} ->
            From ! {bnc, #{count=>Count,catagories=>maps:keys(Weights)}},
            loop(Model);
        {From, export} ->
            From ! {bnc, Model},
            loop(Model);
        {From, stop} ->
            From ! {bnc, stopped}
    end.

difuse(Item) ->
  Iter = maps:iterator(Item),
  difuse(maps:next(Iter),#{}).
difuse(none,Acc) ->
  Acc;
difuse({K,V, Iter},Acc) -> 
  difuse(maps:next(Iter),maps:merge(Acc,maps:put(K,maps:put(V,1,#{}),#{}))).

merge(Map1,Weights) when is_map(Map1)->
  Iter = maps:iterator(Map1),
  merge(maps:next(Iter),Weights);
merge(none,Weights) -> 
  Weights;
merge({K,Incr, Iter},Weights) -> 
  [Fld]=maps:keys(Incr),
  case maps:get(K,Weights,none) of
    none->merge(maps:next(Iter),maps:put(K,maps:put(Fld,1,#{}),Weights));
    Found -> case maps:get(Fld,Found,none) of
               none -> merge(maps:next(Iter),maps:put(K,maps:put(Fld,1,Found),Weights));
               Count -> merge(maps:next(Iter),maps:update(K,maps:merge(Found,maps:put(Fld,Count+1,#{})),Weights))
             end
  end.

score(Item,Weights) -> 
  Iter = maps:iterator(Item),
  score(maps:next(Iter),Weights,0).

score(none,_Weights,Sum) -> 
  Sum;

score({K,V, Iter},Weights,Sum) -> 
  N = maps:get(V,maps:get(K,Weights,#{}),0),
  score(maps:next(Iter),Weights,Sum+N).


