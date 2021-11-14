-module(bayesian_utils).
-export([split/2]).

split(N,Xs) when length(Xs)>N ->
    split(N,Xs,{[],[]}).
split(0,Xs,{Test,_})->
  {Test,Xs};
split(N,Xs,{Test,Train})->
  {X,Tail}=take_and_remove(rand:uniform(length(Xs)),Xs),
  split(N-1,Tail,{[X|Test],Train}).

take_and_remove(Nth,Xs)->
  {Head,Tail}=lists:split(Nth,Xs),
  {lists:last(Head),lists:merge(element(1,lists:split(length(Head) - 1, Head)),Tail)}.

