-module(bayesian_utils).
-export([split/2]).

split(Xs,N)->
    split(Xs,N,{[],[]}).
split([],_,{Train,Test})->
  {Train,Test};
split(Xs,0,{_,Test})->
  split([],0,{Xs,Test});
split([X|Xs],N,{Train,Test})->
  split(Xs,N-1,{Train,[X|Test]}).

