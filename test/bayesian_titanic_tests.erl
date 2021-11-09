-module(bayesian_titanic_tests).
-export([titanic/1]).
-include_lib("eunit/include/eunit.hrl").

titanic_test() ->
  BNC = bayesian:start(),
  {ok,Passengers}=file:consult("./test/titanic.edat"),
  Fun=fun(#{'Survived':=Survived}=X)->{maps:remove('Survived',X),Survived} end,
  [bayesian:train(BNC,Fun(X))||X<-Passengers],
  [
   ?assertEqual(0,element(1,lists:nth(1,bayesian:predict(BNC,#{'Sex' => 'male'})))),
   ?assertEqual(1,element(1,lists:nth(1,bayesian:predict(BNC,#{'Sex' => 'female'}))))
  ].
  

titanic(N) ->
  BNC = bayesian:start(),
  {ok,Passengers}=file:consult("./test/titanic.edat"),
  {Train,Test}=bayesian_utils:split(Passengers,N),
  Fun=fun(#{'Survived':=Survived}=X)->{maps:remove('Survived',X),Survived} end,
  [bayesian:train(BNC,Fun(X))||X<-Train],
  Sample=[Fun(X)||X<-Test],
  Results=[ {maps:get('Name',Item),element(1,lists:nth(1,bayesian:predict(BNC,Item)))==A} || {Item,A}<-Sample],
  length([Name||{Name,TF}<-Results,TF==true])/N.
