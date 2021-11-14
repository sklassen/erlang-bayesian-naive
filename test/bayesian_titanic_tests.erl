-module(bayesian_titanic_tests).
-export([predict/1,guess/1,died/1]).
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
  

predict(N) ->
  BNC = bayesian:start(),
  {ok,Passengers}=file:consult("./test/titanic.edat"),
  {Test,Train}=bayesian_utils:split(N,Passengers),
  Fun=fun(#{'Survived':=Survived}=X)->{maps:remove('Survived',X),Survived} end,
  [bayesian:train(BNC,Fun(X))||X<-Train],
  Sample=[Fun(X)||X<-Test],
  Results=[ {maps:get('Name',Item),element(1,lists:nth(1,bayesian:predict(BNC,Item)))==A} || {Item,A}<-Sample],
  length([Name||{Name,TF}<-Results,TF==true])/N.

died(N) ->
  {ok,Passengers}=file:consult("./test/titanic.edat"),
  {Test,_Train}=bayesian_utils:split(N,Passengers),
  Fun=fun(#{'Survived':=Survived}=X)->{maps:remove('Survived',X),Survived} end,
  Sample=[Fun(X)||X<-Test],
  Results=[ {maps:get('Name',Item),A==0} || {Item,A}<-Sample],
  length([Name||{Name,TF}<-Results,TF==true])/N.

guess(N) ->
  {ok,Passengers}=file:consult("./test/titanic.edat"),
  {Test,Train}=bayesian_utils:split(N,Passengers),
  Fun=fun(#{'Survived':=Survived}=X)->{maps:remove('Survived',X),Survived} end,
  Sample=[Fun(X)||X<-Train],
  {Yes,No}=lists:foldl(fun({_,Z},{A,B})->case Z of 0->{A,B+1}; 1->{A+1,B} end end,{0,0},Sample),
  Weight=round(100*Yes/(Yes+No)),
  Examine=[Fun(X)||X<-Test],
  Results=[ {maps:get('Name',Item),A==(case rand:uniform(100)<Weight of true->1; false->0 end)} || {Item,A}<-Examine],
  length([Name||{Name,TF}<-Results,TF==true])/N.
