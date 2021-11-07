-module(bayesian_simple_tests).
-include_lib("eunit/include/eunit.hrl").

animals_test() ->
  BNC = bayesian:start(),
  bayesian:train(BNC,{#{name=>catmatix,coat=>black,sound=>meow},cat}),
  bayesian:train(BNC,{#{name=>rover,coat=>brown,sound=>woof},dog}),
  bayesian:train(BNC,{#{name=>rex,coat=>brown,sound=>woof},dog}),
  bayesian:train(BNC,{#{name=>rex,coat=>black,sound=>woof},dog}),
  [
   ?assertEqual(dog,element(1,lists:nth(1,bayesian:predict(BNC,#{coat=>black,name=>rex})))),
   ?assertEqual(cat,element(1,lists:nth(1,bayesian:predict(BNC,#{sound=>meow})))),
   ?assertEqual(dog,element(1,lists:nth(1,bayesian:predict(BNC,#{sound=>woof}))))
  ].

