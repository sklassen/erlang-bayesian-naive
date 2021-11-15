# erlang-bayesian-naive
Naive Bayes Classification written in pure erlang

Extremely simple train and predict via maps. 

Each model is a erlang process.

Training happens in single calls or as batches.

Predictions are ordered by weight. 

```
Erlang/OTP 23 [erts-11.1.8] 

> BNC = bayesian:start(),
> bayesian:train(BNC,{#{name=>catmatix,coat=>black,sound=>meow},cat}),
> bayesian:train(BNC,{#{name=>rover,coat=>brown,sound=>woof},dog}),
> bayesian:train(BNC,[{#{name=>rex,coat=>brown,sound=>woof},dog},
                      {#{name=>rex,coat=>black,sound=>woof},dog}]),

> bayesian:predict(BNC,#{sound=>woof}).
[{dog,0.75},{cat,0.0}]


```

Before training you can set fields to ignore.

```
> BNC = bayesian:start().
<0.140.0>
> bayesian:field(BNC,{name,ignore}).
added
> bayesian:train(BNC,{#{name=>garfield,coat=>orange,sound=>whine},cat}).
ok
> bayesian:export(BNC).
{bnc,1,
     #{cat => #{coat => #{orange => 1},sound => #{whine => 1}}},
     [{sound,catagory},{coat,catagory},{name,ignore}]}
```
