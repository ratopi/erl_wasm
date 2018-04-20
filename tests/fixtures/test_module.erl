-module(test_module).
-export([add/2]).

add(A,B) ->  id(A) + id(B).

id(I) -> I.
