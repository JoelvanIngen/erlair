-module(test2).
-export([testA/2, testB/2]).

testA(A, _B) when A == 1 -> 1;
testA(A, _B) when A == 2 -> 2.
testB(_A, _B) -> 1.
