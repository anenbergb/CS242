{-

Lecture 3
Aside: Fixpoint on streams.

Function g:
1, 2, 3, 4
0, 1, 3, 5, 7

Fixpoint is Fibonaccis
0, 1, ..
0, 1, 1, ..
0, 1, 1, 2, ...
0, 1, 1, 2, 3, ...

Start w/ empty stream epsilon, 

Fixpoint x of a function f, such that f(x) = x

One strategy: Start w/ empty stream e, then f(f(...f(f(e))))

Lambda calculus:

e ::= x | lambda x.e | e1 e2

Javascript
e ::= x
| function(x) {return e1}
| e1(e2)

Haskell
e :: x
  | \x -> e
  | e1 e2

model of computation that is Turing complete.

lambda + evaluation strategy
call-by-value : javascript
call-by-name : haskell

lambda + type system
slmply-typed lambda calculus
polymorphic lambda caluculus
dependent types

Roadmap
the lambda-calculus
capturing-avoiding subsitution
evaluation order

Recap:
e ::= x | lambda x.e | e1 e2

(lambda x. (2+x))   (add 2)
(lambda x. (2+x)) 5 = 7


Recap: Closures
((lambda x. (lambda y. x)) 2 ) 3
(lambda y. 2) 3
-> 2
Returned functoin has x subsituted.

lambda xy.e = lambda x. (lambda y. e)
Left associative application:
	f x y = (f x) y  =not= f ( x y)
	lambda x. f x = lambda x. (f x) =not= (lambda x. f) x
like Haskell: \x y -> e == \x -> (\y ->e)

--implicit parenthesization 
x = e1 e2
y = f x
y = f (e1 e2)


function f(x) {
	return x + 2;
}
f(f(3));

=>> desugar
(lambda f. f(f 3))
lambda x. x+2

let x = e1 in 32
=>>
(lambda x. e2) e1

Bound and Free variable
(lambda x.x)  : Bound Variable (a closed term)

(lambda x. y) : Free variable (open term)

Bound and Free variables summary

-}
