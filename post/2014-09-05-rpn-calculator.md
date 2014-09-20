---
title: Reverse Polish Notation (RPN) Calculator in Ruby and Haskell
tags: programming, ruby, haskell
mathjax: on
---

A friend of mine is in DevBootCamp, an intensive coding school program designed to get students ready for real world job placement.
Anyway, he was telling me how he was given an extra credit assignment: implement a RPN calculator which supports 3 functions --- add, subtract, and multiply --- on integer numbers.
I.e., write a function `evaluate()` that when given a string such as `"3 4 + 8 *"` gives the result `56`.
He gave me his Ruby solution; in response, I wrote my own Ruby version and decided soon thereafter to write an equivalent Haskell version, because, why not?

## Ruby

First, the Ruby version:

```{.ruby .numberLines}
def evaluate(equation_str)
	terms = equation_str.split(' ')
	stack = []
	while terms.size > 0
		term = terms.shift
		case term
		when /^\d+?$/
			stack.push(term.to_i)
		when '+', '-', '*'
			if stack.size < 2
				raise "stack too small for operator application"
			else
				b = stack.pop
				a = stack.pop
				op = term.to_sym
				c = b.send(op, a)
				stack.push(c)
			end
		else
			raise "invalid input `#{term}'"
		end
	end
	stack
end
```

Pretty straightforward, yes?
The `terms` variable holds an array of numbers and operators.
We use a calculator stack (aptly named, `stack`) to untangle the RPN in a stack-based way.

In the `while` loop, the actions depend on what `term` looks like.
If `term` is a number in string form, we convert it into a real number with `String#to_i`, and push it into `stack`.
If `term` is one of the 3 recognized operators, we remove 2 terms `a` and `b` from `stack` and apply the necessary operation on it with Ruby's magic `Object#send` method, and push the new term `c` back into `stack` for further operations (if any, in later iterations of the `while` loop).
Otherwise, we reject the input as an invalid term.

## Haskell

Here is the Haskell version:

```{.haskell .numberLines}
import Data.Char (isDigit)

data Term
	= TermInt Integer
	| TermOp (Integer -> Integer -> Integer)

evaluate :: String -> [Integer]
evaluate = evalTerms . map mkTerm . words

mkTerm :: String -> Term
mkTerm termStr = case termStr of
	"+" -> TermOp (+)
	"-" -> TermOp (-)
	"*" -> TermOp (*)
	_
		| and $ map isDigit termStr -> TermInt $ read termStr
		| otherwise -> error $ "invalid input `" ++ termStr ++ "'"

evalTerms :: [Term] -> [Integer]
evalTerms = foldl modifyStack []
	where
	modifyStack stack term = case term of
		TermInt n -> n : stack
		TermOp op -> case stack of
			(a:b:_) -> op a b : drop 2 stack
			_ -> error "stack too small for operator application"
```

Probably the first thing to note is that we define a robust data type, `Term`, to encapsulate the values held in a given RPN string.
We take advantage of Haskell's functions-as-first-class-values ability, and define the `TermOp` constructor with it (i.e., it needs an arithmetic function `(Integer -> Integer -> Integer)` as an argument).

The next thing to notice is that the `evaluate` function is composed of smaller helper functions, `mkTerm` and `evalTerms`.
`mkTerm` simply converts a `String` type into an appropriate `Term` type.
`evalTerms` takes a list of `Term` values, and reduces it as much as possible by applying the `modifyStack` function over it with `foldl` (Haskell's version of a single-pass loop).

When `modifyStack` encounters a `TermInt`, it pushes the number into the stack.
When it encounters a `TermOp`, it applies that operator to the first 2 items in `stack`, and pushes this result back into `stack`.
We use pattern matching with `(a:b:_)` to pull out the `a` and `b` values from the stack --- the expression `(a:b:_)` means a value that matches either $[x_1, x_2]$, or $[x_1, x_2, ... , x_n]$, because the `_` operator matches anything, including the empty list `[]` used to finalize list creation.
The `drop 2 stack` is necessary because Haskell's types by default are immutable.

## Thoughts

I much prefer the Haskell version.

The separation of concerns is a big win --- we can easily create helper functions like `mkTerm` and `evalTerms` because of Haskell's purity[^purity].
Haskell embraces the use of algebraic data types (i.e., `Term` here), and perhaps this preference lends itself to the use of helper functions that convert things from one type to another.
Ruby does not have types, at least in the sense of Haskell types, so to artificially create such concepts and to implement them would be difficult.[^ruby-type]
I mean, I really want to write equivalent `mkTerm` and `evalTerms` methods in Ruby, but my beginner skills prevent me from doing it in a simple, straightforward way.
I know enough about coding to abandon "solutions" that require circuitous, complex design.

I also like how all the functions are pure and thus easy to reason about with the type signatures.
Refactoring code like this is a dream.

In short, algebraic data types, the clean delegation of subtasks to helper functions, and purity make the Haskell version easier to reason about and maintain in the long run.

[^purity]: Haskell's *purity* is a term of art. Look it up!
[^ruby-type]: The closest thing to Haskell types in Ruby, in my mind, are Ruby objects.
Certainly, Ruby knows the difference between two objects belonging to different classes, but to do that here would make the code much longer and offer little benefit.
