---
title: Functors for Newbies
tags: haskell, programming
---

This is a tutorial on functors.
It is inspired by the already-excellently written article [here][tut-with-pics].

## What is a Functor?

In Haskell, there is a **Functor** type class.
If a type is an instance of the **Functor** class, it is essentially a container type that adds *context* to any value.

The **Functor** class is defined as follows:

```{.numberLines .haskell}
class Functor f where
	fmap :: (a -> b) -> f a -> f b
```

. That looks a bit weird, so I will rewrite it:

```{.numberLines .haskell}
class Functor box where
	fmap :: (apples -> oranges) -> box apples -> box oranges
```

. So, for a container type to be a real **Functor**, it has to be able to take in a function, and be able to modify the value contained inside itself *without changing the context*.
This is *extremely useful*.
Let's look at some functors.

### The List Functor

Imagine you have a list with some numbers in it: `xs = [1, 2, 3]`.
Also imagine you already wrote this function below:

```{.numberLines .haskell}
makeStatement :: Int -> String
makeStatement a
	| odd a = "Yuck."
	| otherwise = "Yay!"
```

. Now, you'd like to use `makeStatement` on the list `xs`, which has a type `[Int]`, so that every `Int` inside the list gets transformed according to `makeStatement`.
Thankfully, **List** already has an instance of the **Functor** class, so we can do it like this:

```{.numberLines .haskell}
myFunc :: [Int] -> [String]
myFunc xs = fmap makeStatement xs
```

. Though, since **List** defines `fmap` with a shorter-named `map`, we can just use `map` instead of `fmap` to save ourselves a keystroke.[^map]
While we're at it, we might as well drop the `xs` because it's redundant.

```{.numberLines .haskell}
myFunc :: [Int] -> [String]
myFunc = map makeStatement
```

This is immensely useful.
Without the **Functor** class, you'd have to use pattern matching to manually extract the values contained in the list first before applying functions on them.
Imagine a sophisticated, recursive binary tree with millions of elements --- as long as the container type has an instance for **Functor**, you can just use a one-liner `fmap` to universally apply the function at hand to every single element inside the tree (and rest assured that the *context* for all the individual values and their interrelationships remain untouched --- a win win!).

For empty lists, You can still pass along a function with `fmap` into them, but nothing will happen because they are empty (that is, there are no values inside to modify with the function).

```{.numberLines .haskell}
map makeStatement [] -- same as []
```

### The Maybe Functor

Let's look at the **Maybe** type just to prove that they are also functors.

**Maybe** is a container, except that it can only contain just 1 element (unlike **List** which can hold multiple elements).
**Maybe** has two constructors: `Just` and `Nothing`; you can use `Just` to put a value into the **Maybe** type, like how you can use the `(:[])` function to put a single item into a list.
Alternatively, you can use `Nothing` to denote the empty **Maybe** container (like `[]` for lists).

```{.numberLines .haskell}
x :: Maybe Int
x = Just 10

bigX :: Maybe Int
bigX = fmap (*2) x -- returns Just 20
```

Again, using `fmap` on an empty container will return the empty container as-is:

```{.numberLines .haskell}
fmap (*2) Nothing -- same as Nothing
```

### The Tuple Functor?

You might be wondering --- what about tuples?
Well, because tuples hold *different* types in a single container, it is impossible to implement `fmap` for it.
Recall the type signature required for `fmap`:

```{.numberLines .haskell}
class Functor box where
	fmap :: (apples -> oranges) -> box apples -> box oranges
```
. Notice that the container must contain a single type `apples` --- tuples by their nature have arbitrary numbers of *different* types (they have apples *and* oranges in them already).
This is the reason why we don't (and can't) have an `fmap` function for tuples.

## Conclusion

Functors are awesome.
If you ever define your own custom container type, be sure to make an instance for **Functor** to make life easy for everyone.

[^map]:I imagine other types' `fmap` implementation will have longer names, which would encourage you to just use the universal, 4-letter `fmap` function instead.

[tut-with-pics]:http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
