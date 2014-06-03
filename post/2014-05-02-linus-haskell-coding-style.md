---
title: Linus's Haskell Coding Style (LHCS)
tags: haskell, programming
mathjax: off
---

Haskell is my favorite programming language.
However, unlike mainstream languages (C, Java, Ruby, etc.), there are no known "coding styles" for it.
C has the venerable Linux Kernel Coding Style [LKCS](https://www.kernel.org/doc/Documentation/CodingStyle), among many others.
I really like LKCS because it is designed to be as readable as possible, and is actually pretty simple (no fancy indentation/brace rules).
I just wish there was something like LKCS for Haskell, mainly because every time I open up Haskell code somebody else wrote, it is plagued with gargantuan amounts of indentation.
It really has gotten out of hand.
So, I hereby propose some rules to reduce the level of indentation, whether you use tabs or spaces.[^tabs]

NOTE: I use 4-character-wide tabs for Haskell, but because HTML renders tabs as 8 characters wide, I've used 4 spaces for a single tab in this post.

## Chapter 1: Indentation

### Wherever possible, use minimal hanging indentation

That is, indent as little as possible.

#### `where` clause

Whenever you use a `where` clause, use it like this:

```
    ...
    f x y
    where
    f = ...
    x = ...
    y = ...
    ...
```

That is, put `where` on its own line, and then avoid any additional indentation for all definitions that follow.

#### Guards

Use guards like this:

```
someArbitraryFunctionName x y z
    | x == 0 = ...
    | y == 0 = ...
    | z == 0 = ...
    | otherwise = ...
```

not like this:

```
someArbitraryFunctionName x y z | x == 0 = ...
                                | y == 0 = ...
                                | z == 0 = ...
                                | otherwise = ...
```

I.e., put guards on their own line with a single indent.

#### `case` expressions

Do not vertically-align the different cases.

Do

```
foo x = case x of
    Abcdef y -> ...
    Foo y -> ...
    Abracadabra y -> ...
```

instead of

```
foo x = case x of
    Abcdef y      -> ...
    Foo y         -> ...
    Abracadabra y -> ...
```

.
The only time you should vertically align *anything* is for lists, or perhaps long tuples.

#### Lists

Align lists as follows:

```
someLongList =
    [ abcd
    , efghi
    , jkl
    , m
    , nopqrstu
    , vwxyz
    ]
```

.
You may align long tuples in a similar manner.

#### `do` notation

Use ``hanging'' do-notation:

```
main = do
    x <- foo
    y z
```

instead of

```
main = do x <- foo
          y z
```

(which tries to align the `x` and `y` together)

or

```
main = do
          x <- foo
          y z
```

.
When writing `let` statements inside `do` notation, write the keyword `let` on its own line, and indent any subsequent `let`-bindings.

## Chapter 2: Misc Guidelines

### Always use explicit type signatures

Everyone reading your code will appreciate it.
Don't force me to load up your code in GHCi to figure out how to use it with (:t) --- **stop wasting everyone's time**.

### Refrain from infix notation

Sometimes infix notation makes code shorter, but it gets confusing when you use the same function with and without it.
Also, in many cases prefix notation is simply shorter because you don't have to use the two backtick characters around the function name.
Use infix notation as often as you'd use GOTO in C.

### Prefer `where` over `let`

This is because if you use `where` clauses, the high-level code comes first, followed by `where` and the various smaller components that make up the preceding code.
Macro-to-micro reading/understanding is almost always superior.
Using `where` clauses also has the benefit of allowing you to more easily move out helper functions defined as part of a `where` block into their own standalone functions if they grow too big.
If you use `let` expressions, you run the risk of including variables in the parent function block's scope, which makes "detaching" them harder later on.

### Do not use multiline {- -} comment blocks

Haddock prefers single-line comments, anyway.

### Use two groups of imports: system-wide and package-wide (and have each group alphabetically ordered)

Import all external library modules in their own ``paragraph'', followed by those modules of your own library; e.g., do


```
    import Control.Monad
    import Data.Maybe
    import Data.Text

    import MyModule.One
    import MyModule.Two
```

.

### Place one pragma per line, in alphabetical order

Do it like this:
```
    {-# LANGUAGE SomeBadExtension #-}
    {-# LANGUAGE SomeCoolExtension #-}
    {-# LANGUAGE SomeScaryExtension #-}
    {-# LANGUAGE SomeUglyExtension #-}
    {-# LANGUAGE StrangeExtensionNobodyUnderstands #-}
```

### Try to use as few pragmas as possible

Do not use 20 language extensions when you don't have to.
Saving a handful of keystrokes is not a good reason to use a multitude of extensions.

### When writing documentation, prefer high-level documentation over line-by-line documentation

If you have a 20-line function, write how it works with a high-level overview.
Haskell is a pure language so it's very easy to tell how the small pieces work.
So, explain the bigger pieces, and *why* they are necessary as written.

### Breaking long lines and strings

We follow LKCS's 80-character column limit.

If you must, break long lines into sensible chunks.
If a function has numerous arguments, you may put each argument on its own line.
If you want to break up a long chain of functions, break them up like this:

```
foo x y z = head
    . init
    . blahblah y
    . funcMX x
    $ abcd z
```

, with each function on its own line, with the function composition operator `.` as the leading character, similar to how the comma `,` is the leading character for breaking up long lists.

[^tabs]: I prefer tabs for indentation, and spaces for alignment (which does happen once in a blue moon).
