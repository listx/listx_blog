---
title: Implementing Binary Search
tags: programming, ruby, haskell, c
mathjax: on
---

If you are a programmer, I'm sure you've encountered the term "binary search" at some point in time.
I myself already know what binary search is, but I'm writing this post to solidify my understanding of it, and because I just want to compare how it might be naively implemented across my 3 favorite languages C, Ruby, and Haskell.

## Background

You can skip this section if you want --- I merely want to write how I first met and fell in love with the concept of "binary".
I first naively discovered "binary search", or rather, "binary division" when I was in high school.
It was a very pleasant realization, and at the time I did not fully realize what I had accidentally encountered.

The situation was so: my Biology teacher, Mr. Kennedy, told the class to draw an even 4x4 square grid on the palms of our hands.
We were learning about the sensory nervous system, and this exercise was supposed to teach us how delicate our hands were to the sense of touch, or something of that nature.
Details aside, everyone was supposed to draw their own 4x4 grid --- but this was when I asked myself, "how can I draw the most even-looking grid, without using a ruler?"
You see, I could not use a ruler easily because I was using my right hand to draw onto my left hand --- and to hold a ruler properly, I would need a third hand!
So there was the problem.

On the one hand, I could not simply draw the four horizontal and four vertical lines one after the other, because I knew that I would introduce a great deal of error, from the first leftmost line to the last rightmost line.
I did not want to draw an ugly, lopsided square.

It took me a few seconds, but I came up with a plan.
I first drew a single large square.
Then, knowing that I could easily eyeball, with good accuracy, where the *middle point of that square* was horizontally, I draw a vertical line right down the middle.
I then did the same thing in the other axis vertically.
I repeated this procedure a few more times, *subdividing* each smaller rectangular shape into halves.
I glanced around the room, and later looked on at other students' palms to see if they had discovered this "divide by $\frac{1}{2}$" trick, but to my dismay no one had done it; I knew this was the case because everyone had very sloppy-looking squares.

I cannot honestly say if this was the very first time I realized the underlying geometric concepts at play here, but I can affirmatively say that it really was the first time I systematically applied such an elegant solution to the given problem.
I wonder if most people who draw grids bother to even think of it as a problem, as the task remains rather trivial --- "draw a grid."

Now, you might say "hey, but that's not binary search at all!"
But to me, binary subdivision is perhaps the underpinning principle behind binary search.
In my opinion, it makes more sense to first describe binary subdivision first before teaching binary search.
So that's that.

## The Problem

The problem is simple --- given a sorted list `L` of items (for simplicity's sake, integers), determine if `x` is in it, and also, the position of `x` in the list if it is indeed in the list.
The catch is that you have no idea as to the contents of `L` --- only that it is sorted from smallest to larget.

## Naive Approach --- Linear Search

The simplest way of doing this is a linear search.
It is probably the novice programmer's first reaction.
You just start from the first item in `L`, and run a **for**-loop all the way across the list, looking at every single item.
There are now two possibilities --- either (1) you indeed discover `x`, and report back your position (aka the "index", usually in the form of the familiar `i` variable in C/C++/Java code), or (2) you find nothing.
If you are clever, you can optimize the search in the second situation by breaking out of the **for**-loop if you items you are comparing are larger than `x`; after all, `L` is sorted, so we know for a fact that the later numbers are only going to get bigger, so there is no point in trying to find `x` in that crowd of numbers.

And, this is it.
There is nothing more to optimize using this method (let's not get into parallelization).
But think of the consequences --- what's the worst case scenario?
Imagine you have 1 trillion items, and that `x` is not in it, because let's say `x` is a much bigger number than the biggest number in `L` --- but of course you haven't run the linear search yet, so you don't know that.
Given this situation, you would have to search the *entire* list of all numbers in `L` before reaching condition (2) described above.

If you wanted to get a little more clever to avoid this problem of searching all 1 trillion items, you could tell the algorithm to refuse to enter the **for**-loop if `x` lies outside the *bounds* of `L`.
Checking the bounds is easy and takes constant time, as you merely check for the first and last item's size (again, as `L` is sorted), and those are your bounds.
This way, if `x` lies outside the bounds, you can *guarantee* that it is not in `L`, no matter how many items `L` has.

But, that really is the end of the optimizations.
What else can you do, really, when searching linearly, looping through every item from the beginning to the next?

## Inverted Bounds-Checking, aka Binary Search

The heading of this subsection might have already given you a hint as to what binary search entails.
Binary search takes the powerful concept of bounds-checking, and applies it repeatedly, recursively, against `L`.

```
Blah blah blah.
```

## List items

- apple
- banana
- candy

## Raw code insertion

- i toy/parking-space.rb
