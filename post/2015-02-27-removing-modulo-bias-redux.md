---
title: Generating Random Numbers without Modulo Bias, Redux
tags: programming, rng, c
mathjax: on
---

In an earlier post, I discussed [how to remove modulo bias from your C code][old-approach].
I would like to add a different version of removing modulo bias, inspired by the code from the [PCG family](http://www.pcg-random.org/) of random number generators.

## The PCG Version

In the [official minimal C implementation](http://www.pcg-random.org/download.html) of PCG, there is a function called `pcg32_boundedrand_r`, which takes a `bound` variable.
The code for that function is as follows:

```{.numberLines .c}
uint32_t pcg32_boundedrand_r(pcg32_random_t* rng, uint32_t bound)
{
	uint32_t threshold = -bound % bound;
	for (;;) {
		uint32_t r = pcg32_random_r(rng);
		if (r >= threshold)
			return r % bound;
	}
}
```

.
If `bound` is 5, then the function will return a uniform range from 0 to 4.
I.e., the return value `r` is such that $0 \leq r < bound$.

## Explanation of the PCG Version

Although I generally prefer to explain code in the order they appear, top to bottom, for this case I would like to explain the `pcg32_boundedrand_r()` function by talking about the usual broken version found in the wild.

### Broken Version

The broken version is like this:

```
r = some_rng();
return r % bound;
```

.
Unless `r` is a multiple of `bound`, we will incur the wrath of modulo bias in the above code.
Why is this?
Well, consider what the modulo operator does.
All it does is chop off any excess range of values that is not a multiple of `bound`.
Consider the following ASCII diagram, where the maximum value returned by our `some_rng()` function is just 11:

```
0 1 2 3 4 5 6 7 8 9 10 11
```

.
So, `some_rng()` has 8 possible unique values, 0 through 11.
If our bound was `4`, then doing `r % bound` is perfectly fine, because we can chop the RNG range into two equal parts of length 4 each, like this:


```
0 1 2 3 | 4 5 6 7 | 8 9 10 11
```

.
If each of the 12 values 0 through 11 occurs uniformly[^uniform], then we can assume that each of the values occurs $\frac{1}{12}$ times.
If we apply `% 4` to the values, then our output range looks like this:

```
0 1 2 3 | 0 1 2 3 | 0 1 2 3
```

.
Can you see how the three subparts are the same?
What's more, 0 will occur $\frac{3}{12} = \frac{1}{4}$ times.
The same goes for `1`, `2`, and `3`.
Because all 4 possible values, 0 through 4, occur an equal $\frac{1}{4}$ times, there is *no modulo bias here*!

#### What if the modulus is not a nice number with respect to RAND_MAX?

This is where the problem occurs.
`RAND_MAX` is the highest value returned by our RNG, and thus defines the range of the possible values output by the RNG.[^rand-max]
Continuing with our example above, if instead of `bound = 4`, we used another value like 5, we will get this instead:


```
0 1 2 3 4 | 5 6 7 8 9 | 10 11
```

--- or essentially, these values as output if we just naivly use `r % bound`:

```
0 1 2 3 4 | 0 1 2 3 4 | 0 1
```

.
Can you see how `0` and `1` occur $\frac{3}{12}$ times, but `2`, `3`, and `4` occur $\frac{2}{12}$ times?

#### The fix --- adjust the range!

Now, we can fix the above example by simply throwing out certain values.
The approach I used in the [old appraoch][old-approach] was to discard the right hand side values.
So, in our example with `bound = 5`, where we have

```
0 1 2 3 4 | 0 1 2 3 4 | 0 1
```

, the old example tried to discard the last 2 values, like this:

```
0 1 2 3 4 | 0 1 2 3 4 | x x
```

.
In other words, if `r` fell within the range 10 through 11, then we'd simply discard it and call `some_rand()` again.

But it doesn't have to be this way.
Instead of throwing out the values on the right, we can throw out the values on the left!
So, instead of

```
0 1 2 3 4 | 0 1 2 3 4 | x x
```

we can instead do

```
x x 2 3 4 | 0 1 2 3 4 | 0 1
```

.
Can you see how all of the values 0 through 4 occur exactly 2 times?
No more bias!

### PCG's approach

The approach in PCG is the same; here is the code again:

```{.numberLines .c}
uint32_t pcg32_boundedrand_r(pcg32_random_t* rng, uint32_t bound)
{
	uint32_t threshold = -bound % bound;
	for (;;) {
		uint32_t r = pcg32_random_r(rng);
		if (r >= threshold)
			return r % bound;
	}
}
```

.
The `threshold` value is the initial range that must be discarded.
So we can visualize it like this:

```
x x 2 3 4 | 0 1 2 3 4 | 0 1

// The `x x` here is the initial threshold range, that must be discarded.
```

On line 3, we determine the value of `threshold`, and then on line 4 we enter a `for` loop that repeatedly calls the RNG until we get a value outside of this threshold range (well, technically, *greater-than-or-equal-to* the threshold value).
If we do get such a value, then we return the modulo of it.
If we visualize it, it's like this:

```
0 1 2 3 4 | 5 6 7 8 9 | 10 11

- Discard 0 and 1 (the threshold area).

x x 2 3 4 | 5 6 7 8 9 | 10 11

- Return (r % bound).

x x 2 3 4 | 0 1 2 3 4 | 0 1
```

.

#### How is `threshold` calculated?

The above high-level explanation should be sufficient for you, dear reader.
But if you want to go down to the innards of C, to see how `pcg32_boundedrand_r` works, read on.

The cornerstone of the PCG approach is to use a variable called `threshold`.
The code to calculate `threshold` is somewhat complicated:

```{.c}
uint32_t threshold = -bound % bound;
```

.
Now, let's remind ourselves that the whole point of `threshold` is to be a minimum value that sets a cutoff of values to be discarded --- if the generated value `r` is too low (lower than our threshold), then we discard it.

Consider the following diagram, with `RAND_MAX` set to `11`, and `bound` set to 5:

```
0 1 2 3 4 | 5 6 7 8 9 | 10 11
```
.
We can visualze the above like this instead:

```
R R R R R | _ _ R R R | R R
```

.
The underscores represent the values that must be *skipped* over, in order to eliminate modulo bias.
The question then, is to figure out how to count the number of underscores.
In our case, it is 2, and so as long as we skip the first two values 0 and 1, we should be fine.

The first step is to count backwards from the right-hand edge:

```
                |<---count|
R R R R R | _ _ R R R | R R
                t         |
                          \-> RAND_MAX
```

.
We end up where `t` is on the diagram above, by counting backwards from `RAND_MAX`.
If we then take the modulo of this by `count` itself, then we end up with `t` being the value we want --- the number of underscores.
This is the essence of `-bound % bound` --- we first take `-bound` which is obtained by counting backwards from `RAND_MAX`, and then we take the modulo of this number by `bound` itself, to get what we need.
Using the ASCII diagram again, we get

```
                |<-----count|
0 1 2 3 4 | 5 6 7 8 9 | 10 11
                t          |
                           \-> RAND_MAX

```

where `t` is 7, and now applying % 5, we get:

```
0 1 2 3 4 | 0 1 2 3 4 | 0 1
                t
```

`t = 2`, the correct answer!
You can try out different values for `RAND_MAX` and `bound`, but you will get the right answer each time using `threshold = -bound % bound`.

##### But why is `-bound` the way it is?

The [C standard](http://stackoverflow.com/questions/2711522/what-happens-if-i-assign-a-negative-value-to-an-unsigned-variable) says that a negative unsigned value is stored as a positive value.
Without getting too technical, here is what the values of `bound` look like:

```
Bound | Actual value
------+-------------
2     | 2
1     | 1
0     | 0
-1    | (2^32) - 1 (same as RAND_MAX)
-2    | (2^32) - 2 (same as RAND_MAX - 1)
-3    | (2^32) - 3 (same as RAND_MAX - 2)
... and so on
```
.
In our case, if our `RAND_MAX` is 11, and `bound` is 5, then `-bound` is indeed 7.

```
Bound | Actual value
------+-------------
0     | 0
-1    | 11 - 0 (same as RAND_MAX)
-2    | 11 - 1 (same as RAND_MAX - 1)
-3    | 11 - 2 (same as RAND_MAX - 2)
-4    | 11 - 3 (same as RAND_MAX - 3)
-5    | 11 - 4 (same as RAND_MAX - 4) = 7
... and so on
```

## Conclusion

I thoroughly enjoyed looking at the source code in PCG, only to discover an elegant solution around removing modulo bias.
Unfortunately, I do not know the true origin of this approach; it is possible that the authors of the PCG code invented it, but I find this improbable.
Meanwhile, I strongly recommend the following code for anyone using a low-level generator that does not come with a bounded version:

- i rng/pcg-style.c

[old-approach]:2013-07-12-generating-random-numbers-without-modulo-bias.html

[^uniform]: Any RNG worth their salt will return a uniformly distributed value, typically from 0 to RAND_MAX.
In the example here, our RAND_MAX is 11.

[^rand-max]: For a 32-bit unsigned integer RNG, $2^{32} - 1$ (all 1 bits set) is the highest value that can be returned.
That is, our RNG returns a value from 0 (no bits set) to `RAND_MAX` (all bits set).
This means that our RNG generates everything from all 0s to all 1s and everything in between.
