---
title: Development Best Practices
tags: programming
---

This is a summary of the article [*Best Practices for Scientific Computing* (Greg Wilson et. al, 2012)][paper].
I think the points raised have importance in software development practices in general.
A lot of the material resonated with me as a developer, and I recommend you read it or my condensed version (with my embedded comments) below.

## Write Programs for People, Not Computers

#### A program should not require its readers to hold more than a handful of facts in memory at once.

Instead of

```{.numberLines}
def rect_area(x1, y1, x2, y2):
	...calculation...
```
write

```{.numberLines}
def rect_area(point1, point2):
	...calculation...
```
.
You should make function arguments less dependent on argument order, or remove argument ordering requirements completely.

#### Names should be consistent, distinctive, and meaningful.

On a similar note, do not name variables "data" or "foo".
I think this also depends on static vs. dynamic typing.[^naming]

#### Code style and formatting should be consistent.

Be like the Linux Kernel project --- use a single coding style.

#### All aspects of software development should be broken down into tasks roughly an hour long.

Our brains start to lose focus after 60-90 minutes of performing an intense activity.

## Automate Repetitive Tasks

#### Rely on the computer to repeat tasks.

Write shell scripts if you need to.

#### Save recent commands in a file for re-use.

Take advantage of your shell's ability to save history.

#### Use a build tool to automate workflows.

You can use *GNU Make* not only to compile code, but to describe a dependency graph of your workflow (do X before Y).

## Use the Computer to Record History

#### Software tools should be used to track computational work automatically, to achieve provenance.

To reproduce your results, you should record everything needed to re-create the output[^provenance], and such record-keeping should be done automatically.
In a software development context, the most immediate example is version control, which keeps all of the code necessary to recreate the desired program behavior.
However, there are several other things which you should also keep track of in your development notebook:

- UUIDs and version numbers for raw data records (e.g., sample parameters/input for your program);
- UUIDs and version numbers for programs and libraries;
- the values and parameters used to generate any given output; and
- the names and version numbers of programs (however small) used to generate those outputs.

This is not entirely on point, but every Linux distribution keeps careful records of all packages (dependencies, version numbers, etc.), so that a user can reliably install and use that package if they want to.
There is a lesson to be learned here.

## Make Incremental Changes

#### Work in small steps with frequent feedback and course correction.

Do not plan months or years of work in advance.
The goal is to produce working (but incomplete) code after each iteration.

## Use Version Control

#### Use a version control system.

Again, note that version control gives you provenance for you program's behavior *throughout its development history*, so that you can always reproduce the behavior at any given point in time.
Version control gives you provenance for your program --- it's just that powerful.

#### Everything that has been created manually should be put in version control.

Large binary files should still be recorded (git-annex comes to mind).
If you don't want to use git-annex, just write down the SHA1 hash of the binary file, and commit this hash number into your version control.
Add in a test suite that checks for the correct hashes of these binary files, and you're all set!

## Don't Repeat Yourself (or Others)

#### Every piece of data must have a single authoritative representation in the system.

E.g., define the constant `pi` once in your program, and always use `pi` when you mean it.
Likewise, you should try to refactor your variables so that they lead down to a single source.

#### At small scales, code should be modularized rather than copied and pasted.

Modularized code can also be more easily repurposed for other projects.

#### At large scales, re-use code instead of rewriting it.

Use libraries for not-so-trivial problems outside of your comfort zone (e.g., 3D graphics).

## Plan for Mistakes

### Defensive Programming

#### Add assertions to programs to check their operation.

Assertions are great because they simplify debugging by making your program halt as soon as one of them gets triggered.
Even better, they serve as *executable documentation*, which by their nature *never become obsolete*.

#### Use automated testing.

At small scales (single functions, components) unit tests are handy.
At larger scales, employ *integration testing*: tests that check multiple components working together.
I personally recommend randomized stress testing, a la QuickCheck.
*Regression testing* is the practice of running pre-existing tests after changes to the code in order to make sure that nothing has broken.

Do not practice test-driven development (TDD), where you write tests before writing any new code, because a meta-study of its effectiveness done in 2010 did not find any significant impact on programmer productivity.[^tdd]

#### Use an off-the-shelf testing library.

Use of a testing library encourages us to write testable code, which generally means cleaner functions and less spaghetti code.
If you're working with a legacy code base without tests, a great way to refactor it is to make parts of it testable.

### Use a Variety of Oracles

An oracle is something which tells a developer how a program should behave or what its output should be.
Define generally expected program behavior and make sure your code conforms to it.
All of this can be done in an automated way with a good testing suite, but you can also manually write down a spec (probably as part of the documentation) and make sure your program performs as advertised, in all situations.

### Turn bugs into test cases

Write a test that will trigger the bug, and then fix your code to kill the bug.
This is an easy and simple way to write a regression test.

### Use a debugger

The more complex your code becomes, the more important it is to familiarize yourself with a debugger.

## Optimize Software Only After it Works Correctly

#### Use a profiler to identify bottlenecks accurately.

You should only care about performance *after* you get working code.

#### Write code in the highest-level language possible.

Research has confirmed that most programmers write roughly the same number of lines of code per unit time regardless of the language they use.[^hll]
Thus, you should try to write code in a high level language to save development time and increase productivity, and then fall back to a low level language to a speed boost where necessary.

## Document Design and Purpose, Not Mechanics

#### Document interfaces and resons, not implementations.

It is more useful to explain why code is written a certain way, instead of explaining the mechanics behind the implementation, for the code will speak for itself in the case of the latter.
If the code cannot speak for itself, then see the point below.

#### Refactor code instead of explaining how it works.

Reorganize complex code so that it doesn't even need an explanation.

Literate programming might be a good approach to fuse documentation and code together, but I personally have no experience with LP.

## Collaborate

#### Use pre-merge code reviews.

Code reviews are highly effective, but you should do it before the merge.
Once code gets merged, there is little incentive to review it, because hey, nothing's broken, right?

#### Use pair programming when bringing someone new up to speed and when tackling particularly tricky problems.

Pair programming can be intrusive, so take it with a grain of salt.

#### Use an issue tracking tool.

There are lots of free issue tracking tools available, not to mention sites like GitHub/Gitorious/BitBucket that integrate online collaboration, sharing, issue tracking, etc. all into one package.

[^naming]:In Haskell (static typing), people routinely name variables "a" or "b" with no fear of confusion, thanks to type signatures.
[^provenance]:*Provenance* is term from archaeology and forensics.
[^tdd]:Burak Turhan, Lucas Layman, Madeline Diep, Hakan Erdogmus, and Forrest Shull. *How Effective is Test-Driven Development?* In Andy Oram and Greg Wilson, editiors, *Making Software: What Really Works, and Why We Believe It*, pp. 207-217. O'Reilly, 2010.
[^hll]:See Lutz Prechelt. *Two Comparisons of Programming Languages.* In Andy Oram and Greg Wilson, editiors, *Making Software: What Really Works, and Why We Believe It*. O'Reilly, 2010.

## Conclusion

In my opinion, the holy trinity in software development is composed of (1) version control, (2) automated testing, and (3) documentation.
If you have these three, you are already golden and the rest is icing on the cake.

[paper]:http://arxiv.org/abs/1210.0530
