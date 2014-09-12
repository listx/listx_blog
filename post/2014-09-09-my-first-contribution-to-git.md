---
title: My First Contribution to Git
tags: programming, git
---

I am happy to say that my [first][first] [patches][patches] were [recently merged][merged] into the master Git branch!
Now, I am by no means a C or Git expert, so the patches only dealt with changes to the documentation.
I did not know anyone involved with Git in real life; nor did I have any experience submitting patches to a big project like Git.
So, how did I do it?

## Dead Simple Fix

It all started when I typed in `man git init` and found this line:

```
The template directory used will (in order):
```

. Obviously, there was a missing word "be" in that sentence.
I could not believe that such a glaring error was present in such a high profile project like Git.
Then again, if you think about it, you only invoke `git init` once per project (to initialize a Git repository), so the vast majority of users have but a cursory glance at the manpage --- that is, if they even bother at all to consult the manpages for such a basic task.

Anyway, my initial reaction was "Aha! I will fix this typo and submit my first patch to Git!".
I thought fixing something so simple and obvious would surely have no trouble getting into the main branch.
But then, something stirred inside me, and compelled me to look for similar errors in the manpage.
Judging by the pace of things, my next patch to Git might take one, two, five years (or forever?), and the thought of becoming a one-shot, one-commit, one-word-fix contributor made my ego twitch.
And besides, it felt a bit stupid to just submit a two-letter ("b" and "e") fix.
I thought to myself, "Maybe there are other low-hanging fruits to pluck?"

## Real Work

So, I set a medium-term goal to myself: read the entire `git init` manpage, with a singular focus on clarity.
I spotted more typographical errors, and even some confusing parts that a Git newbie would have trouble with.
`git init`, after all, is a very basic, fundamental operation that all newcomers to Git would probably have questions about.
It might even be the first thing that newbies might look up in the docs!

First things first.
I cloned the [master Git repository](https://github.com/git/git) from GitHub.
I then created a new branch to record my work; in the end, I created 7 commits.
I read Git's documentation on how to submit patches, and also consulted contribution advice for the Linux Kernel project in the form of [Greg Kroah-Hartman's excellent talk](http://youtu.be/LLBrBBImJt4).
The Linux Kernel is where Git's real action is (tens of thousands of contributors, all using Git!), so it made sense to check out the Kernel's way of doing things.

I used `git rebase -i` countless times until I got everything sorted out.
I even reordered the commits to have the [most basic one](https://github.com/git/git/commit/6e1ccacbedf084971f095816f4450c4b607607c5) (all typographical changes) be the first one, in case they wanted to merge just a few of them and reject the rest.

After re-reading everything for the eighth time, I sent an email to the official mailing list with `git send-email`.
I checked `git blame` for the lines I touched to CC everyone that could be directly affected by my changes.
After a few days, I got a response back --- from the principal maintainer of Git, Junio Hamano, no less!

## The Response?

He was very polite, and true to his duty as maintainer, very thorough.
You can see the discussion that took place over at the [mailing list archive](http://thread.gmane.org/gmane.comp.version-control.git/254705).
I sent off my second version of my 7-commit patch series after a few days, and waited.

And waited.
I waited two weeks, but alas, my new patch series was not getting any further feedback.
I was concerned that my changes were simply forgotten.
A nagging thought annoyed me --- "What if I miss the current merge window[^merge-window]?"
In mild desperation, I sent an email directly to Junio to remind him about my patches.
Still no response.
A couple more weeks passed, and I almost forgot about my patches...

Until today!
I got an email from `tip4commit.com` telling me that I was given some miniscule fraction of a bitcoin for my contribution to the Git project.
It felt like some strange sort of scam, but even if it was, whoever wrote that automated emailing script did their homework on the Github API because lo and behold, my patches were indeed merged into the mainline!

## Conclusion

Well, as they say, if you don't know how to code but still want to contribute to an open source project, you should work on the documentation!
I assume that I did not get any feedback after the initial code review because the maintainer, Junio, was busy with other matters.
If I think about it now, he does get tons of patches and needs to do countless merges, containing countless commits, to make a new release of Git.

So, when contributing to open source, try working on the documentation first.
Improving the documentation not only benefits all newcomers to the project, but also helps you understand the project better --- a win win!
And, be patient with your patches --- especially if the project you are contributing to has a busy schedule.

Happy hacking!

[first]: http://thread.gmane.org/gmane.comp.version-control.git/254705
[patches]: http://thread.gmane.org/gmane.comp.version-control.git/255050
[merged]: https://github.com/git/git/commit/4645b014c5c82a3b75337
[^merge-window]: This is the period of time when the maintainer of a project accepts patches for the next release.
