---
title: Find Lines with 80+ Columns in Multiple Files
tags: linux
---

It's nice to follow the 80-columns-per-line rule[^lkcs], but what if you have some lingering project that has yet to conform?

Just go to that project's sources directory and do

```{.numberLines}
find . -type f -exec sh -c "expand -t 8 {} | grep -n \".\{81\}\"" \; -print
```

.

The call to `expand` is necessary because different people use different widths for tabs.
That is, we are trying to find files with 80+ *columns* (as they appear to humans), not 80+ *characters* (as they appear to the computer).

[^lkcs]: See the [Linux Kernel Coding Style](https://www.kernel.org/doc/Documentation/CodingStyle), "Chapter 2: Breaking long lines and strings".
