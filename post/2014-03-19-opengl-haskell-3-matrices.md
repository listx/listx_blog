---
title: "OpenGL from Haskell (#3: Matrices)"
tags: haskell, opengl
---

The following is my translation/adaptation of [tutorial #2](http://www.opengl-tutorial.org/beginners-tutorials/tutorial-3-matrices/) at [http://www.opengl-tutorial.org](http://www.opengl-tutorial.org).
My [last post](2014-03-15-opengl-from-haskell.html) was a translation of tutorial #2, which dealt with triangles --- this is the reason why this post's title is called "#3: Matrices".
The end result of this tutorial is a 3D triangle with 3 different colored vertices that are interpolated smoothly by OpenGL.

My version again uses the code from [https://github.com/YPares/Haskell-OpenGL3.1-Tutos](https://github.com/YPares/Haskell-OpenGL3.1-Tutos).
The `Data.Vec` import is for the [Vec](http://hackage.haskell.org/package/Vec) package.
Like my last post, my code here does does not use `Control.Applicative` puts everything, including the GLSL shaders directly into the code.
The `RankNTypes` and `TypeOperators` GHC extensions are only there to suppress warnings from using `ghc --make -Wall`; if you don't want to use these extensions, just remove the type signature for the `vec3` function near the bottom.

I have also removed the use of backticks for Haskell's infix notation (`` `...` ``).
It's not because I like using parentheses --- I just don't like using infix notation because it runs against the argument handling order of normal functions found everywhere else.

Also, I have fixed YPares's original `lookAt` function which is actually [broken as of commit 7a027b927d061fbd26138cb7357c40c4cacbc927](https://github.com/YPares/Haskell-OpenGL3.1-Tutos/commit/7a027b927d061fbd26138cb7357c40c4cacbc927); you will need my version if you wish to pursue the later tutorials that actually test the validity of this function, such as the keyboard/mouse input tutorial #6 from [http://www.opengl-tutorial.org](http://www.opengl-tutorial.org).

The code here is released into the Public Domain.

- i opengl/matrices.hs
