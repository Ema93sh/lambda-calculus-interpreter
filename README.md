Lambda Calculus REPL:
====================

A simple REPL for lambda calculus.


## Screencast

![Screencapture GIF](screencast.gif)


## Run

To run the REPL, just type in:

    cabal run


## Examples:

To switch normal order reduction strategy
:n
(λx.xx)((λxy.yx)y)
→(λx.xx)((λxy.yx)y)
→((λxy.yx)y)((λxy.yx)y)
→(λa.ay)((λxy.yx)y)
→((λxy.yx)y)y
→(λa.ay)y
yy

:a
(λx.xx)((λxy.yx)y)
→(\x.xx)((λxy.yx)y)
→(λx.xx)(λa.ay)
yy

-- Alpha Equivalence
:eq (\x.xx) (\y.yy)
:eq (\xy.xy) (\yx.yx)
:eq (\xy.xy) (\yx.yy)


-- Convert to Debruijn Index
:d (\xysz.xs(ysz))
:d (\x.x(\y.xx))

-- Explain
(λx.(λy.xy))y (\x.xx)


TODO
:eq y x not equal
