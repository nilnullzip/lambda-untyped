module Test where

{-

Eval problems:

expected = "P((Lw.Ly.Lc.y(wyc))0)" -- good result
theLambda =  "P((Lw.Ly.Lx.y(wyx))0)" -- bad result

Simplified:

expected  = "Lx.(Lc.cf)(Lu.x)" -- good
theLambda = "Lx.(Lx.xf)(Lu.x)" -- bad

Still some problem with variable naming. Substituting c for x above works correctly.

-}

-- expected =  "(Lw.Ly.Lx.y(wyx))0"
-- expected =  "P((Lw.Ly.Lx.y(wyx))0)" -- bad result
-- theLambda = "P((Lw.Ly.Lc.y(wyc))0)" -- good result

--expected  = "Lf.Lx.(Ly.Lc.yc)(Lg.Lh.h(gf))(Lu.x)(Lu.u)" -- good result
--theLambda = "Lf.Lx.(Ly.Lx.yx)(Lg.Lh.h(gf))(Lu.x)(Lu.u)" -- bad result

--theLambda = "P1"
--expected  = "Lx.(Lx.(Lg.Lh.h(gf))x)(Lu.x)"
--theLambda = "Lx.(Lc.(Lg.Lh.h(gf))c)(Lu.x)"

--expected  = "Lx.(Lc.cf)(Lu.x)" -- good
--theLambda = "Lx.(Lx.xf)(Lu.x)" -- bad

-- expected =  "Lc.(Ls.Lx.s)(Lu.c)(Lu.u)"
-- theLambda = "Lx.(Ls.Lx.s)(Lu.x)(Lu.u)" -- fail

--expected = "Ly.(0y)"
--theLambda = "(Lw.Ly.(wy))0" -- fail

--expected = "1"
--theLambda = "S0"

--expected = "2"
--theLambda = "S(S0)"

--expected = "0"
--expected = "(Ln.Lf.Lx.n(Lg.Lh.h(gf))(Lu.x)(Lu.u))(Lf.Ls.fs)" -- hanging?

--expected =  "Lx.(Ls.Lz.sz)(Lg.Lh.g)a"
--theLambda = "Lx.(Ly.Lx.yx)(Lg.Lh.g)a"

--theLambda = "Lx.(Lx.(Lg.Lh.g)x)a"
--theLambda = "Lx.(Lz.(Lg.Lh.g)z)a"

--expected = "0"
--theLambda = "P(S0)" -- fail

--expected = "(Ln.Lf.Lc.n(Lg.Lh.h(gf))(Lu.c)(Lu.u))(Ls.Lz.sz)" -- good
--theLambda = "(Ln.Lf.Lc.n(Lg.Lh.h(gf))(Lu.c)(Lu.u))(Ly.Lx.yx)" -- good

--expected =  "Lx.(Lz.(Lg.Lh.g)z)(Lu.x)a" -- pass
--theLambda = "Lx.(Lx.(Lg.Lh.g)x)(Lu.x)a" -- fail

-- This one fails because we throw away closure association upon parsing lambda.
--expected  = "Lx.(Ly.yy)(Lu.x)"
--theLambda = "Lx.(Lx.xx)(Lu.x)" -- fail
