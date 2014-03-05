module Test where

import           AST
import           Data.IORef
import           Debug.Trace
import           Parse
import           Reduce

foreach = flip mapM_

tests = [
    ("Lx.(Ls.Lx.s)(Lu.x)(Lu.u)", "Lc.(Ls.Lx.s)(Lu.c)(Lu.u)"),
    ("(Lx.xx)Ly.y", "Ly.y"),
    ("S0", "1"),
    ("S(S0)", "2"),
    ("P((Lw.Ly.Lx.y(wyx))0)", "P((Lw.Ly.Lc.y(wyc))0)"),
    ("Lf.Lx.(Ly.Lx.yx)(Lg.Lh.h(gf))(Lu.x)(Lu.u)", "Lf.Lx.(Ly.Lc.yc)(Lg.Lh.h(gf))(Lu.x)(Lu.u)"),
    ("Lx.(Ls.Lx.s)(Lu.x)(Lu.u)", "Lc.(Ls.Lx.s)(Lu.c)(Lu.u)"),
    ("(Lw.Ly.(wy))0", "Ly.(0y)"),
    ("Lx.(Ly.Lx.yx)(Lg.Lh.g)a", "Lx.(Ls.Lz.sz)(Lg.Lh.g)a"),
    ("Lx.(Lx.(Lg.Lh.g)x)a", "Lx.(Lz.(Lg.Lh.g)z)a"),
    ("(Ln.Lf.Lc.n(Lg.Lh.h(gf))(Lu.c)(Lu.u))(Ly.Lx.yx)", "(Ln.Lf.Lc.n(Lg.Lh.h(gf))(Lu.c)(Lu.u))(Ls.Lz.sz)"),
    ("Lx.(Lx.(Lg.Lh.g)x)(Lu.x)a", "Lx.(Lz.(Lg.Lh.g)z)(Lu.x)a"),
    ("Lx.(Lx.xx)(Lu.x)", "Lx.(Ly.yy)(Lu.x)"),
    ("Lx.(Lx.xf)(Lu.x)", "Lx.(Lc.cf)(Lu.x)"),
    ("Lx.(Lc.(Lg.Lh.h(gf))c)(Lu.x)", "Lx.(Lx.(Lg.Lh.h(gf))x)(Lu.x)"),
    ("P(S0)", "0"),
    ("P0", "0"),
    ("(Ln.Lf.Lx.n(Lg.Lh.h(gf))(Lu.x)(Lu.u))(Lf.Ls.fs)", "0"), -- hanging?
    ("2S1","3"),
    ("*22","2S2"),
    ("x","x")
    ]

runtest :: (String, String) -> IO()

runtest t = let
        got = reduce (parse (fst t)) []
        expected = reduce (parse(snd t)) []
        pgot = pdbi got
        pexpected = pdbi expected
    in do
        putStrLn ("")
        putStrLn ("testing:   " ++ (fst t))
        if pgot == pexpected then
            putStrLn $ "Passed:    " ++ pgot
        else
            do
                putStrLn ("expecting: " ++ (snd t))
                putStrLn ("testing AST:   " ++ show (parse (fst t)))
                putStrLn ("expecting AST: " ++ show (parse (snd t)))
                putStrLn ("got:       " ++ show got);
                putStrLn ("expected:  " ++ show expected)
                putStrLn ("got:       " ++ pgot);
                putStrLn ("expected:  " ++ pexpected)

runtests =
    do
        putStrLn ("")
        putStrLn ("Running tests")
        foreach tests runtest

