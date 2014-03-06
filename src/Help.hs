module Help where

cr = "\n"

help =
      "type '?' for help"
    ++"otherwise type in a lambda expression."++cr
    ++cr
    ++"Input format: Lf.Lx.fx"++cr
    ++cr
    ++"Variables names are single character."++cr
    ++cr
    ++"Literals:"++cr
    ++"  numbers:    0 1 2 3 4 5 6 7 8 9"++cr
    ++"  arithmetic: + - * ^"++cr
    ++"  inequality: Z > < ="++cr
    ++"  logical:    T F & | ~"++cr
    ++"  pairs:      C A D"++cr
    ++"  recursion:  Yf = f(Yf)"++cr
    ++"  identity:   If = f"++cr
    ++cr
    ++"Examples:"++cr
    ++"  Add two numbers '1+2'"++cr
    ++"  Multiply two numbers '*23'"++cr 
    ++"  Sum the first three integers: 'Y(Lr.Ln.Zn0(n+(r(-n))))3'"++cr
    ++cr
    ++"Results are printed in De Bruijn format: L.L.21"++cr
    ++cr
    ++"Random:"++cr
    ++"  type '?t' to run regression tests"++cr
