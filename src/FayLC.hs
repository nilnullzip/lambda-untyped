module FayLC where

import FFI
import AST
import Parse
import Reduce

alert :: String -> Fay ()
alert = ffi "alert(%1)"

setBodyHtml :: String -> Fay ()
setBodyHtml = ffi "document.body.innerHTML = %1"

data Event
addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

greet :: Event -> Fay()
greet event = do
  putStrLn ("The document has loaded")
  setBodyHtml "Lambda Calculus enabled by Juan!"

main :: Fay ()
main = do
  putStrLn "Lambda Calculus enabled by Juan!"
  putStrLn (pdbi(reduce (parse "Y(Lr.Ln.Zn0(n+(r(-n))))3") [])) -- demo prints on console
  putStrLn "Try this: Strict.FayLC.eval('TF'.split('')).join('')"
  --alert "Lambda Calculus enabled by Juan!"
  --addWindowEvent "load" greet

-- helpful string to string for JS

eval s = pdbi (reduce (parse s) [])
