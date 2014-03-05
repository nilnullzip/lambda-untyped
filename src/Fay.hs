module Hello where

import FFI
import AST
import Parse
import Reduce


alert :: String -> Fay ()
alert = ffi "alert(%1)"

setBodyHtml :: String -> Fay ()
setBodyHtml = ffi "document.body.innerHTML = %1"

--addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
--addWindowEvent = ffi "window.addEventListener(%1, %2)"

--greet :: Event -> Fay()
--greet event = do
--  putStrLn "The document has loaded"
--  setBodyHtml "Hello HTML!"

main :: Fay ()
main = do
  putStrLn "Hello Console!"
  --putStrLn (show (parse "x"))
  putStrLn (pdbi(reduce (parse "Y(Lr.Ln.Zn0(n+(r(-n))))3") []))

  --alert "Hello Alert!"
  --addWindowEvent "load" greet
