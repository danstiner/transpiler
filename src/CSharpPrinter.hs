module CSharpPrinter (printAsCSharp) where

import BatchParser

printAsCSharp :: Script -> IO ()
printAsCSharp s = preamble >> mapM_ printCommand s >> postamble
  where
    preamble = putStrLn "{"
    postamble = putStrLn "}"

printCommand :: Command -> IO ()
printCommand = p where
  p (EchoMessage str) = putStrLn $ "Echo(\"" ++ str ++ "\");"
  p (EchoEnabled v) = putStrLn $ "EchoEnable(" ++ showBool v ++ ");"
  p (Find str paths) = putStrLn $ "Find(" ++ str ++ ");"
  p (Goto str) = putStrLn $ "Goto(" ++ str ++ ");"
  p (GotoEof) = putStrLn $ "GotoEof();"
  p (If e c1 c2) =
    do putStrLn $ "if (" ++ showExpr e ++ ")"
       putStrLn "{"
       printCommand c1
       putStrLn "}"
       putStrLn "else"
       putStrLn "{"
       printCommand c2
       putStrLn "}"
  p (Label str) = putStrLn $ "Label(\"" ++ str ++ "\");"
  p Noop = return ()
  p (Pipe c1 c2) = putStrLn $ "Pipe();"
  p (Redirection c1 sink) = putStrLn $ "Redirect();"
  p (Quieted c) = putStrLn "{ Quiet();" >> printCommand c >> putStrLn "}"
  p (Rem str) = putStrLn $ "// " ++ str
  p (Rename f t) = putStrLn $ "" ++ f
  p (Ver) = putStrLn $ "Ver();"
  p (Verify v) = putStrLn $ "Verify(\"" ++ showBool v ++ "\");"
  p (ExternalCommand name args) = putStrLn $ "Run(\"" ++ name ++ "\", \"" ++ args ++ "\");"

showBool = show

showExpr = e where
  e (ErrorLevelExpr i) = "ErrorLevelEqOrAbove(" ++ show i ++ ")"
  e (EqualsExpr e1 e2) = "(" ++ showExpr e1 ++ ") == (" ++ showExpr e2 ++ ")"
  e (Exist path) = "Exists(\"" ++ path ++ "\")"
  e (FalseExpr) = "false"
  e (TrueExpr) = "true"
  e (NotExpr e) = "!(" ++ showExpr e ++ ")"
  e (StringExpr str) = "\"" ++ str ++ "\""
