import Batch.Parser
import CSharpPrinter

main :: IO ()
main = getContents >>= parseAndPrint where
  parseAndPrint = p . Batch.Parser.parse
  p parsed = case parsed of
    Left error -> print error
    Right script -> printAsCSharp script
