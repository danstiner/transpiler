import BatchParser
import CSharpPrinter

main :: IO ()
main = getContents >>= parseAndPrint where
  parseAndPrint = p . BatchParser.parse
  p parsed = case parsed of
    Left error -> print error
    Right script -> printAsCSharp script
