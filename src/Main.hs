import Batch.Parser
import Batch.Transformer
import CSharp.Transformer
import CSharp.Printer

main :: IO ()
main = getContents >>= parseAndPrint where
  parseAndPrint = p . Batch.Parser.parse'
  p parsed = case parsed of
    Left err -> print err
    Right program -> transformAndPrint program
  transformAndPrint = CSharp.Printer.printProject . CSharp.Transformer.to . Batch.Transformer.from
