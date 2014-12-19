import Batch.Parser
import Batch.Transformer
import CSharp.Printer
import CSharp.Transformer

main :: IO ()
main =
    getContents >>= either print transformAndPrint . Batch.Parser.parse
  where
    transformAndPrint = CSharp.Printer.printProject . CSharp.Transformer.to . Batch.Transformer.from
