import BatchParser as Batch

main :: IO ()
main = getContents >>= print . Batch.parse
