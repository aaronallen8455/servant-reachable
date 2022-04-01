import           Test.DocTest

main :: IO ()
main = doctest ["-isrc", "test/Test.hs"]
