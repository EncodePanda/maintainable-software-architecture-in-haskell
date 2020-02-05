module FM.Fm where

-- | take Int, return +1 as text
doStuff :: Int -> String
doStuff i = "New value: " ++ (show $ i + 1)
