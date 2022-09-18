main = do
  writeFile "foo.txt" $
    unlines
      [ "Hello",
        "World"
      ]
