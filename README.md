# `brick-table`

This is a package for creating and manipulating simple spreadsheed-like table widgets in [`brick`](https://hackage.haskell.org/package/brick-0.17). These tables come equipped with a notion of selection/focus on a set of cells, as well as a handful of functions for accessing and manipulating that subset.

A `table` in this case wraps an underlying `array` value that's indexed with an `(Int, Int)` pair. We can create a new `table` with default values using the `table` function, and include it in our application's `state`.

```.haskell
app :: App (Table TableElement MyName) MyEvent MyName
app = App
  { appDraw = \ s -> renderTable True s
  , appChooseCursor = \ _ _ -> Nothing
  , appHandleEvent = \ s e -> case e of
      VtyEvent (EvKey KEsc []) -> halt s
      _                        -> do
        s' <- handleTableEvenArrowKeys ev s
        continue s'
  , appStartEvent = \s -> return s
  , appAttrMap = \_ -> attrMap mempty []
  }

main :: IO ()
main = do
  let myTable = table myName (width, height) myDrawFunction myTableElement
  _ <- Brick.defaultMain app myTable
  return ()
```
