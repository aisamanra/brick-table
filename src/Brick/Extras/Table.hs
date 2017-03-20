module Brick.Extras.Table
( Table(..)
, table
, handleTableEvent
, handleTableEventArrowKeys
, handleTableEventVimKeys
, renderTable
, getFocusedElement
, setFocusedElement
, getElement
, setElement
) where

import qualified Brick as Brick
import qualified Data.Array as A
import qualified Data.Ix as Ix
import qualified Data.List as L
import qualified Graphics.Vty as Vty

-- | Table state. A table is a two-dimensional array of
-- values as well as a cursor which is focuses on one of those values.
data Table e n = Table
  { tableContents :: A.Array (Int, Int) e
    -- ^ The array which represents the contents of the
    -- table. This always begins at index @(0,0)@.
  , tableCurIndex :: (Int, Int)
    -- ^ The currently focused index
  , tableDraw     :: e -> Bool -> Brick.Widget n
    -- ^ The function the table uses to draw its contents. The
    -- boolean parameter will be 'True' if the element is the
    -- currently focused element, and 'False' otherwise.
  , tableName     :: n
    -- ^ The name of the table
  }

-- | Create a new table state with a single default value for
-- all cells.
table :: n
            -- ^ The name of the table (must be unique)
            -> (Int, Int)
            -- ^ The @(width, height)@ of the desired table
            -> (e -> Bool -> Brick.Widget n)
            -- ^ The rendering function for contents of the
            -- table. The boolean parameter will be 'True' if
            -- the element is the currently focused element, and
            -- 'False' otherwise.
            -> e
            -- ^ The default element with which to fill the table
            -> Table e n
table name (width, height) draw def = Table
  { tableContents = A.listArray bounds (repeat def)
  , tableCurIndex = (0, 0)
  , tableDraw     = draw
  , tableName     = name
  } where bounds = ((0, 0), (width-1, height-1))

left, right, up, down :: (Int, Int) -> (Int, Int)
left (x, y) = (x - 1, y)
right (x, y) = (x + 1, y)
up (x, y) = (x, y - 1)
down (x, y) = (x, y + 1)

clamp :: (Int, Int) -> (Int, Int) -> (Int, Int)
clamp (x, y) (maxX, maxY) = (go x maxX, go y maxY)
  where go n maxN
          | n < 0     = 0
          | n > maxN  = maxN
          | otherwise = n

-- | A representation of UI events which can change a table's
-- state.
data TableEvent
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
    deriving (Eq, Show)

-- | Extract the currently-focused element from a table.
getFocusedElement :: Table e n -> e
getFocusedElement Table
  { tableContents = cs
  , tableCurIndex = idx
  } = cs A.! idx

-- | Modify the element currenly focused in the table.
setFocusedElement :: Table e n -> e -> Table e n
setFocusedElement sp@Table
  { tableContents = cs
  , tableCurIndex = idx
  } new = sp { tableContents = cs A.// [(idx, new)] }

-- | Extract an element at an arbitrary index from the
-- table. This will return 'Nothing' if the index is outside the
-- bounds of the table.
getElement :: Table e n -> (Int, Int) -> Maybe e
getElement Table { tableContents = cs } idx
  | A.bounds cs `Ix.inRange` idx = Just (cs A.! idx)
  | otherwise                    = Nothing

-- | Modify an element at an abitrary index in the table. This
-- will return the table unchanged if the index is outside the
-- bounds of the table.
setElement :: Table e n -> (Int, Int) -> e -> Table e n
setElement sp@Table { tableContents = cs } idx new
  | A.bounds cs `Ix.inRange` idx =
    sp { tableContents = cs A.// [(idx, new)] }
  | otherwise = sp

-- | Handle a "vty" event by moving the currently focused item in
-- response to the arrow keys.
handleTableEventArrowKeys :: Vty.Event -> Table e n
                                -> Brick.EventM n (Table e n)
handleTableEventArrowKeys ev sp = case spEvent of
  Just cmd -> handleTableEvent cmd sp
  Nothing  -> return sp
  where spEvent = case ev of
          Vty.EvKey Vty.KUp    [] -> Just MoveUp
          Vty.EvKey Vty.KDown  [] -> Just MoveDown
          Vty.EvKey Vty.KLeft  [] -> Just MoveLeft
          Vty.EvKey Vty.KRight [] -> Just MoveRight
          _                       -> Nothing

-- | Handle a "vty" event by moving the currently focused item in
-- response to the vim-style movement keys @h@, @j@, @k@, or @l@.
handleTableEventVimKeys :: Vty.Event -> Table e n
                              -> Brick.EventM n (Table e n)
handleTableEventVimKeys ev sp = case spEvent of
  Just cmd -> handleTableEvent cmd sp
  Nothing  -> return sp
  where spEvent = case ev of
          Vty.EvKey (Vty.KChar 'k') [] -> Just MoveUp
          Vty.EvKey (Vty.KChar 'j') [] -> Just MoveDown
          Vty.EvKey (Vty.KChar 'h') [] -> Just MoveLeft
          Vty.EvKey (Vty.KChar 'l') [] -> Just MoveRight
          _                            -> Nothing

-- | Handle a 'TableEvent' event by modifying the state of the
-- table accordingly. This allows you to choose your own
-- keybindings for events you want to handle.
handleTableEvent :: TableEvent -> Table e n
                       -> Brick.EventM n (Table e n)
handleTableEvent e sp =
  let (_, maxB) = A.bounds (tableContents sp)
      curIndex = tableCurIndex sp
      modify f = sp { tableCurIndex = clamp (f curIndex) maxB }
  in return $ case e of
    MoveUp    -> modify up
    MoveDown  -> modify down
    MoveLeft  -> modify left
    MoveRight -> modify right

-- | Render a table to a "brick" 'Widget'.
renderTable :: Bool -> Table e n -> Brick.Widget n
renderTable spFocus sp =
  let (_, (maxX, maxY)) = A.bounds (tableContents sp)
  in Brick.hBox $ L.intersperse (Brick.hLimit 1 (Brick.fill '│'))
       [ Brick.vBox $ L.intersperse (Brick.vLimit 1 (Brick.fill '─'))
         [ Brick.padLeft Brick.Max $ tableDraw sp item isFocus
         | y <- [0..maxY]
         , let item = tableContents sp A.! (x, y)
               isFocus = spFocus && ((x, y) == tableCurIndex sp)
         ]
       | x <- [0..maxX]
       ]
