module Brick.Extras.Table
( Table(..)
, table
, renderTable
, renderTableWithStyle
-- * Event Handlers
, TableEvent(..)
, handleTableEvent
, handleTableEventArrowKeys
, handleTableEventVimKeys
-- * Table manipulation helpers
, Focus(..)
, getFocused
, modifyFocused
, getElement
, modifyElement
) where

import qualified Brick as Brick
import qualified Brick.Widgets.Border.Style as Brick
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
  , tableCurIndex :: ((Int, Int), (Int, Int))
    -- ^ The currently focused range of cells, of the form
    -- @((minX, minY), (maxX, maxY))@. Be aware that this module maintains
    -- the invariants that @minX <= minY && maxX <= maxY@, and that both
    -- the min and max indices are within the range of the table. If
    -- this value is modified without preserving that invariant, functions
    -- in this module may produce anomalous results or even crash.
  , tableDraw     :: e -> Bool -> Brick.Widget n
    -- ^ The function the table uses to draw its contents. The
    -- boolean parameter will be 'True' if the element is the
    -- currently focused element, and 'False' otherwise.
  , tableName     :: n
    -- ^ The name of the table
  }

type Idx = (Int, Int)
type Range = (Idx, Idx)

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
  , tableCurIndex = ((0, 0), (0, 0))
  , tableDraw     = draw
  , tableName     = name
  } where bounds = ((0, 0), (width-1, height-1))

data Direction = L | R | U | D

left, right, up, down :: (Int, Int) -> (Int, Int)
left (x, y) = (x - 1, y)
right (x, y) = (x + 1, y)
up (x, y) = (x, y - 1)
down (x, y) = (x, y + 1)

canMove :: Direction -> Idx -> Range -> Bool
canMove L (_, _) ((x, _), _) = x > 0
canMove R (m, _) (_, (x, _)) = x < m
canMove U (_, _) ((_, y), _) = y > 0
canMove D (_, m) (_, (_, y)) = y < m

onFst :: (a -> b) -> (a, c) -> (b, c)
onFst f (x, y) = (f x, y)

onSnd :: (a -> b) -> (c, a) -> (c, b)
onSnd f (x, y) = (x, f y)

onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (x, y) = (f x, f y)

-- | A representation of UI events which can change a table's
-- state.
data TableEvent
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | ExpandLeft
  | ExpandRight
  | ExpandUp
  | ExpandDown
  | ContractLeft
  | ContractRight
  | ContractUp
  | ContractDown
    deriving (Eq, Show)

applyEvent :: TableEvent -> Table e n -> Table e n
applyEvent ev tbl = case ev of
  MoveLeft      | canMove L mx idx -> go (onBoth left)
  MoveRight     | canMove R mx idx -> go (onBoth right)
  MoveUp        | canMove U mx idx -> go (onBoth up)
  MoveDown      | canMove D mx idx -> go (onBoth down)
  ExpandLeft    | canMove L mx idx -> go (onFst left)
  ExpandRight   | canMove R mx idx -> go (onSnd right)
  ExpandUp      | canMove U mx idx -> go (onFst up)
  ExpandDown    | canMove D mx idx -> go (onSnd down)
  ContractLeft  | lx < hx          -> go (onSnd left)
  ContractRight | lx < hx          -> go (onFst right)
  ContractUp    | ly < hy          -> go (onSnd up)
  ContractDown  | ly < hy          -> go (onFst down)
  _                                -> tbl
  where mx   = snd (A.bounds (tableContents tbl))
        idx  = tableCurIndex tbl
        go f = tbl { tableCurIndex = f idx }
        ((lx, ly), (hx, hy)) = idx

-- | Represents the current focus: either a single element or a
-- contiguous rectancle of focused cells from the table.
data Focus e
  = FocusElement (Int, Int) e
  | FocusRange (A.Array (Int, Int) e)
    deriving (Eq, Show)

-- | Extract the currently-focused element or elements from a table.
getFocused :: Table e n -> Focus e
getFocused Table
  { tableContents = cs
  , tableCurIndex = range@(lIdx, rIdx)
  } | lIdx == rIdx = FocusElement lIdx (cs A.! lIdx)
    | otherwise =
      FocusRange (A.array range [ (idx, cs A.! idx)
                                | idx <- Ix.range range
                                ])

-- | Apply a function to the entire focused region. This function
--   is passed the index of the cell as well as current value of
--   the cell.
modifyFocused :: Table e n -> ((Int, Int) -> e -> e) -> Table e n
modifyFocused tbl@Table
  { tableContents = cs
  , tableCurIndex = range
  } func = tbl { tableContents = cs A.// [ (idx, func idx (cs A.! idx))
                                         | idx <- Ix.range range
                                         ]
               }

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
modifyElement :: Table e n -> (Int, Int) -> ((Int, Int) -> e -> e)
              -> Table e n
modifyElement sp@Table { tableContents = cs } idx func
  | A.bounds cs `Ix.inRange` idx =
    sp { tableContents = cs A.// [(idx, func idx (cs A.! idx))] }
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
          Vty.EvKey (Vty.KChar 'k') [Vty.MShift] -> Just ExpandUp
          Vty.EvKey (Vty.KChar 'j') [Vty.MShift] -> Just ExpandDown
          Vty.EvKey (Vty.KChar 'h') [Vty.MShift] -> Just ExpandLeft
          Vty.EvKey (Vty.KChar 'l') [Vty.MShift] -> Just ExpandRight
          Vty.EvKey (Vty.KChar 'K') [] -> Just ExpandUp
          Vty.EvKey (Vty.KChar 'J') [] -> Just ExpandDown
          Vty.EvKey (Vty.KChar 'H') [] -> Just ExpandLeft
          Vty.EvKey (Vty.KChar 'L') [] -> Just ExpandRight
          Vty.EvKey (Vty.KChar 'k') [Vty.MMeta] -> Just ContractUp
          Vty.EvKey (Vty.KChar 'j') [Vty.MMeta] -> Just ContractDown
          Vty.EvKey (Vty.KChar 'h') [Vty.MMeta] -> Just ContractLeft
          Vty.EvKey (Vty.KChar 'l') [Vty.MMeta] -> Just ContractRight
          _                            -> Nothing

-- | Handle a 'TableEvent' event by modifying the state of the
-- table accordingly. This allows you to choose your own
-- keybindings for events you want to handle.
handleTableEvent :: TableEvent -> Table e n
                 -> Brick.EventM n (Table e n)
handleTableEvent e sp = return (applyEvent e sp)

-- | Render a table to a "brick" 'Widget' using the default border
-- style.
renderTable :: Bool -> Table e n -> Brick.Widget n
renderTable spFocus sp =
  renderTableWithStyle spFocus sp Brick.defaultBorderStyle

-- | Render a table to a "brick" 'Widget' using a custom border style.
renderTableWithStyle :: Bool -> Table e n -> Brick.BorderStyle
                     -> Brick.Widget n
renderTableWithStyle spFocus sp bs =
  let (_, (maxX, maxY)) = A.bounds (tableContents sp)
      vert = Brick.bsVertical bs
      horz = Brick.bsHorizontal bs
      horzDivider = Brick.vLimit 1 (Brick.fill horz)
      vertDivider t b i = Brick.vBox $ [Brick.str [t]] ++ ls ++ [Brick.str [b]]
        where ls = (take (2 * maxY + 1) (cycle [ Brick.str [vert]
                                               , Brick.str [i]
                                               ]))
      cols x = L.intersperse horzDivider
               [ Brick.padLeft Brick.Max $ tableDraw sp item isFocus
               | y <- [0..maxY]
               , let item = tableContents sp A.! (x, y)
                     isFocus = spFocus && (tableCurIndex sp `Ix.inRange` (x, y))
               ]
      rows = L.intersperse
               (vertDivider (Brick.bsIntersectT bs)
                            (Brick.bsIntersectB bs)
                            (Brick.bsIntersectFull bs))
               [ Brick.vBox $ [horzDivider] ++ cols x ++ [horzDivider]
               | x <- [0..maxX]
               ]
  in Brick.hBox $ [vertDivider (Brick.bsCornerTL bs)
                               (Brick.bsCornerBL bs)
                               (Brick.bsIntersectL bs)] ++
                  rows ++
                  [vertDivider (Brick.bsCornerTR bs)
                               (Brick.bsCornerBR bs)
                               (Brick.bsIntersectR bs)]
