module Brick.Extras.Spreadsheet
( Spreadsheet(..)
, spreadsheet
, handleSpreadsheetEvent
, handleSpreadsheetEventArrowKeys
, handleSpreadsheetEventVimKeys
, renderSpreadsheet
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

-- | Spreadsheet state. A spreadsheet is a two-dimensional array of
-- values as well as a cursor which is focuses on one of those values.
data Spreadsheet e n = Spreadsheet
  { spreadsheetContents :: A.Array (Int, Int) e
    -- ^ The array which represents the contents of the
    -- spreadsheet. This always begins at index @(0,0)@.
  , spreadsheetCurIndex :: (Int, Int)
    -- ^ The currently focused index
  , spreadsheetDraw     :: e -> Bool -> Brick.Widget n
    -- ^ The function the spreadsheet uses to draw its contents. The
    -- boolean parameter will be 'True' if the element is the
    -- currently focused element, and 'False' otherwise.
  , spreadsheetName     :: n
    -- ^ The name of the spreadsheet
  }

-- | Create a new spreadsheet state with a single default value for
-- all cells.
spreadsheet :: n
            -- ^ The name of the spreadsheet (must be unique)
            -> (Int, Int)
            -- ^ The @(width, height)@ of the desired table
            -> (e -> Bool -> Brick.Widget n)
            -- ^ The rendering function for contents of the
            -- spreadsheet. The boolean parameter will be 'True' if
            -- the element is the currently focused element, and
            -- 'False' otherwise.
            -> e
            -- ^ The default element with which to fill the spreadsheet
            -> Spreadsheet e n
spreadsheet name (width, height) draw def = Spreadsheet
  { spreadsheetContents = A.listArray bounds (repeat def)
  , spreadsheetCurIndex = (0, 0)
  , spreadsheetDraw     = draw
  , spreadsheetName     = name
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

-- | A representation of UI events which can change a spreadsheet's
-- state.
data SpreadsheetEvent
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
    deriving (Eq, Show)

-- | Extract the currently-focused element from a spreadsheet.
getFocusedElement :: Spreadsheet e n -> e
getFocusedElement Spreadsheet
  { spreadsheetContents = cs
  , spreadsheetCurIndex = idx
  } = cs A.! idx

-- | Modify the element currenly focused in the spreadsheet.
setFocusedElement :: Spreadsheet e n -> e -> Spreadsheet e n
setFocusedElement sp@Spreadsheet
  { spreadsheetContents = cs
  , spreadsheetCurIndex = idx
  } new = sp { spreadsheetContents = cs A.// [(idx, new)] }

-- | Extract an element at an arbitrary index from the
-- spreadsheet. This will return 'Nothing' if the index is outside the
-- bounds of the spreadsheet.
getElement :: Spreadsheet e n -> (Int, Int) -> Maybe e
getElement Spreadsheet { spreadsheetContents = cs } idx
  | A.bounds cs `Ix.inRange` idx = Just (cs A.! idx)
  | otherwise                    = Nothing

-- | Modify an element at an abitrary index in the spreadsheet. This
-- will return the spreadsheet unchanged if the index is outside the
-- bounds of the spreadsheet.
setElement :: Spreadsheet e n -> (Int, Int) -> e -> Spreadsheet e n
setElement sp@Spreadsheet { spreadsheetContents = cs } idx new
  | A.bounds cs `Ix.inRange` idx =
    sp { spreadsheetContents = cs A.// [(idx, new)] }
  | otherwise = sp

-- | Handle a "vty" event by moving the currently focused item in
-- response to the arrow keys.
handleSpreadsheetEventArrowKeys :: Vty.Event -> Spreadsheet e n
                                -> Brick.EventM n (Spreadsheet e n)
handleSpreadsheetEventArrowKeys ev sp = case spEvent of
  Just cmd -> handleSpreadsheetEvent cmd sp
  Nothing  -> return sp
  where spEvent = case ev of
          Vty.EvKey Vty.KUp    [] -> Just MoveUp
          Vty.EvKey Vty.KDown  [] -> Just MoveDown
          Vty.EvKey Vty.KLeft  [] -> Just MoveLeft
          Vty.EvKey Vty.KRight [] -> Just MoveRight
          _                       -> Nothing

-- | Handle a "vty" event by moving the currently focused item in
-- response to the vim-style movement keys @h@, @j@, @k@, or @l@.
handleSpreadsheetEventVimKeys :: Vty.Event -> Spreadsheet e n
                              -> Brick.EventM n (Spreadsheet e n)
handleSpreadsheetEventVimKeys ev sp = case spEvent of
  Just cmd -> handleSpreadsheetEvent cmd sp
  Nothing  -> return sp
  where spEvent = case ev of
          Vty.EvKey (Vty.KChar 'k') [] -> Just MoveUp
          Vty.EvKey (Vty.KChar 'j') [] -> Just MoveDown
          Vty.EvKey (Vty.KChar 'h') [] -> Just MoveLeft
          Vty.EvKey (Vty.KChar 'l') [] -> Just MoveRight
          _                            -> Nothing

-- | Handle a 'SpreadsheetEvent' event by modifying the state of the
-- spreadsheet accordingly. This allows you to choose your own
-- keybindings for events you want to handle.
handleSpreadsheetEvent :: SpreadsheetEvent -> Spreadsheet e n
                       -> Brick.EventM n (Spreadsheet e n)
handleSpreadsheetEvent e sp =
  let (_, maxB) = A.bounds (spreadsheetContents sp)
      curIndex = spreadsheetCurIndex sp
      modify f = sp { spreadsheetCurIndex = clamp (f curIndex) maxB }
  in return $ case e of
    MoveUp    -> modify up
    MoveDown  -> modify down
    MoveLeft  -> modify left
    MoveRight -> modify right

-- | Render a spreadsheet to a "brick" 'Widget'.
renderSpreadsheet :: Bool -> Spreadsheet e n -> Brick.Widget n
renderSpreadsheet spFocus sp =
  let (_, (maxX, maxY)) = A.bounds (spreadsheetContents sp)
  in Brick.hBox $ L.intersperse (Brick.hLimit 1 (Brick.fill '│'))
       [ Brick.vBox $ L.intersperse (Brick.vLimit 1 (Brick.fill '─'))
         [ Brick.padLeft Brick.Max $ spreadsheetDraw sp item isFocus
         | y <- [0..maxY]
         , let item = spreadsheetContents sp A.! (x, y)
               isFocus = spFocus && ((x, y) == spreadsheetCurIndex sp)
         ]
       | x <- [0..maxX]
       ]
