{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Brick
import           Brick.Extras.Table
import qualified Graphics.Vty as Vty

app :: Brick.App (Table Int ()) () ()
app = Brick.App
  { Brick.appDraw         = \ s ->
      [ Brick.vBox [ renderTable True s
                   , Brick.str "Keybindings:"
                   , Brick.str "  move with {h,j,k,l}"
                   , Brick.str "  expand with {H,J,K,L}"
                   , Brick.str "  contract with M-{h,j,k,l}"
                   , Brick.str "  increment the currently focused cells with enter"
                   , Brick.str "  quit with q or ESC"
                   , Brick.str ("Current selection: " ++ show (getFocused s))
                   ]]
  , Brick.appChooseCursor = \_ _ -> Nothing
  , Brick.appHandleEvent  = \s e -> case e of
    Brick.VtyEvent (Vty.EvKey (Vty.KEsc) []) -> Brick.halt s
    Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') _) -> Brick.halt s
    Brick.VtyEvent (Vty.EvKey (Vty.KEnter) []) ->
      Brick.continue (modifyFocused s (\ _ x -> x + 1))
    Brick.VtyEvent (ev) -> do
      s' <- handleTableEventVimKeys ev s
      Brick.continue s'
    _                   -> Brick.continue s
  , Brick.appStartEvent   = return
  , Brick.appAttrMap      = \ _ ->
      Brick.attrMap (Vty.withForeColor Vty.defAttr Vty.white)
        [ ("selected", Vty.withStyle Vty.defAttr Vty.reverseVideo) ]
  }

drawElem :: Int -> Bool -> Brick.Widget n
drawElem n True  = Brick.withAttr "selected" $ Brick.str (show n)
drawElem n False = Brick.str (show n)

main :: IO ()
main = do
  let tb = table () (10, 10) drawElem 0
  _ <- Brick.defaultMain app tb
  return ()
