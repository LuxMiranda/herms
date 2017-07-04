{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module AddCLI where

import Control.Monad
import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

data Name = RecipeName
          | Description
          | IngrAmount
          | IngrUnit
          | IngrName
          | IngrAttr
          | Directions
          | Tags
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       , _edit2 :: E.Editor String Name
       , _edit3 :: E.Editor String Name
       , _edit4 :: E.Editor String Name
       , _edit5 :: E.Editor String Name
       , _edit6 :: E.Editor String Name
       , _edit7 :: E.Editor String Name
       , _edit8 :: E.Editor String Name
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
        e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)
        e3 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit3)
        e4 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit4)
        e5 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit5)
        e6 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit6)
        e7 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit7)
        e8 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit8)

        ui = C.center $
            str "                                    Herm's - Add a recipe" <=>
            str " " <=>
            (str "          Name: " <+> (hLimit 62 e1)) <=>
            str " " <=>
            (str "   Description: " <+> (hLimit 62 $ vLimit 5 e2)) <=>
            str " " <=>
            str "                  qty.   unit               name                attribute" <=>
            (str "  Ingredients: \n(one per line)  " 
              <+> (hLimit 7 $ vLimit 8 e3) 
              <+> (hLimit 9 $ vLimit 8 e4)
              <+> (hLimit 28 $ vLimit 8 e5) 
              <+> (hLimit 18 $ vLimit 8 e6)) 
              <=> 
            str " " <=>
            (str "   Directions: \n(one per line)  " <+> (hLimit 62 $ vLimit 8 e7)) <=>
            str " " <=>
            (str "          Tags: " <+> (hLimit 62 $ vLimit 1 e8)) <=>
            str " " <=>
            str "                       Tab / Shift+Tab      - Next / Previous field" <=>
            str "                       Ctrl + <Arrow keys>  - Navigate fields" <=>
       --   str "                       Ctrl + <h-j-k-l>     - Navigate fields" <=> TODO
            str "                       Esc                  - Save or Cancel"

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
        V.EvKey V.KDown [V.MCtrl] -> M.continue $ st & focusRing %~ (case F.focusGetCurrent (st^.focusRing) of
               Just RecipeName -> (F.focusNext)
               Just Description -> (F.focusNext)
               Just IngrAmount -> (F.focusNext . F.focusNext . F.focusNext . F.focusNext)
               Just IngrUnit -> (F.focusNext . F.focusNext . F.focusNext)
               Just IngrName -> (F.focusNext . F.focusNext)
               Just IngrAttr -> (F.focusNext)
               Just Directions -> (F.focusNext)
               Just Tags -> (F.focusNext)
               Nothing -> (F.focusNext))
        V.EvKey V.KUp [V.MCtrl] -> M.continue $ st & focusRing %~ (case F.focusGetCurrent (st^.focusRing) of
               Just RecipeName -> (F.focusPrev)
               Just Description -> (F.focusPrev)
               Just IngrAttr -> (F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev)
               Just IngrName -> (F.focusPrev . F.focusPrev . F.focusPrev)
               Just IngrUnit -> (F.focusPrev . F.focusPrev)
               Just IngrAmount -> (F.focusPrev)
               Just Directions -> (F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev)
               Just Tags -> (F.focusPrev)
               Nothing -> (F.focusPrev))
        V.EvKey V.KRight [V.MCtrl] -> M.continue $ st & focusRing %~ (case F.focusGetCurrent (st^.focusRing) of
               Just RecipeName -> (F.focusNext . F.focusNext)
               Just Description -> (F.focusNext)
               Just IngrAmount -> (F.focusNext)
               Just IngrUnit -> (F.focusNext)
               Just IngrName -> (F.focusNext)
               Just IngrAttr -> (F.focusPrev . F.focusPrev . F.focusPrev)
               Just Directions -> (F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev)
               Just Tags -> (F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev)
               Nothing -> (F.focusNext))
        V.EvKey V.KLeft [V.MCtrl] -> M.continue $ st & focusRing %~ (case F.focusGetCurrent (st^.focusRing) of
               Just RecipeName -> (F.focusNext . F.focusNext)
               Just Description -> (F.focusNext)
               Just IngrAmount -> (F.focusNext . F.focusNext . F.focusNext)
               Just IngrUnit -> (F.focusPrev)
               Just IngrName -> (F.focusPrev)
               Just IngrAttr -> (F.focusPrev)
               Just Directions -> (F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev)
               Just Tags -> (F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev)
               Nothing -> (F.focusNext))
 
        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just RecipeName -> T.handleEventLensed st edit1 E.handleEditorEvent ev
               Just Description -> T.handleEventLensed st edit2 E.handleEditorEvent ev
               Just IngrAmount -> T.handleEventLensed st edit3 E.handleEditorEvent ev
               Just IngrUnit -> T.handleEventLensed st edit4 E.handleEditorEvent ev
               Just IngrName -> T.handleEventLensed st edit5 E.handleEditorEvent ev
               Just IngrAttr -> T.handleEventLensed st edit6 E.handleEditorEvent ev
               Just Directions -> T.handleEventLensed st edit7 E.handleEditorEvent ev
               Just Tags -> T.handleEventLensed st edit8 E.handleEditorEvent ev
               Nothing -> return st
appEvent st _ = M.continue st

initialState :: St
initialState =
    St (F.focusRing [RecipeName, Description, IngrAmount, IngrUnit, IngrName, IngrAttr, Directions, Tags])
       (E.editor RecipeName (Just 1) "")
       (E.editor Description Nothing "")
       (E.editor IngrAmount Nothing "")
       (E.editor IngrUnit Nothing "")
       (E.editor IngrName Nothing "")
       (E.editor IngrAttr Nothing "")
       (E.editor Directions Nothing "")
       (E.editor Tags (Just 1) "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.black)
    , (E.editFocusedAttr,            V.black `on` V.white)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

getAddInput :: IO ([[String]])
getAddInput = do 
  st <- M.defaultMain theApp initialState     
  return $ [ E.getEditContents $ st^.edit1
             , E.getEditContents $ st^.edit2
             , E.getEditContents $ st^.edit3
             , E.getEditContents $ st^.edit4
             , E.getEditContents $ st^.edit5
             , E.getEditContents $ st^.edit6
             , E.getEditContents $ st^.edit7
             , E.getEditContents $ st^.edit8 
             ] 
