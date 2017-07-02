{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

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
            str "                              Herm's - Add a recipe" <=>
            str " " <=>
            (str "          Name: " <+> (hLimit 50 e1)) <=>
            str " " <=>
            (str "   Description: " <+> (hLimit 50 $ vLimit 5 e2)) <=>
            str " " <=>
            str "                qty.   unit           name            attribute   " <=>
            (str "  Ingredients: \n(one per line)  " 
              <+> (hLimit 5 $ vLimit 5 e3) 
              <+> (hLimit 8 $ vLimit 5 e4) 
              <+> (hLimit 22 $ vLimit 5 e5) 
              <+> (hLimit 15 $ vLimit 5 e6)) 
              <=> 
            str " " <=>
            (str "   Directions: \n(one per line)  " <+> (hLimit 50 $ vLimit 5 e7)) <=>
            str " " <=>
            (str "          Tags: " <+> (hLimit 50  $ vLimit 1 e8)) <=>
            str " " <=>
            str "            Press Tab to switch between fields, Esc to save or cancel"

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
        V.EvKey V.KDown [] -> M.continue $ st & focusRing %~ (F.focusNext . F.focusNext)
                -- TODO context-specific mapping to down key such that when it is in
                --      any cell but an Ingredient cell, it simply goes down once,
                --      but if it is in an ingredient cell it ought to move to directions.
                --      Mirror for the up key, allow left-right keys for moving betweeen 
                --      ingredient cells, and repeat for h-j-k-l

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

main :: IO ()
main = do
    st <- M.defaultMain theApp initialState
    putStrLn "In input 1 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit1
    putStrLn "In input 2 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit2
    putStrLn "In input 3 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit3
    putStrLn "In input 4 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit4
    putStrLn "In input 5 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit5
    putStrLn "In input 6 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit6
    putStrLn "In input 7 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit7
    putStrLn "In input 8 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit8
