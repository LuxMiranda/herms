{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module AddCLI where

import Data.Monoid
import qualified Data.Text.Zipper as Z
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
import qualified Brick.Widgets.Core as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)
import ReadConfig (Translator)
import qualified Lang.Strings as Str

data Name = RecipeName
          | Description
          | ServingSize
          | IngrAmount
          | IngrUnit
          | IngrName
          | IngrAttr
          | Directions
          | Tags
          deriving (Ord, Show, Eq)

data FocusChange = FocusDown
                 | FocusUp
                 | FocusLeft
                 | FocusRight
                 deriving Eq

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
       , _edit9 :: E.Editor String Name
       }

makeLenses ''St

-- | This is a modification of the default renderEditor implementation
-- provided in Brick.Widgets.Edit
renderIngrEditor :: Int -> ([String] -> T.Widget Name) -> Bool -> E.Editor String Name -> T.Widget Name
renderIngrEditor row draw foc e =
    let z = e^.E.editContentsL
        (r,c) = Z.cursorPosition z
        cursorLoc = T.Location $
            if foc
               then (C.textWidth (take c (Z.currentLine z)), r)
               else (0, row)
        cursorWid = C.textWidth . (:[]) $ Z.currentChar z ^. non ' '
        contents = E.getEditContents e
    in C.withAttr (if foc then E.editFocusedAttr else E.editAttr) $
       C.viewport (E.editorName e) T.Both $
       (if foc then C.showCursor (E.editorName e) cursorLoc else id) $
       C.visibleRegion cursorLoc (cursorWid, 1) $
       draw $
       contents <> if foc then [] else replicate (row - length contents + 1) []

drawUI :: Translator -> St -> [T.Widget Name]
drawUI t st = [ui]
    where
        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
        e2 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit2)
        e3 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit3)
        ingrFocusRow = (^. non 0) . (^? each . _Just)
            $ F.withFocusRing (st^.focusRing) (\foc e ->
                if foc then Just (fst . Z.cursorPosition $ e^.E.editContentsL) else Nothing)
            <$> [st^.edit4, st^.edit5, st^.edit6, st^.edit7]
        e4 = F.withFocusRing (st^.focusRing) (renderIngrEditor ingrFocusRow (str . unlines)) (st^.edit4)
        e5 = F.withFocusRing (st^.focusRing) (renderIngrEditor ingrFocusRow (str . unlines)) (st^.edit5)
        e6 = F.withFocusRing (st^.focusRing) (renderIngrEditor ingrFocusRow (str . unlines)) (st^.edit6)
        e7 = F.withFocusRing (st^.focusRing) (renderIngrEditor ingrFocusRow (str . unlines)) (st^.edit7)
        e8 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit8)
        e9 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit9)

        ui = C.center $
            str (t Str.tuiTitle) <=>
            str " " <=>
            (str (t Str.tuiName) <+> hLimit 62 e1) <=>
            str " " <=>
            (str (t Str.tuiDesc) <+> hLimit 62 (vLimit 4 e2)) <=>
            str " " <=>
            (str (t Str.tuiServingSize) <+> hLimit 3 e3) <=>
            str " " <=>
            str (t Str.tuiHeaders) <=>
            (str (t Str.tuiIngrs)
              <+> hLimit 7 (vLimit 7 e4)
              <+> hLimit 9 (vLimit 7 e5)
              <+> hLimit 28 (vLimit 7 e6)
              <+> hLimit 18 (vLimit 7 e7))
              <=>
            str " " <=>
            (str (t Str.tuiDirs) <+> hLimit 62 (vLimit 8 e8)) <=>
            str " " <=>
            (str (t Str.tuiTags) <+> hLimit 62 (vLimit 1 e9)) <=>
            str " " <=>
            str (t Str.tuiHelp1) <=>
            str (t Str.tuiHelp2) <=>
            str (t Str.tuiHelp3) <=>
            str (t Str.tuiHelp4)

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev

        -- Ctrl + <Arrow Keys>
        V.EvKey V.KDown [V.MCtrl] ->
          M.continue $ st & focusRing %~ determineNextFocus FocusDown st
        V.EvKey V.KUp [V.MCtrl] ->
          M.continue $ st & focusRing %~ determineNextFocus FocusUp st
        V.EvKey V.KRight [V.MCtrl] ->
          M.continue $ st & focusRing %~ determineNextFocus FocusRight st
        V.EvKey V.KLeft [V.MCtrl] ->
          M.continue $ st & focusRing %~ determineNextFocus FocusLeft st

        -- Meta + <h-j-k-l>
        V.EvKey (V.KChar 'h') [V.MMeta] ->
          M.continue $ st & focusRing %~ determineNextFocus FocusLeft st
        V.EvKey (V.KChar 'j') [V.MMeta] ->
          M.continue $ st & focusRing %~ determineNextFocus FocusDown st
        V.EvKey (V.KChar 'k') [V.MMeta] ->
          M.continue $ st & focusRing %~ determineNextFocus FocusUp st
        V.EvKey (V.KChar 'l') [V.MMeta] ->
          M.continue $ st & focusRing %~ determineNextFocus FocusRight st

        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just RecipeName -> T.handleEventLensed st edit1 E.handleEditorEvent ev
               Just Description -> T.handleEventLensed st edit2 E.handleEditorEvent ev
               Just ServingSize -> T.handleEventLensed st edit3 E.handleEditorEvent ev
               Just IngrAmount -> T.handleEventLensed st edit4 E.handleEditorEvent ev
               Just IngrUnit -> T.handleEventLensed st edit5 E.handleEditorEvent ev
               Just IngrName -> T.handleEventLensed st edit6 E.handleEditorEvent ev
               Just IngrAttr -> T.handleEventLensed st edit7 E.handleEditorEvent ev
               Just Directions -> T.handleEventLensed st edit8 E.handleEditorEvent ev
               Just Tags -> T.handleEventLensed st edit9 E.handleEditorEvent ev
               Nothing -> return st
appEvent st _ = M.continue st

determineNextFocus :: FocusChange -> St -> F.FocusRing n -> F.FocusRing n
determineNextFocus action st =
  case action of
    FocusDown -> case currentFocus of
        Just RecipeName -> F.focusNext
        Just Description -> F.focusNext
        Just ServingSize -> F.focusNext
        Just IngrAmount -> F.focusNext . F.focusNext . F.focusNext . F.focusNext
        Just IngrUnit -> F.focusNext . F.focusNext . F.focusNext
        Just IngrName -> F.focusNext . F.focusNext
        Just IngrAttr -> F.focusNext
        Just Directions -> F.focusNext
        Just Tags -> F.focusNext
        Nothing -> F.focusNext
    FocusUp -> case currentFocus of
        Just RecipeName -> F.focusPrev
        Just Description -> F.focusPrev
        Just ServingSize -> F.focusPrev
        Just IngrAttr -> F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev
        Just IngrName -> F.focusPrev . F.focusPrev . F.focusPrev
        Just IngrUnit -> F.focusPrev . F.focusPrev
        Just IngrAmount -> F.focusPrev
        Just Directions -> F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev
        Just Tags -> F.focusPrev
        Nothing -> F.focusPrev
    FocusLeft -> case currentFocus of
        Just RecipeName -> F.focusNext . F.focusNext
        Just Description -> F.focusNext
        Just ServingSize -> F.focusNext
        Just IngrAmount -> F.focusNext . F.focusNext . F.focusNext
        Just IngrUnit -> F.focusPrev
        Just IngrName -> F.focusPrev
        Just IngrAttr -> F.focusPrev
        Just Directions -> F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev
        Just Tags -> F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev
        Nothing -> F.focusNext
    FocusRight -> case currentFocus of
        Just RecipeName -> F.focusNext . F.focusNext
        Just Description -> F.focusNext
        Just ServingSize -> F.focusNext
        Just IngrAmount -> F.focusNext
        Just IngrUnit -> F.focusNext
        Just IngrName -> F.focusNext
        Just IngrAttr -> F.focusPrev . F.focusPrev . F.focusPrev
        Just Directions -> F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev
        Just Tags -> F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev . F.focusPrev
        Nothing -> F.focusNext
  where currentFocus = F.focusGetCurrent $ st^.focusRing


initialState :: String -> String -> String -> String -> String -> String -> String -> String -> String -> St
initialState name desc serving amounts units ingrs attrs dirs tags =
    St (F.focusRing [RecipeName, Description, ServingSize, IngrAmount, IngrUnit, IngrName, IngrAttr, Directions, Tags])
       (E.editor RecipeName (Just 1) name)
       (E.editor Description Nothing desc)
       (E.editor ServingSize (Just 1) serving)
       (E.editor IngrAmount Nothing amounts)
       (E.editor IngrUnit Nothing units)
       (E.editor IngrName Nothing ingrs)
       (E.editor IngrAttr Nothing attrs)
       (E.editor Directions Nothing dirs)
       (E.editor Tags (Just 1) tags)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.black)
    , (E.editFocusedAttr,            V.black `on` V.white)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: Translator -> M.App St e Name
theApp t =
    M.App { M.appDraw = drawUI t
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

getEdit :: Translator -> String -> String -> String -> String -> String -> String -> String -> String -> String -> IO [[String]]
getEdit t name desc serving amounts units ingrs attrs dirs tags = do
  st <- M.defaultMain (theApp t) (initialState name desc serving amounts units ingrs attrs dirs tags)
  return [ E.getEditContents $ st^.edit1
             , E.getEditContents $ st^.edit2
             , E.getEditContents $ st^.edit3
             , E.getEditContents $ st^.edit4
             , E.getEditContents $ st^.edit5
             , E.getEditContents $ st^.edit6
             , E.getEditContents $ st^.edit7
             , E.getEditContents $ st^.edit8
             , E.getEditContents $ st^.edit9
             ]

getAddInput :: Translator -> IO [[String]]
getAddInput t = do
  st <- M.defaultMain (theApp t) (initialState "" "" "" "" "" "" "" "" "")
  return [ E.getEditContents $ st^.edit1
             , E.getEditContents $ st^.edit2
             , E.getEditContents $ st^.edit3
             , E.getEditContents $ st^.edit4
             , E.getEditContents $ st^.edit5
             , E.getEditContents $ st^.edit6
             , E.getEditContents $ st^.edit7
             , E.getEditContents $ st^.edit8
             , E.getEditContents $ st^.edit9
             ]
