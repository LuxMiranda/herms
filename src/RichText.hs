{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module RichText (
  Color(..), Text, RichText,
  (~~), bgColor, bold, fontColor, putText, putTextLn
) where

import Data.String
import System.Console.ANSI
import System.IO
import Debug.Trace

class Text t where
  toStr :: Bool -> t -> String

instance Text [Char] where
  toStr _ t = t

data RichText = forall t1 t2. (Text t1, Text t2) => Append t1 t2
              | forall t. Text t                 => BgColor Color t
              | forall t. Text t                 => Bold t
              | forall t. Text t                 => FontColor Color t
              | forall t. Text t                 => Plain t

instance IsString RichText where
  fromString = Plain

instance Text RichText where
  toStr formatted = toStr'
    where
      toStr' (Append t1 t2) = toStr formatted t1 ++ toStr formatted t2
      toStr' (BgColor color t) = wrap (SetColor Background Dull color) t
      toStr' (Bold t) = wrap (SetConsoleIntensity BoldIntensity) t
      toStr' (FontColor color t) = wrap (SetColor Foreground Vivid color) t
      toStr' (Plain t) = toStr formatted t
      wrap code t
        | formatted = setSGRCode [code] ++ s ++ setSGRCode [Reset]
        | otherwise = s
        where s = toStr formatted t

(~~) :: forall t1 t2. (Text t1, Text t2) => t1 -> t2 -> RichText
(~~) = Append

bgColor :: forall s. Text s => Color -> s -> RichText
bgColor = BgColor

bold :: forall s. Text s => s -> RichText
bold = Bold

fontColor :: forall s. Text s => Color -> s -> RichText
fontColor = FontColor

putText :: forall t. Text t => t -> IO ()
putText t = do
  -- We don't want to output ANSI codes if STDOUT is being redirected to a file,
  -- so we check if STDOUT is a terminal that supports ANSI.
  supports <- hSupportsANSI stdout
  putStr $ toStr supports t

putTextLn :: forall t. Text t => t -> IO ()
putTextLn t = putText t >> putStrLn ""
