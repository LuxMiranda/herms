module Import.Format
  ( ImportFormat (..),
    readImportFormat,
  )
where

data ImportFormat
  = JSON
  | YAML
  deriving (Eq, Ord)

readImportFormat :: String -> Maybe ImportFormat
readImportFormat "json" = Just JSON
readImportFormat "yaml" = Just YAML
readImportFormat _ = Nothing
