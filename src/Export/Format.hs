module Export.Format
  ( ExportFormat (..),
    readExportFormat,
  )
where

data ExportFormat
  = JSON
  | YAML
  | HTML
  | Org
  deriving (Eq, Ord)

readExportFormat :: String -> Maybe ExportFormat
readExportFormat "json" = Just JSON
readExportFormat "yaml" = Just YAML
readExportFormat "html" = Just HTML
readExportFormat "org" = Just Org
readExportFormat _ = Nothing
