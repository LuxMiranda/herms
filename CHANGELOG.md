# Changelog



### Unreleased

* HTML export
* Catalan language translation


### v2.0.1 - 2020-10-17

Bugfixes to the release process.

### v2.0.0 - 2020-10-17

Many changes!

* GHC support:
  * Dropped support for GHC 8.2
  * Added support for GHC 8.8
* Config files now stored in XDG directories
* Added Nix build and shell configurations
* Added YAML/JSON recipe format, with import and export functionality
* Added `shopping` command for generating shopping lists
* Added `find` command for searching through recipes
* Added `--convert` option to `edit` command
* Added a test suite!
* New translations:
  * German
  * Spanish
  * Portuguese

### v1.8.1.4 - 2017-03-11

* The meta update! Added version flag ("-v" and "--version")

### v1.8.1.3 - 2017-03-11

* Tags may now contain multiple words. If herms detects commas in the
  tag entry field, then a recipe's tags will be comma-separated strings
  rather than space-separated words.

### v1.8.1.2 - 2017-10-18

* Initial release
