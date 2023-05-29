{lib}: let
  version = lib.fileContents ../.version;
in
  version
