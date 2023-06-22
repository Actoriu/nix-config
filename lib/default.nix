{lib, ...} @ args: let
  mkLib = self: let
    callLibs = file: import file ({inherit self;} // args);
  in {
    importers = callLibs ./importers.nix;

    inherit (self.importers) buildModuleList flattenTree rakeLeaves;
  };
in
  lib.makeExtensible mkLib
