{
  inputs,
  lib,
  ...
} @ args: let
  mkLib = lib.makeExtensible (self: let
    callLibs = file: import file ({inherit self;} // args);
  in {
    attrs = callLibs ./attrs.nix;
    importers = callLibs ./importers.nix;
    options = callLibs ./options.nix;

    inherit (self.attrs) mergeAny;
    inherit (self.importers) flattenTree rakeLeaves buildModuleList;
    inherit (self.options) mkEnableOpt' mkOpt mkOptStr mkBoolOpt;
  });
in
  mkLib
  # mkLib.extend (self: super:
  #   lib.foldr (a: b: a // b) {} (lib.attrValues super))
