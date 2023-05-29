{
  inputs,
  lib,
  ...
}: let
  flattenTree = tree: let
    mkNewPrefix = prefix: name: "${
      if prefix == ""
      then ""
      else "${prefix}/"
    }${name}";

    flattenTree' = prefix: remain:
      if lib.isAttrs remain
      then lib.flatten (lib.mapAttrsToList (name: value: flattenTree' (mkNewPrefix prefix name) value) remain)
      else [(lib.nameValuePair prefix remain)];
  in
    lib.listToAttrs (flattenTree' "" tree);

  rakeLeaves = src: let
    haumea = inputs.haumea.lib;
    loader = lib.const lib.id;
    transformer = _cursor: dir:
      if dir ? default
      then assert (lib.attrNames dir == ["default"]); dir.default
      else dir;
  in
    haumea.load {inherit src loader transformer;};
in {
  inherit flattenTree rakeLeaves;

  buildModuleList = dir: lib.attrValues (flattenTree (rakeLeaves dir));
}
