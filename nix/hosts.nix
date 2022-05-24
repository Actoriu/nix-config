let
  inherit (builtins) attrNames concatMap listToAttrs;

  filterAttrs = pred: set:
    listToAttrs (concatMap (name: let value = set.${name}; in if pred name value then [{ inherit name value; }] else [ ]) (attrNames set));

  hosts = {
    d630 = {
      type = "nixos";
      localSystem = "x86_64-linux";
      address = "630";
    };
    actoriu = {
      type = "home-manager";
      localSystem = "x86_64-linux";
      address = "actoriu";
    };
  };
in
{
  all = hosts;

  nixos = rec {
    all = filterAttrs (_: v: v.type == "nixos") hosts;
    x86_64-linux = filterAttrs (_: v: v.localSystem == "x86_64-linux") all;
    aarch64-linux = filterAttrs (_: v: v.localSystem == "aarch64-linux") all;
  };

  homeManager = rec {
    all = filterAttrs (_: v: v.type == "home-manager") hosts;
    x86_64-linux = filterAttrs (_: v: v.localSystem == "x86_64-linux") all;
    aarch64-linux = filterAttrs (_: v: v.localSystem == "aarch64-linux") all;
  };
}
