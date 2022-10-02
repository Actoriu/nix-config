{ ... }:

let
  hosts = {
    oneplus5 = {
      type = "droid";
      hostPlatform = "aarch64-linux";
    };
    actoriu = {
      type = "home-manager";
      hostPlatform = "x86_64-linux";
    };
    d630 = {
      type = "nixos";
      hostPlatform = "x86_64-linux";
    };
  };

  inherit (builtins) attrNames concatMap listToAttrs;

  filterAttrs = pred: set:
    listToAttrs (concatMap
      (name:
        let value = set.${name};
        in
        if pred name value then
          [{ inherit name value; }]
        else [ ])
      (attrNames set));

  systemPred = system: (_: v: builtins.match ".*${system}.*" v.hostPlatform != null);

  genFamily = filter: hosts: rec {
    all = filterAttrs filter hosts;

    nixos = genFamily (_: v: v.type == "nixos") all;
    nix-darwin = genFamily (_: v: v.type == "darwin") all;
    nix-droid = genFamily (_: v: v.type == "droid") all;
    homeManager = genFamily (_: v: v.type == "home-manager") all;

    darwin = genFamily (systemPred "-darwin") all;
    linux = genFamily (systemPred "-linux") all;

    aarch64-darwin = genFamily (systemPred "aarch64-darwin") all;
    aarch64-linux = genFamily (systemPred "aarch64-linux") all;
    x86_64-darwin = genFamily (systemPred "x86_64-darwin") all;
    x86_64-linux = genFamily (systemPred "x86_64-linux") all;
  };
in
genFamily (_: _: true) hosts