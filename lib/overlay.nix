{
  devshell,
  nixos-cn,
  nixpkgs,
  nur,
  sops-nix,
  spacemacs,
  ...
}: let
  inherit (nixpkgs) lib;
  localOverlays =
    lib.mapAttrs'
    (f: _:
      lib.nameValuePair
      (lib.removeSuffix ".nix" f)
      (import (./overlays + "/${f}")))
    (builtins.readDir ./overlays);
in
  localOverlays
  // {
    default = lib.composeManyExtensions ((lib.attrValues localOverlays)
      ++ [
        devshell.overlay
        nixos-cn.overlay
        nur.overlay
        sops-nix.overlay
        {
          spacemacs = final: prev: {
            spacemacs = spacemacs;
          };
        }
      ]);
  }
