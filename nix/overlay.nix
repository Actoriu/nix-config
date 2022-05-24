{
  self,
  deploy-rs,
  emacs-overlay,
  guix-overlay,
  neovim-overlay,
  nixos-cn,
  nur,
  nvfetcher,
  ragenix,
  nixpkgs,
  ...
}:

let
  inherit (nixpkgs.lib) composeManyExtensions;
  inherit (builtins) attrNames readDir;
  localOverlays = map
    (f: import (../overlays + "/${f}"))
    (attrNames (readDir ../overlays));

in
composeManyExtensions (localOverlays ++ [
  deploy-rs.overlay
  emacs-overlay.overlay
  guix-overlay.overlays.default
  neovim-overlay.overlay
  nixos-cn.overlay
  nur.overlay
  nvfetcher.overlay
  ragenix.overlay
])
