{
  self,
  inputs,
  ...
}:
with inputs; [
  (final: prev: {
    __dontExport = true;
    lib = prev.lib.extend (
      lfinal: lprev: {
        our = self.lib;
      });
    spacemacs = inputs.spacemacs;
  })
  inputs.deploy-rs.overlay
  inputs.emacs-overlay.overlay
  inputs.guix-overlay.overlays.default
  inputs.neovim-overlay.overlay
  inputs.nixos-cn.overlay
  inputs.nur.overlay
  inputs.nvfetcher.overlay
  inputs.sops-nix.overlay
  (import ../../pkgs)
]
