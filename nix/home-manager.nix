{ self, inputs, ... }: {
  actoriu = inputs.home-manager.lib.homeManagerConfiguration {
    system = "x86_64-linux";
    stateVersion = "21.11";
    username = "actoriu";
    homeDirectory = "/home/actoriu";
    configuration = { config, lib, pkgs, ... }:
      # let
      #   overlay-unstable = final: prev: {
      #     unstable = inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;
      #   };
      # in {
      {
        nixpkgs = {
          config = { allowUnfree = true; };
          overlays = [
            inputs.deploy-rs.overlay
            inputs.emacs-overlay.overlay
            inputs.guix-overlay.overlays.default
            inputs.neovim-overlay.overlay
            inputs.nixos-cn.overlay
            inputs.nur.overlay
            inputs.nvfetcher.overlay
            inputs.ragenix.overlay
            # overlay-unstable
            (final: prev: { spacemacs = inputs.spacemacs; })
            # (import ./overlays)
            # (import ./pkgs)
          ];
        };
        imports = [ ../users/actoriu/default.nix ];
      };
  };
}
