{
  self,
  inputs,
  ...
}: {
  oneplus5 = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
    system = "aarch64-linux";
    # config = ./hosts/oneplus5/default.nix;
    config = { config, lib, pkgs, ... }:
      let
        overlay-unstable = final: prev: {
          unstable = inputs.nixpkgs-unstable.legacyPackages.aarch64-linux;
        };
      in {
        imports = [
          ../hosts/oneplus5/default.nix
        ];
        home-manager = {
          backupFileExtension = "backup";
          useGlobalPkgs = true;
          useUserPackages = true;
          config = { config, lib, pkgs, ... }: {
            nixpkgs = {
              config = {
                allowUnfree = true;
              };
              overlays = [
                inputs.deploy-rs.overlay
                inputs.emacs-overlay.overlay
                # inputs.guix-overlay.overlays.default
                inputs.neovim-overlay.overlay
                inputs.nixos-cn.overlay
                inputs.nur.overlay
                # inputs.nvfetcher.overlay
                inputs.ragenix.overlay
                overlay-unstable
                (final: prev: { spacemacs = inputs.spacemacs; })
                # (import ./overlays)
                # (import ./pkgs)
              ] ++
              config.nixpkgs.overlays;
            };
            home.stateVersion = "21.11";
            imports = [ ../users/nix-on-droid/default.nix ];
          };
        };
      };
  };
}
