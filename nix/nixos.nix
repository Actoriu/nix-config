{ self, inputs, ... }: {
  d630 = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = [
      ({ config, lib, pkgs, ... }:
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
          imports = [
            ../hosts/d630/default.nix
            ../profiles/nix/gc/default.nix
            ../profiles/nix/settings/default.nix
            ../profiles/nix/version/default.nix
            ../profiles/nix/cachix/default.nix
            inputs.guix-overlay.nixosModules.guix-binary
            inputs.impermanence.nixosModules.impermanence
            inputs.nixos-cn.nixosModules.nixos-cn-registries
            inputs.nixos-cn.nixosModules.nixos-cn
            inputs.ragenix.nixosModules.age
            inputs.sops-nix.nixosModules.sops
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.actoriu = {
                  imports = [
                    inputs.impermanence.nixosModules.home-manager.impermanence
                    ../users/actoriu/default.nix
                  ];
                };
              };
            }
          ];
        })
    ];
  };
}
