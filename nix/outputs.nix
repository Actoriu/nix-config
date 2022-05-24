{ self, ... }@inputs:
{
  # nixos
  nixosConfigurations = {
    d630 = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [
        ({ config, lib, pkgs, ... }:
          let
            overlay-unstable = final: prev: {
              unstable = inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;
            };
          in {
            nixpkgs = {
              config = {
                allowUnfree = true;
              };
              overlays = [
                inputs.deploy-rs.overlay
                inputs.emacs-overlay.overlay
                inputs.guix-overlay.overlays.default
                inputs.neovim-overlay.overlay
                inputs.nixos-cn.overlay
                inputs.nur.overlay
                inputs.nvfetcher.overlay
                inputs.ragenix.overlay
                overlay-unstable
                (final: prev: { spacemacs = inputs.spacemacs; })
                # (import ./overlays)
                # (import ./pkgs)
              ];
            };
            imports = [
              ./hosts/d630/default.nix
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
                      ./users/actoriu/default.nix
                    ];
                  };
                };
              }
            ];
          })
      ];
    };
  };

  # home-manager
  homeConfigurations = {
    actoriu = inputs.home-manager.lib.homeManagerConfiguration {
      system = "x86_64-linux";
      stateVersion = "21.11";
      username = "actoriu";
      homeDirectory = "/home/actoriu";
      configuration = { config, lib, pkgs, ... }:
        let
          overlay-unstable = final: prev: {
            unstable = inputs.nixpkgs-unstable.legacyPackages.x86_64-linux;
          };
        in {
          nixpkgs = {
            config = {
              allowUnfree = true;
            };
            overlays = [
              inputs.deploy-rs.overlay
              inputs.emacs-overlay.overlay
              inputs.guix-overlay.overlays.default
              inputs.neovim-overlay.overlay
              inputs.nixos-cn.overlay
              inputs.nur.overlay
              inputs.nvfetcher.overlay
              inputs.ragenix.overlay
              overlay-unstable
              (final: prev: { spacemacs = inputs.spacemacs; })
              # (import ./overlays)
              # (import ./pkgs)
            ];
          };
          imports = [ ./users/actoriu/default.nix ];
        };
    };
  };
  defaultPackage.x86_64-linux =
    self.homeConfigurations.actoriu.activationPackage;

  # nix-on-droid
  nixOnDroidConfigurations = {
    oneplus5 = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
      system = "aarch64-linux";
      # config = ./hosts/oneplus5/default.nix;
      config = {
        imports = [
          ./hosts/oneplus5/default.nix
        ];
        home-manager = {
          backupFileExtension = "backup";
          useGlobalPkgs = true;
          useUserPackages = true;
          config = { config, lib, pkgs, ... }:
            let
              overlay-unstable = final: prev: {
                unstable = inputs.nixpkgs-unstable.legacyPackages.aarch64-linux;
              };
            in {
              nixpkgs = {
                config = {
                  allowUnfree = true;
                };
                verlays = [
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
                ];
              };
              imports = [ ./users/nix-on-droid/default.nix ];
            };
        };
      };
    };
  };
};
