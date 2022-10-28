{ inputs
, self
, ...
}:
inputs.nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  specialArgs = { inherit inputs; };
  modules = [
    ({ ... }: {
      nixpkgs = {
        config = {
          allowUnfree = true;
          allowBroken = true;
          allowUnsupportedSystem = true;
        };
        overlays = builtins.attrValues self.overlays;
      };
    })
    inputs.impermanence.nixosModules.impermanence
    inputs.nixos-cn.nixosModules.nixos-cn-registries
    inputs.nixos-cn.nixosModules.nixos-cn
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager
    {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = { inherit inputs; };
        users.actoriu = { config, pkgs, ... }: {
          home.stateVersion = "22.11";
          programs.home-manager.enable = true;
          manual.manpages.enable = false;
          systemd.user.startServices = "sd-switch";
          imports = [
            inputs.impermanence.nixosModules.home-manager.impermanence
            ../../modules/users
            ../../users/actoriu
          ];
        };
      };
    }
    ../../modules/nixos
    ../../profiles/nixos
    ./configuration.nix
  ];
}
