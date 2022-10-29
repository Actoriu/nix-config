{ inputs
, self
, ...
}: {
  imports = [
    ({ ... }: {
      nixpkgs = {
        inherit (self.legacyPackages."x86_64-linux") config overlays;
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
        extraSpecialArgs = { inherit inputs self; };
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
