{ self
, inputs
, ...
}: {
  d630 = inputs.nixos.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = with inputs; [
      impermanence.nixosModules.impermanence
      nixos-cn.nixosModules.nixos-cn-registries
      nixos-cn.nixosModules.nixos-cn
      home-manager.nixosModules.home-manager
      ({ pkgs, ... }: {
        nixpkgs = {
          config = { allowUnfree = true; };
          overlays = with inputs; [
            nixos-cn.overlay
            nur.overlay
            nvfetcher.overlay
            (final: prev: { spacemacs = inputs.spacemacs; })
          ];
        };
        system.configurationRevision =
          inputs.nixos.lib.mkIf (self ? rev) self.rev;
      })
      ./profiles/shared/home-manager
      ./hosts/d630
    ];
  };
}
