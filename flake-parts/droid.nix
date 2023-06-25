{
  config,
  hostname,
  inputs,
  lib,
  outputs,
  pkgs,
  stateVersion,
  username,
  ...
}: {
  imports = lib.optional (hostname != null) ../profiles/hosts/${hostname};

  nixpkgs = {
    config = {
      allowUnfree = true;
      # for build nvidia
      allowBroken = true;
    };
  };

  system.stateVersion = "${stateVersion}";

  home-manager = {
    extraSpecialArgs = {inherit username stateVersion;};
    # useGlobalPkgs = true;
    useUserPackages = true;
    config = {
      config,
      lib,
      pkgs,
      ...
    }: {
      imports =
        [
          inputs.impermanence.nixosModules.home-manager.impermanence
          inputs.nur.hmModules.nur
          inputs.sops-nix.homeManagerModules.sops
        ]
        ++ lib.optional (username != null) ../profiles/users/${username};

      nixpkgs = {
        config = {
          allowUnfree = true;
          # Workaround for https://github.com/nix-community/home-manager/issues/2942
          allowUnfreePredicate = _: true;
        };
        overlays = builtins.attrValues outputs.overlays;
      };

      home.stateVersion = "${stateVersion}";
      manual.manpages.enable = false;
      programs.home-manager.enable = true;
    };
  };
}
