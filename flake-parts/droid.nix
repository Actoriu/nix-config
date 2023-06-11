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
  imports = lib.optional (hostname != null) ../hosts/${hostname};

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
      imports = lib.optional (username != null) ../users/${username};
    };
  };
}
