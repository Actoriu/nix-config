{ config
, ...
}: {
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    # extraSpecialArgs = { inherit inputs; };
    sharedModules = [{
      manual.manpages.enable = false;
      programs.home-manager.enable = true;
      home.stateVersion = "${config.custom.users.version}";
    }];
  };
}
