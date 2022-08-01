{ config
, ...
}: {
  home = {
    username = ${config.custom.users.user.name};
    homeDirectory = "home/${config.custom.users.user.name}";
    stateVersion = ${config.custom.users.version};
  };
  programs.home-manager.enable = true;
}
