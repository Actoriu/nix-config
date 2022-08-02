{ config
, ...
}: {
  home = {
    username = config.custom.users.userName;
    homeDirectory = "home/${config.custom.users.userName}";
    stateVersion = config.custom.users.version;
  };
  programs.home-manager.enable = true;
}
