{ config
, ...
}: {
  home = {
    username = config.users.users.${name}.name;
    homeDirectory = config.users.users.${name}.home;
    stateVersion = config.custom.users.version;
  };
  programs.home-manager.enable = true;
}
