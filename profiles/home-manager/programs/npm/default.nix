{
  config,
  lib,
  pkgs,
  ...
}: {
  programs = {
    npm = {
      enable = true;
      npmrc = ''
        cache = ''${XDG_CACHE_HOME}/npm
        prefix = ''${XDG_DATA_HOME}/npm
        registry=https://registry.npmmirror.com
      '';
    };
  };
}
