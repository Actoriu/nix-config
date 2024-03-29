{
  lib,
  pkgs,
  stateVersion,
  username,
  ...
}: {
  imports = lib.optional (username != null) ../profiles/users/${username};

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = username;
    homeDirectory =
      (
        if pkgs.stdenv.isDarwin
        then "/Users"
        else "/home"
      )
      + "/${username}";
    stateVersion = "${stateVersion}";
  };

  programs.home-manager.enable = true;
  manual.manpages.enable = false;
  systemd.user.startServices = "sd-switch";
}
