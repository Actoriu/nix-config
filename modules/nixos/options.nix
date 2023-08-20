{
  config,
  options,
  lib,
  home-manager,
  ...
}: let
  inherit (builtins) elem pathExists toString;
  inherit
    (lib)
    findFirst
    isList
    mapAttrs
    mapAttrsToList
    mkAliasDefinitions
    mkOption
    ;
  inherit (lib.strings) concatMapStringsSep concatStringsSep;
  inherit (lib.types) attrs attrsOf either listOf oneOf path str;
  inherit (lib.my) mkOpt mkOpt';
in {
  options = {
    user = mkOpt attrs {};
  };

  config = {
    user = let
      user = builtins.getEnv "USER";
      name =
        if elem user ["" "root"]
        then "actoriu"
        else user;
    in {
      inherit name;
      description = "Primary user account";
      extraGroups = ["wheel"];
      isNormalUser = true;
      home = "/home/${name}";
      group = "users";
      uid = 1000;
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;
  };
}
