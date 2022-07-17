{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.user;
in
{
  options.custom.user = {
    name = mkOption {
      type = types.str;
      description = "Enable support for users.users.<name>.name";
    };
  };
}
