{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.shell;
in {
  options.custom.shell = {
    enable = mkEnableOption "Enable support for user shell.";

    defaultUserShell = mkOption {
      type = types.bool;
      default = true;
      description = ''
        This option defines the default shell assigned to user
        accounts. This can be either a full system path or a shell package.
        This must not be a store path, since the path is
        used outside the store (in particular in /etc/passwd).
      '';
    };

    package = mkOption {
      type = types.shellPackage;
      default = pkgs.zsh;
      defaultText = "pkgs.zsh";
      example = literalExpression "pkgs.fish";
      description = "Enable support for user shell package.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    # (mkIf (cfg.package.pname == "fish") { programs.fish.enable = true; })
    # (mkIf (cfg.package.pname == "zsh") { programs.zsh.enable = true; })
    (mkIf cfg.defaultUserShell { users.defaultUserShell = cfg.package; })
  ]);
}
