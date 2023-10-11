{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.npm;
in {
  options = {
    programs.npm = {
      enable = mkEnableOption (lib.mdDoc "{command}`npm` user config");

      package = mkOption {
        type = types.package;
        description = lib.mdDoc "The npm package version / flavor to use";
        default = pkgs.nodePackages.npm;
        defaultText = literalExpression "pkgs.nodePackages.npm";
        example = literalExpression "pkgs.nodePackages_13_x.npm";
      };

      npmrc = mkOption {
        type = lib.types.lines;
        description = lib.mdDoc ''
          The system-wide npm configuration.
          See <https://docs.npmjs.com/misc/config>.
        '';
        default = ''
          prefix = ''${HOME}/.npm
        '';
        example = ''
          prefix = ''${HOME}/.npm
          https-proxy=proxy.example.com
          init-license=MIT
          init-author-url=http://npmjs.org
          color=true
        '';
      };
    };
  };

  config = mkMerge [
    # assertions = [
    #   {
    #     assertion = config.xdg.enable == false;
    #     message = "Need enable management of XDG base directories.";
    #   }
    # ];
    (mkIf (cfg.enable && config.xdg.enable) {
      xdg.configFile."npm/npmrc".text = cfg.npmrc;
    })
    (mkIf cfg.enable {
      home.sessionVariables.NPM_CONFIG_USERCONFIG = "${xdg.configHome}/npm/npmrc";
      home.packages = [cfg.package];
    })
  ];
}
