{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.pip;
in {
  options = {
    programs.pip = {
      enable = mkEnableOption (lib.mdDoc "{command}`pip` user config");

      /*
      package = mkOption {
        type = types.package;
        description = lib.mdDoc "The npm package version / flavor to use";
        default = pkgs.python311Packages.pip;
        defaultText = literalExpression "pkgs.python311Packages.pip";
        example = literalExpression "pkgs.python3xxPackages.pip";
      };
      */

      pipconf = mkOption {
        type = lib.types.lines;
        description = lib.mdDoc ''
          The system-wide npm configuration.
          See <https://pip.pypa.io/en/stable/topics/configuration/>.
        '';
        default = ''
          [global]
          index-url = https://pypi.tuna.tsinghua.edu.cn/simple
        '';
        example = ''
          [global]
          index-url = https://pypi.tuna.tsinghua.edu.cn/simple
          extra-index-url = https://mirror.sjtu.edu.cn/pypi/web/simple https://mirrors.bfsu.edu.cn/pypi/web/simple
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && config.xdg.enable) {
      xdg.configFile."pip/pip.conf".text = cfg.pipconf;
    })
    /*
    (mkIf cfg.enable {
      home.packages = [cfg.package];
    })
    */
  ];
}
