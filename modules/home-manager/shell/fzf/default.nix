{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.custom.fzf;
in {
  options.custom.fzf = {
    enable = mkEnableOption "Enable support for fzf.";
  };

  config = mkIf cfg.enable (mkMerge [
    {programs.fzf.enable = cfg.enable;}
    (mkIf config.custom.tmux.enable {
      programs.fzf.tmux = {
        enableShellIntegration = config.custom.tmux.enable;
      };
    })
  ]);
}
