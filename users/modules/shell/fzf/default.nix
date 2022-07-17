{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.shell.fzf;
in
{
  options.custom.shell.fzf = {
    enable = mkEnableOption "Enable support for fzf.";
  };

  config = mkIf cfg.enable (mkMerge [
    { programs.fzf.enable = cfg.enable; }
    (mkIf config.custom.shell.tmux.enable {
      programs.fzf.tmux = {
        enableShellIntegration = config.custom.shell.tmux.enable;
      };
    })
  ]);
}
