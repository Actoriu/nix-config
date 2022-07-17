{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.shell.tmux;
in
{
  options.custom.shell.tmux = {
    enable = mkEnableOption "Enable support for tmux.";
  };

  config = mkIf cfg.enable {
    programs = {
      tmux = {
        enable = cfg.enable;
        terminal = "screen-256color";
      };
    };
  };
}
