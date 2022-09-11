{ config
, lib
, ...
}:
with lib; let
  cfg = config.custom.tmux;
in
{
  options.custom.tmux = {
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
