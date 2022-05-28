{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.terminal.xst;
in
{
  options.custom.terminal.xst = {
    enable = mkEnableOption "Enable support for st terminal emulator.";
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        xst
        (makeDesktopItem {
          name = "xst";
          desktopName = "Suckless Terminal";
          genericName = "Default terminal";
          icon = "utilities-terminal";
          exec = "${xst}/bin/xst";
          categories = [ "System" "TerminalEmulator" ];
        })
      ];
    };
  };
}
