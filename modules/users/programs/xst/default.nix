{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.xst;
in
{
  options.custom.xst = {
    enable = mkEnableOption "Enable support for st terminal emulator.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
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
}
