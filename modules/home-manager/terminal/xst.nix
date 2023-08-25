{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.private.terminal.xst;
in {
  options.private.terminal.xst = {
    enable = mkEnableOption "Enable support for st terminal emulator.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      xst
      (makeDesktopItem {
        name = "xst";
        comment = "A clone of the terminal emulator st";
        desktopName = "Xst";
        genericName = "xst";
        icon = "utilities-terminal";
        exec = "${xst}/bin/xst";
        categories = ["System" "TerminalEmulator"];
      })
    ];
  };
}
