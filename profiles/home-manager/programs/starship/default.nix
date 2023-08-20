{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.starship = {
    enable = true;
    settings = {
      command_timeout = 1000;
      character = {
        error_symbol = "[❯](bold red)[❯](bold yellow)[❯](bold red)";
        success_symbol = "[❯](bold red)[❯](bold yellow)[❯](bold green)";
        vimcmd_replace_one_symbol = "[❮](bold green)[❮](bold yellow)[❮](bold purple)";
        vimcmd_replace_symbol = "[❮](bold green)[❮](bold yellow)[❮](bold purple)";
        vimcmd_symbol = "[❮](bold green)[❮](bold yellow)[❮](bold red)";
        vimcmd_visual_symbol = "[❮](bold green)[❮](bold yellow)[❮](bold yellow)";
      };
    };
  };
}
