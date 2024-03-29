{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;
    userName = "Actoriu";
    userEmail = "51398008+Actoriu@users.noreply.github.com";
    extraConfig = {
      url = {
        "git@github.com:" = {
          insteadOf = "https://github.com/";
        };
      };
      # http = {
      #   proxy = socks5://127.0.0.1:20170;
      # };
      core = {
        pager = "${pkgs.gitAndTools.delta}/bin/delta";
      };
      pager = {
        diff = "${pkgs.gitAndTools.delta}/bin/delta";
        log = "${pkgs.gitAndTools.delta}/bin/delta";
        reflog = "${pkgs.gitAndTools.delta}/bin/delta";
        show = "${pkgs.gitAndTools.delta}/bin/delta";
      };
      interactive = {
        diffFilter = "${pkgs.gitAndTools.delta}/bin/delta --color-only";
      };
      color = {
        ui = true;
      };
      diff = {
        colorMoved = "default";
      };
    };
  };

  programs.git.delta = {
    enable = true;
    options = {
      syntax-theme = "Nord";
      line-numbers = true;
      side-by-side = true;
      features = "line-numbers side-by-side decorations";
      whitespace-error-style = "22 reverse";
      decorations = {
        commit-decoration-style = "bold yellow box ul";
        file-style = "bold yellow ul";
        file-decoration-style = "none";
        hunk-header-decoration-style = "yellow box";
      };
    };
  };
}
