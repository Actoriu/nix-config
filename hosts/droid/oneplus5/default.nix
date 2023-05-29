{
  config,
  inputs,
  lib,
  outputs,
  pkgs,
  username,
  ...
}: {
  # Set up nix for flakes
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Set your time zone
  time = {
    timeZone = "Asia/Shanghai";
  };

  # Set your shell
  user = {
    shell = "${pkgs.zsh}/bin/zsh";
  };

  terminal.font = let
    fontPackage = pkgs.nerdfonts.override {
      fonts = ["Iosevka"];
    };
    fontPath = "/share/fonts/truetype/NerdFonts/Iosevka Nerd Font Complete Mono.ttf";
  in
    fontPackage + fontPath;

  # Simply install just the packages
  environment = {
    # Simply install just the packages
    packages = with pkgs; [
      # User-facing stuff that you really really want to have
      # vim  # or some other editor, e.g. nano or neovim

      # Some common stuff that people expect to have
      diffutils
      findutils
      utillinux
      tzdata
      hostname
      man
      gawk
      gnugrep
      gnupg
      gnused
      gnutar
      bzip2
      gzip
      xz
      zip
      unzip
    ];

    # Backup etc files instead of failing to activate generation if a file already exists in /etc
    etcBackupExtension = ".bak";
  };

  # Read the changelog before changing this value
  system.stateVersion = config.lib.self.flakeStateVersion;

  # Configure home-manager
  home-manager = {
    extraSpecialArgs = {inherit inputs outputs;};
    # useGlobalPkgs = true;
    useUserPackages = true;
    config = {
      config,
      lib,
      pkgs,
      ...
    }: {
      nixpkgs = {
        config = {
          allowUnfree = true;
          # Workaround for https://github.com/nix-community/home-manager/issues/2942
          allowUnfreePredicate = _: true;
        };
        overlays = builtins.attrValues outputs.overlays;
      };
      home.stateVersion = lib.self.flakeStateVersion;
      manual.manpages.enable = false;
      programs.home-manager.enable = true;
      imports = [
        ../../../modules/home-manager
        ../../../users/${username}
      ];
    };
  };
}
