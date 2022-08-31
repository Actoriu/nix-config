{ config
, pkgs
, ...
}: {
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
      "https://cache.nixos.org/"
      "https://nix-on-droid.cachix.org"
    ];
  };

  time = {
    timeZone = "Asia/Shanghai";
  };

  user = {
    shell = "${pkgs.zsh}/bin/zsh";
  };

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
  system.stateVersion = "22.05";

  # nix-channel --add https://github.com/rycee/home-manager/archive/release-22.05.tar.gz home-manager
  # nix-channel --update
  # you can configure home-manager in here like
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    config = { pkgs, ... }: {
      home.stateVersion = "22.05";
      imports = [ ../../users/nix-on-droid ];
    };
  };

}

# vim: ft=nix
