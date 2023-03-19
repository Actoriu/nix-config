{
  config,
  inputs,
  lib,
  outputs,
  pkgs,
  username,
  version,
  ...
}: {
  # Set up nix for flakes
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

  # Set your time zone
  time = {
    timeZone = "Asia/Shanghai";
  };

  # Set your shell
  user = {
    shell = "${pkgs.zsh}/bin/zsh";
  };

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
  system.stateVersion = "${version}";

  # Configure home-manager
  home-manager = {
    backupFileExtension = "hm-bak";
    extraSpecialArgs = {inherit inputs outputs version;};
    useGlobalPkgs = true;
    useUserPackages = true;
    config = {
      config,
      lib,
      pkgs,
      ...
    }: {
      home.stateVersion = "${version}";
      manual.manpages.enable = false;
      imports = [
        ../../../modules/home-manager
        ../../../users/${username}
      ];
    };
  };
}
