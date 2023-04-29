{
  config,
  inputs,
  lib,
  outputs,
  pkgs,
  username,
  version,
  ...
}: let
  sshdTmpDirectory = "${config.user.home}/sshd-tmp";
  sshdDirectory = "${config.user.home}/sshd";
  pathToPubKey = "...";
  port = 8022;
in {
  # FIXME: Move sshd config to nix-on-droid
  build.activation.sshd = ''
    $DRY_RUN_CMD mkdir $VERBOSE_ARG --parents "${config.user.home}/.ssh"
    $DRY_RUN_CMD cat ${pathToPubKey} > "${config.user.home}/.ssh/authorized_keys"

    if [[ ! -d "${sshdDirectory}" ]]; then
      $DRY_RUN_CMD rm $VERBOSE_ARG --recursive --force "${sshdTmpDirectory}"
      $DRY_RUN_CMD mkdir $VERBOSE_ARG --parents "${sshdTmpDirectory}"

      $VERBOSE_ECHO "Generating host keys..."
      $DRY_RUN_CMD ${pkgs.openssh}/bin/ssh-keygen -t rsa -b 4096 -f "${sshdTmpDirectory}/ssh_host_rsa_key" -N ""

      $VERBOSE_ECHO "Writing sshd_config..."
      $DRY_RUN_CMD echo -e "HostKey ${sshdDirectory}/ssh_host_rsa_key\nPort ${toString port}\n" > "${sshdTmpDirectory}/sshd_config"

      $DRY_RUN_CMD mv $VERBOSE_ARG "${sshdTmpDirectory}" "${sshdDirectory}"
    fi
  '';

  # Set up nix for flakes
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
      "https://pre-commit-hooks.cachix.org"
      "https://nix-actions.cachix.org"
      "https://nix-on-droid.cachix.org"
    ];
    trustedPublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "nix-actions.cachix.org-1:WTp8/9EIjoPRzwSERLLMHzDUVGthajaIJ/zEZY6DHvM="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
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
      (writeScriptBin "sshd-start" ''
        #!${pkgs.runtimeShell}

        echo "Starting sshd in non-daemonized way on port ${toString port}"
        ${pkgs.openssh}/bin/sshd -f "${sshdDirectory}/sshd_config" -D
      '')
    ];

    # Backup etc files instead of failing to activate generation if a file already exists in /etc
    etcBackupExtension = ".bak";
  };

  # Read the changelog before changing this value
  system.stateVersion = "${version}";

  # Configure home-manager
  home-manager = {
    extraSpecialArgs = {inherit inputs outputs version;};
    useGlobalPkgs = true;
    useUserPackages = true;
    config = {
      config,
      lib,
      pkgs,
      ...
    }: {
      # nixpkgs = {
      #   config = {
      #     allowUnfree = true;
      #     allowBroken = true;
      #     allowUnsupportedSystem = true;
      #   };
      #   overlays = builtins.attrValues outputs.overlays;
      # };
      home.stateVersion = "${version}";
      manual.manpages.enable = false;
      imports = [
        ../../../modules/home-manager
        ../../../users/${username}
      ];
    };
  };
}
