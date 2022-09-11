{ config
, pkgs
, ...
}:
let
  sshdTmpDirectory = "${config.user.home}/sshd-tmp";
  sshdDirectory = "${config.user.home}/sshd";
  pathToPubKey = "${config.user.home}/.ssh/id_rsa.pub";
  port = 8022;
in
{
  nix = {
    extraConfig = ''
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

      # sshd-start script
      # refers to: https://github.com/t184256/nix-on-droid/wiki/SSH-access
      (pkgs.writeScriptBin "sshd-start" ''
        #!${pkgs.runtimeShell}
        echo "Starting sshd in non-daemonized way on port ${toString port}"
        ${pkgs.openssh}/bin/sshd -f "${sshdDirectory}/sshd_config" -D
      '')

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
      openssh
      nettools
      (
        lib.setPrio # make bintools less prior

          (busybox.meta.priority + 10)
          busybox
      )
    ];

    # Backup etc files instead of failing to activate generation if a file already exists in /etc
    etcBackupExtension = ".bak";
  };

  # Read the changelog before changing this value
  system.stateVersion = "22.05";

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

  # nix-channel --add https://github.com/rycee/home-manager/archive/release-22.05.tar.gz home-manager
  # nix-channel --update
  # you can configure home-manager in here like
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    config =
      { lib
      , pkgs
      , ...
      }: {
        home.stateVersion = "22.11";
        imports = [ ../../users/nix-on-droid ];
      };
  };
}
# vim: ft=nix

