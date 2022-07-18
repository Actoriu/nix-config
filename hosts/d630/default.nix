# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config
, lib
, pkgs
, ...
}: {
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  custom = {
    development = {
      cc.enable = true;
      nodejs.enable = true;
      python.enable = true;
      texlive.enable = true;
    };
    editors = {
      emacs = {
        enable = true;
        spacemacs = true;
        emacs-application-framework = true;
      };
      neovim.enable = true;
      zathura.enable = true;
    };
    fonts.enable = true;
    hardware = {
      audio.pipewire.enable = true;
      bluetooth.enable = true;
      cpu.intel.enable = true;
      opengl.enable = true;
      printers.enable = true;
      video.nvidia = {
        enable = true;
        drivers = "nvidia-340";
      };
    };
    loader = {
      enable = true;
      biostype = "legacy";
      bootloader = "grub";
      device = "/dev/sda";
      disktype = "mbr";
    };
    locale = {
      enable = true;
      inputMethod = "fcitx5";
      locale = "zh_CN";
    };
    network = {
      enable = true;
      networkmanager = true;
    };
    powerManagement = {
      enable = true;
      acpid = true;
      powertop = true;
      tlp = true;
      upower = true;
    };
    shell = {
      bat.enable = true;
      dircolors.enable = true;
      direnv = {
        enable = true;
        nix-direnv = true;
      };
      fzf.enable = true;
      git.enable = true;
      gnupg.enable = true;
      openssh.enable = true;
      password-store.enable = true;
      rofi.enable = true;
      tmux.enable = true;
      xdg.enable = true;
      xresources.enable = true;
      zoxide.enable = true;
      zsh.enable = true;
    };
    terminal = {
      alacritty.enable = true;
      urxvt.enable = true;
      xst.enable = true;
    };
    usershell = {
      enable = true;
      defaultUserShell = true;
      package = pkgs.zsh;
    };
    user.name = "actoriu";
    version.enable = "22.05";
    video = {
      mpv.enable = true;
    };
  };

  users.users.${config.user.name} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    uid = 1000;
    useDefaultShell = true;
  };

  # home-manager.users.${config.user.name} = {
  #   home.stateVersion = config.custom.version;
  # };

  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkbOptions in tty.
  # };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = {
  #   "eurosign:e";
  #   "caps:escape" # map caps to escape.
  # };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
  #   firefox
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  # system.stateVersion = "22.05"; # Did you read the comment?
}
