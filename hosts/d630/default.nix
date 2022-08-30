# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config
, pkgs
, ...
}: {
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # ../../modules/nixos
      # ../../profiles/nixos
    ];

  services.xserver.enable = true;

  custom = {
    fonts.enable = true;
    audio = {
      enable = true;
      pipewire = true;
    };
    bluetooth.enable = true;
    cpu.intel.enable = true;
    opengl.enable = true;
    printers.enable = true;
    video.nvidia = {
      enable = true;
      drivers = "nvidia-340";
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
    users = {
      enable = true;
      # defaultUserShell = true;
      package = pkgs.zsh;
      # userName = "actoriu";
      userVersion = "22.11";
    };
  };

  users.users.${username} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    uid = 1000;
  };

  # home-manager.useGlobalPkgs = true;
  # home-manager.useUserPackages = true;
  # home-manager = {
  #   users.${config.custom.users.userName} = {
  #     imports = [
  #       impermanence.nixosModules.home-manager.impermanence
  #       ../../users/actoriu
  #     ];
  #   };
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
