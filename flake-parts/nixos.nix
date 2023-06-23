{
  inputs,
  lib,
  pkgs,
  stateVersion,
  self,
  ...
}: let
  buildSuites = profiles: f: lib.mapAttrs (_: lib.flatten) (lib.fix (f profiles));

  nixosModules = lib.buildModuleList ../modules/nixos;
  nixosProfiles = lib.rakeLeaves ../profiles/nixos;
  nixosSuites = buildSuites nixosProfiles (profiles: suites: {
    base = with profiles; [nix];
    hardware = with profiles; [
      hardware.audio.pipewire
      hardware.bluetooth
      hardware.cpu.intel
      hardware.opengl
      hardware.printers
      hardware.video.nvidia.340xx
    ];
    graphical = with profiles; [fonts];
    network = with profiles; [
      networking.networkmanager
    ];
    power = with profiles; [
      power-manager.acpid
      power-manager.powertop
      power-manager.tlp
      power-manager.upower
    ];
    security = with profiles; [security.polkit];
    services = with profiles; [services.laptop];
    shell = with profiles; [
      shell.zsh
    ];
  });

  defaultModules =
    nixosModules
    ++ [
      # make flake inputs accessible in NixOS
      {
        _module.args.self = self;
        _module.args.inputs = inputs;
        _module.args.lib = lib;
      }
      # load common modules
      ({...}: {
        imports = [
          inputs.impermanence.nixosModules.impermanence
          inputs.disko.nixosModules.disko
          inputs.nur.nixosModules.nur
          inputs.sops-nix.nixosModules.sops
          inputs.home-manager.nixosModules.home-manager
        ];
      })
    ];

  mkNixosConfig = {
    extraModules ? [],
    hostname ? null,
    non-nixos ? false,
    username ? null,
    system ? "x86_64-linux",
    stateVersion ? lib.fileContents ../.version,
    ...
  }: {
    ${hostname} = lib.nixosSystem {
      specialArgs = {
        inherit username stateVersion;
        # profiles = nixosProfiles;
        # suites = nixosSuites;
      };
      modules =
        defaultModules
        ++ extraModules
        ++ lib.optional (hostname != null) ../hosts/${hostname}
        ++ [
          ({
            config,
            lib,
            ...
          }: {
            nixpkgs = {
              config = {
                allowUnfree = true;
                # for build nvidia
                allowBroken = true;
              };
              hostPlatform = lib.mkDefault system;
            };

            networking.hostName = lib.mkDefault hostname;

            system.stateVersion = stateVersion;

            home-manager = {
              extraSpecialArgs = {inherit inputs non-nixos username stateVersion;};
              # useGlobalPkgs = true;
              useUserPackages = true;
              users.${username} = import ./home.nix;
            };
          })
        ];
    };
  };
in {
  flake.nixosConfigurations = lib.mkMerge [
    (mkNixosConfig {
      hostname = "d630";
      username = "actoriu";
      extraModules = [];
    })
  ];
}
