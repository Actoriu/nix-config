{ inputs
, hostname
, persistence
, ...
}: {
  imports = [
    inputs.impermanence.nixosModules.impermanence
    ./fonts
    ./hardware
    ./loader
    ./locale
    ./network
    ./power-management
    ./users
  ];

  networking.hostName = hostname;
}
