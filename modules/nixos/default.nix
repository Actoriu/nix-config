{ inputs
, lib
, hostname
, persistence
, ...
}: {
  imports = with inputs; [
    impermanence.nixosModules.impermanence
    nixos-cn.nixosModules.nixos-cn-registries
    nixos-cn.nixosModules.nixos-cn
  ] ++ [
    ./fonts
    ./hardware
    ./loader
    ./locale
    ./network
    ./power-management
    ./users
  ];
}
