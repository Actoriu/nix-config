{ inputs
, lib
, username
, persistence
, ...
}: {
  imports = with inputs; [
    impermanence.nixosModules.home-manager.impermanence
  ] ++ [
    ./editors
    ./lang
    ./misc
    ./readers
    ./services
    ./shell
    ./terminal
    ./video
  ];
}
