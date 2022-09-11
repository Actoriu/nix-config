{ inputs, ... }: {
  imports = [
    inputs.impermanence.nixosModules.home-manager.impermanence
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
