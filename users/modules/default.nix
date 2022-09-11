{ inputs
, persistence
, features
, ...
}: {
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
  ]
  # Import features that have modules
  ++ builtins.filter builtins.pathExists (map (feature: ./${feature}) features);

  systemd.user.startServices = "sd-switch";
}
