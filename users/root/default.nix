{
  config,
  ...
}: let
  user = builtins.baseNameOf ./.;
in {
  sops.secrets."users/${user}" = {
    neededForUsers = true;
  };

  users = {
    users = {
      root = {
        passwordFile = config.sops.secrets."users/${user}".path;
      };
    };
  };
}
