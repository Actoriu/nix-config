{
  config,
  lib,
  pkgs,
  ...
}: {
  programs = {
    ssh = {
      enable = true;
      controlMaster = "auto";
      matchBlocks = {
        "github.com" = {
          hostname = "ssh.github.com";
          user = "git";
          port = 443;
          identityFile = "~/.ssh/id_ed25519.pub";
          identitiesOnly = true;
          extraOptions = {
            AddKeysToAgent = "yes";
            # UseKeychain = "yes";
            # PreferredAuthentications = "publickey";
          };
        };
      };
    };
  };
}
