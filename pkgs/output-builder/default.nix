channels: inputs: {
  packages = {
    # nix develop .#sops-shell --impure
    sops-shell = with channels.nixos;
      mkShell {
        nativeBuildInputs = [
          sops-import-keys-hook
        ];
        sopsPGPKeyDirs = [
          #"../secrets/keys/hosts"
          "../secrets/keys/users"
        ];
      };
  };
}
