{...}: {
  nix = {
    settings = {
      substituters = ["https://nix-actions.cachix.org"];
      trusted-public-keys = [
        "nix-actions.cachix.org-1:0QFVMkhyCHpNBpxW1XyI8/+OGW81Vt1KpeeN/IdjEYg="
      ];
    };
  };
}
