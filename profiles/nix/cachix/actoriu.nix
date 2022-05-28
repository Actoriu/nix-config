{ ... }: {
  nix = {
    settings = {
      substituters = [ "https://nix-actions.cachix.org" ];
      trusted-public-keys = [
        "nix-actions.cachix.org-1:WTp8/9EIjoPRzwSERLLMHzDUVGthajaIJ/zEZY6DHvM="
      ];
    };
  };
}
