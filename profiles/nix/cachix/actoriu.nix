{ ... }: {
  nix = {
    settings = {
      substituters = [
        "https://actoriu.cachix.org"
      ];
      trusted-public-keys = [
        "actoriu.cachix.org-1:htl65pXtoZ5aa5pgM5Rj42jg02WGBFabB8vcm3WVm8A="
      ];
    };
  };
}
