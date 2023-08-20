{pkgs, ...}: {
  projectRootFile = "flake.nix";
  programs = {
    alejandra.enable = true;
    prettier.enable = true;
    shellcheck.enable = true;
    shfmt.enable = true;
  };
  settings.formatter = {
    alejandra = {
      excludes = [
        "pkgs/_sources/generated.nix"
      ];
    };
    prettier = {
      excludes = [
        "pkgs/_sources/generated.json"
        ".github/renovate.json"
      ];
    };
  };
}
