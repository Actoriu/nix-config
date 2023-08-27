{
  formatterPackArgsFor,
  inputs,
  pkgs,
  ...
}:
inputs.pre-commit-hooks.lib.${pkgs.system}.run {
  src = ../.;
  hooks = {
    actionlint.enable = true;
    alejandra.enable = false;
    deadnix.enable = false;
    eslint = {
      enable = true;
      excludes = ["pkgs/_sources/generated.json" ".github/renovate.json"];
    };
    prettier = {
      enable = false;
      excludes = ["pkgs/_sources/generated.json" ".github/renovate.json"];
    };
    shellcheck.enable = false;
    shfmt = {
      enable = false;
      entry = pkgs.lib.mkForce "${pkgs.shfmt}/bin/shfmt -i 2 -s -w";
    };
    statix.enable = false;
    # treefmt.enable = true;
  };
  settings = {
    treefmt.package = formatterPackArgsFor.${pkgs.system}.config.build.wrapper;
  };
}
