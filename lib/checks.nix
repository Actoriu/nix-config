{
  self,
  pre-commit-hooks,
  ...
}: system:
with self.pkgs.${system}; {
  pre-commit-check =
    pre-commit-hooks.lib.${system}.run
    {
      src = lib.cleanSource ../.;
      hooks = {
        actionlint.enable = true;
        alejandra.enable = true;
        deadnix.enable = true;
        eslint.enable = true;
        luacheck.enable = true;
        shellcheck.enable = true;
        shfmt.enable = true;
        statix.enable = true;
        stylua.enable = true;

        prettier = {
          enable = true;
          types_or = ["ts" "json" "yaml"];
        };
      };

      settings = {
        eslint.extensions = "\\.ts";
      };
    };
}
