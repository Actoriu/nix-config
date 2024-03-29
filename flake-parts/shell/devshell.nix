{inputs, ...}: {
  imports = [inputs.devshell.flakeModule];

  perSystem = {
    config,
    pkgs,
    ...
  }: {
    devshells.default = {
      name = "nix-config";
      # imports = [(pkgs.devshell.extraModulesDir + "/git/hooks.nix")];
      # git.hooks.enable = true;
      # git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
      packages = with pkgs; [
        # cachix
        # home-manager
        # alejandra
        # nodePackages.prettier
        # nodePackages.prettier-plugin-toml
        # shfmt
        # treefmt
        sops
        ssh-to-age
        age
        rage
        (config.treefmt.build.wrapper)
      ];

      commands = [
        {
          category = "update";
          name = pkgs.nvfetcher.pname;
          help = pkgs.nvfetcher.meta.description;
          command = "cd $PRJ_ROOT/pkgs; ${pkgs.nvfetcher}/bin/nvfetcher -c ./sources.toml $@";
        }
      ];

      devshell = {
        startup = {
          # only for pkgs.treefmt
          nodejs-setuphook = pkgs.lib.stringsWithDeps.noDepEntry ''
            export NODE_PATH=${pkgs.nodePackages.prettier-plugin-toml}/lib/node_modules:$NODE_PATH
          '';
          pre-commit.text = config.pre-commit.installationScript;
        };
      };
    };
  };
}
