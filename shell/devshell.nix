{
  pkgs,
  self,
  system,
  formatterPackArgsFor,
  ...
}:
pkgs.devshell.mkShell {
  name = "nix-config";
  # imports = [(pkgs.devshell.extraModulesDir + "/git/hooks.nix")];
  # git.hooks.enable = true;
  # git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
  packages = with pkgs; [
    cachix
    # home-manager
    # alejandra
    # nodePackages.prettier
    # nodePackages.prettier-plugin-toml
    # shfmt
    # treefmt
    (formatterPackArgsFor.${system})
  ];

  commands = [
    {
      category = "update";
      name = pkgs.nvfetcher.pname;
      help = pkgs.nvfetcher.meta.description;
      command = "cd $PRJ_ROOT/pkgs; ${pkgs.nvfetcher}/bin/nvfetcher --commit-changes -c ./sources.toml $@";
    }
  ];

  devshell = {
    startup = {
      # only for pkgs.treefmt
      # nodejs-setuphook = pkgs.lib.stringsWithDeps.noDepEntry ''
      #   export NODE_PATH=${pkgs.nodePackages.prettier-plugin-toml}/lib/node_modules:$NODE_PATH
      # '';
      pre-commit.text = "${self.checks.${system}.pre-commit-check.shellHook}";
    };
  };
}
