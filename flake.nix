{
  description = "Nix configuration with flakes";

  nixConfig = {
    extra-experimental-features = "nix-command flakes recursive-nix";
    substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://mirrors.ustc.edu.cn/nix-channels/store"
    ];
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://nix-actions.cachix.org"
      "https://nix-on-droid.cachix.org"
      "https://nixos-cn.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-actions.cachix.org-1:WTp8/9EIjoPRzwSERLLMHzDUVGthajaIJ/zEZY6DHvM="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
      "nixos-cn.cachix.org-1:L0jEaL6w7kwQOPlLoCR3ADx+E3Q8SEFEcB9Jaibl0Xg="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    /*
      emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
      flake-utils.follows = "flake-utils";
      nixpkgs.follows = "nixos";
      };
      };

      guix-overlay = {
      url = "github:foo-dogsquared/nix-overlay-guix";
      inputs = {
      nixpkgs.follows = "latest";
      };
      };
    */

    impermanence.url = "github:nix-community/impermanence";

    /*
      neovim-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs = {
      flake-compat.follows = "flake-compat";
      nixpkgs.follows = "latest";
      };
      };
    */

    nixos-cn = {
      url = "github:nixos-cn/flakes";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };

    /*
      nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs = {
      nixpkgs.follows = "nixpkgs";
      };
      };
    */

    nur.url = "github:nix-community/NUR";

    peerix = {
      url = "github:misterio77/peerix";
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    spacemacs = {
      url = "github:syl20bnr/spacemacs/develop";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , flake-utils
    , nix-on-droid
    , ...
    }@inputs:
    let
      forEachSystem = nixpkgs.lib.genAttrs [ "aarch64-linux" "x86_64-linux" ];

      pkgs = forEachSystem (system:
        import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
          };
          overlays = builtins.attrValues self.overlays;
        });

      lib = nixpkgs.lib.extend (final: prev:
        import ./lib {
          inherit pkgs inputs;
          lib = final;
        });

      inherit (lib.my) mkSystem mkHome mkDroid;
    in
    {
      legacyPackages = pkgs;

      overlays = {
        # default = import ./overlays { inherit inputs; };
        devshell = inputs.devshell.overlay;
        nixos-cn = inputs.nixos-cn.overlay;
        nur = inputs.nur.overlay;
        peerix = inputs.peerix.overlay;
        sops-nix = inputs.sops-nix.overlay;
        my = final: prev: { my = self.packages; };
        spacemacs = final: prev: { spacemacs = inputs.spacemacs; };
      };

      # formatter = forEachSystem (system: legacyPackages.${system}.nixpkgs-fmt);

      packages = forEachSystem (system:
        import ./pkgs { pkgs = self.pkgs; }
      );

      # devShells = forEachSystem
      #   (system:
      #     let
      #       pkgs = legacyPackages.${system};
      #     in
      #     {
      #       default = pkgs.devshell.mkShell {
      #         name = "nix-config";
      #         imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
      #         git.hooks.enable = true;
      #         git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
      #         packages = with pkgs; [
      #           cachix
      #           nix-build-uncached
      #           nixpkgs-fmt
      #           nodePackages.prettier
      #           nodePackages.prettier-plugin-toml
      #           nvfetcher
      #           shfmt
      #           treefmt
      #         ];
      #         commands = with pkgs; [
      #           {
      #             category = "update";
      #             name = nvfetcher.pname;
      #             help = nvfetcher.meta.description;
      #             command = "cd $PRJ_ROOT/pkgs; ${nvfetcher}/bin/nvfetcher -c ./sources.toml $@";
      #           }
      #         ];
      #         devshell.startup.nodejs-setuphook = pkgs.lib.stringsWithDeps.noDepEntry ''
      #           export NODE_PATH=${pkgs.nodePackages.prettier-plugin-toml}/lib/node_modules:$NODE_PATH
      #         '';
      #       };
      #     });

      nixosConfigurations = {
        d630 = import ./lib/nixos.nix {
          hostname = "d630";
          username = "actoriu";
          extraModules = [
            inputs.impermanence.nixosModules.impermanence
            inputs.nixos-cn.nixosModules.nixos-cn-registries
            inputs.nixos-cn.nixosModules.nixos-cn
            inputs.sops-nix.nixosModules.sops
            # ({
            #   nixpkgs = { inherit (legacyPackages."x86_64-linux") config overlays; };
            # })
            ./modules/nixos
            ./profiles/nixos
          ];
          home_extraModules = [
            inputs.impermanence.nixosModules.home-manager.impermanence
            ./modules/users
          ];
        };
      };

      #   nixosConfigurations = {
      #     d630 = nixpkgs.lib.nixosSystem {
      #       system = "x86_64-linux";
      #       specialArgs = { inherit inputs self; };
      #       modules = [
      #         ({ ... }: {
      #           nixpkgs = {
      #             inherit (legacyPackages."x86_64-linux") config overlays;
      #           };
      #         })
      #         inputs.impermanence.nixosModules.impermanence
      #         inputs.nixos-cn.nixosModules.nixos-cn-registries
      #         inputs.nixos-cn.nixosModules.nixos-cn
      #         inputs.sops-nix.nixosModules.sops
      #         inputs.home-manager.nixosModules.home-manager
      #         {
      #           home-manager = {
      #             useGlobalPkgs = true;
      #             useUserPackages = true;
      #             extraSpecialArgs = { inherit inputs self; };
      #             users.actoriu = { ... }: {
      #               home.stateVersion = "22.11";
      #               programs.home-manager.enable = true;
      #               manual.manpages.enable = false;
      #               systemd.user.startServices = "sd-switch";
      #               imports = [
      #                 inputs.impermanence.nixosModules.home-manager.impermanence
      #                 ./modules/users
      #                 ./users/actoriu
      #               ];
      #             };
      #           };
      #         }
      #         ./modules/nixos
      #         ./profiles/nixos
      #         ./hosts/d630
      #       ];
      #     };
      #   };

      #   homeConfigurations = {
      #     "actoriu@d630" = home-manager.lib.homeManagerConfiguration {
      #       pkgs = legacyPackages."x86_64-linux";
      #       extraSpecialArgs = { inherit inputs self; };
      #       modules = [
      #         ({ ... }: {
      #           nixpkgs = {
      #             inherit (legacyPackages."x86_64-linux") config overlays;
      #           };
      #         })
      #         inputs.impermanence.nixosModules.home-manager.impermanence
      #         {
      #           home = {
      #             username = "actoriu";
      #             homeDirectory = "/home/actoriu";
      #             stateVersion = "22.11";
      #           };
      #           programs.home-manager.enable = true;
      #           manual.manpages.enable = false;
      #           systemd.user.startServices = "sd-switch";
      #         }
      #         ./modules/users
      #         ./users/actoriu
      #       ];
      #     };
      #   };

      #   nixOnDroidConfigurations = {
      #     oneplus5 = nix-on-droid.lib.nixOnDroidConfiguration {
      #       system = "aarch64-linux";
      #       extraSpecialArgs = { inherit inputs self; };
      #       config = import ./hosts/oneplus5/default.nix;
      #     };
      #   };
    };
}
