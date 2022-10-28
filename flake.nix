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
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

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

    nix-formatter-pack = {
      url = "github:Gerschtli/nix-formatter-pack";
      inputs.nixpkgs.follows = "nixpkgs";
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
        flake-utils.follows = "flake-utils";
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
    , nix-on-droid
    , flake-utils
    , nix-formatter-pack
    , ...
    }@inputs:
    flake-utils.lib.eachSystem [ "aarch64-linux" "x86_64-linux" ]
      (system:
      let
        formatterPackArgs = {
          inherit nixpkgs system;
          checkFiles = [ ./. ];
          config.tools = {
            deadnix.enable = true;
            nixpkgs-fmt.enable = true;
            statix.enable = true;
          };
        };
      in
      {
        checks = {
          nix-formatter-pack-check = nix-formatter-pack.lib.mkCheck formatterPackArgs.${system};
        };

        formatter = nix-formatter-pack.lib.mkFormatter formatterPackArgs.${system};

        # packages = import ./pkgs { inherit pkgs; };

        devShells = {
          default = pkgs.devshell.mkShell {
            name = "nix-config";
            imports = [ (pkgs.devshell.extraModulesDir + "/git/hooks.nix") ];
            git.hooks.enable = true;
            git.hooks.pre-commit.text = "${pkgs.treefmt}/bin/treefmt";
            packages = with pkgs; [
              cachix
              nix-build-uncached
              nixpkgs-fmt
              nodePackages.prettier
              nodePackages.prettier-plugin-toml
              nvfetcher
              shfmt
              treefmt
            ];
            commands = with pkgs; [
              {
                category = "update";
                name = nvfetcher-bin.pname;
                help = nvfetcher-bin.meta.description;
                command = "cd $PRJ_ROOT/pkgs; ${nvfetcher-bin}/bin/nvfetcher -c ./sources.toml $@";
              }
            ];
            devshell.startup.nodejs-setuphook = pkgs.lib.stringsWithDeps.noDepEntry ''
              export NODE_PATH=${pkgs.nodePackages.prettier-plugin-toml}/lib/node_modules:$NODE_PATH
            '';
          };
        };
      })
    // {
      overlays = {
        # default = import ./overlays { inherit inputs; };
        devshell = inputs.devshell.overlay;
        nixos-cn = inputs.nixos-cn.overlay;
        nur = inputs.nur.overlay;
        peerix = inputs.peerix.overlay;
        sops-nix = inputs.sops-nix.overlay;
        spacemacs = final: prev: { spacemacs = inputs.spacemacs; };
      };

      nixosConfigurations = {
        d630 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            ({ ... }: {
              nixpkgs = {
                config = {
                  allowUnfree = true;
                  allowBroken = true;
                  allowUnsupportedSystem = true;
                };
                overlays = builtins.attrValues self.overlays;
              };
            })
            inputs.impermanence.nixosModules.impermanence
            inputs.nixos-cn.nixosModules.nixos-cn-registries
            inputs.nixos-cn.nixosModules.nixos-cn
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs; };
                users.actoriu = { ... }: {
                  home.stateVersion = "22.11";
                  programs.home-manager.enable = true;
                  manual.manpages.enable = false;
                  systemd.user.startServices = "sd-switch";
                  imports = [
                    inputs.impermanence.nixosModules.home-manager.impermanence
                    ./modules/users
                    ./users/actoriu
                  ];
                };
              };
            }
            ./modules/nixos
            ./profiles/nixos
            ./hosts/d630
          ];
        };
      };

      homeConfigurations = {
        "actoriu@d630" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ({ ... }: {
              nixpkgs = {
                config = {
                  allowUnfree = true;
                  allowBroken = true;
                  allowUnsupportedSystem = true;
                };
                overlays = builtins.attrValues self.overlays;
              };
            })
            inputs.impermanence.nixosModules.home-manager.impermanence
            {
              home = {
                username = "actoriu";
                homeDirectory = "/home/actoriu";
                stateVersion = "22.11";
              };
              programs.home-manager.enable = true;
              manual.manpages.enable = false;
              systemd.user.startServices = "sd-switch";
            }
            ./modules/users
            ./users/actoriu
          ];
        };
      };

      nixOnDroidConfigurations = {
        oneplus5 = nix-on-droid.lib.nixOnDroidConfiguration {
          system = "aarch64-linux";
          extraSpecialArgs = { inherit inputs; };
          config = { ... }: {
            nixpkgs = {
              config = {
                allowUnfree = true;
                allowBroken = true;
                allowUnsupportedSystem = true;
              };
              overlays = builtins.attrValues self.overlays;
            };
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = { inherit inputs; };
              config = { ... }: {
                home.stateVersion = "22.11";
                manual.manpages.enable = false;
                imports = [
                  ./modules/users
                  ./users/nix-on-droid
                ];
              };
            };
            imports = [
              ./hosts/oneplus5
            ];
          };
        };
      };

    };
}
