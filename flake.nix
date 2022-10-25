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
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-actions.cachix.org-1:WTp8/9EIjoPRzwSERLLMHzDUVGthajaIJ/zEZY6DHvM="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
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
    let
      inherit (nixpkgs.lib) filterAttrs traceVal;
      inherit (builtins) mapAttrs elem;
      inherit (self) outputs;
      notBroken = x: !(x.meta.broken or false);
      inherit (flake-utils.lib) eachDefaultSystem eachSystem;
    in
    {
      overlays = {
        default = import ./overlays { inherit inputs; };
        devshell = inputs.devshell.overlay;
        nixos-cn = inputs.nixos-cn.overlay;
        nur = inputs.nur.overlay;
        nvfetcher = inputs.nvfetcher.overlay;
        peerix = inputs.peerix.overlay;
        sops-nix = inputs.sops-nix.overlay;
        spacemacs = final: prev: { spacemacs = inputs.spacemacs; };
      };

      legacyPackages = eachDefaultSystem (system:
        import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
            allowUnsupportedSystem = true;
          };
          overlays = builtins.attrValues overlays;
        }
      );

      formatterPackArgsPerSystem = eachDefaultSystem (system: {
        inherit nixpkgs system;
        pkgs = legacyPackages.${system};
        checkFiles = [ ./. ];
        config = {
          tools = {
            deadnix.enable = true;
            nixpkgs-fmt.enable = true;
            statix.enable = true;
          };
        };
      });

      checks = eachDefaultSystem (system: {
        nix-formatter-pack-check = nix-formatter-pack.lib.mkCheck formatterPackArgsFor.${system};
      });

      formatter = eachDefaultSystem (system:
        nix-formatter-pack.lib.mkFormatter formatterPackArgsFor.${system});

      packages = eachDefaultSystem (system:
        import ./pkgs { pkgs = legacyPackages.${system}; }
      );

      devShells = eachDefaultSystem (system:
        let
          pkgs = legacyPackages.${system};
        in
        {
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
        });

      hydraJobs = {
        packages = mapAttrs (sys: filterAttrs (_: pkg: (elem sys pkg.meta.platforms && notBroken pkg))) packages;
        nixos = mapAttrs (_: cfg: cfg.config.system.build.toplevel) nixosConfigurations;
        home = mapAttrs (_: cfg: cfg.activationPackage) homeConfigurations;
      };

      nixosConfigurations = {
        d630 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          pkgs = legacyPackages."x86_64-linux";
          specialArgs = { inherit inputs outputs; };
          modules = [
            inputs.impermanence.nixosModules.impermanence
            inputs.nixos-cn.nixosModules.nixos-cn-registries
            inputs.nixos-cn.nixosModules.nixos-cn
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs outputs; };
                users.actoriu = { ... }: {
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
          pkgs = legacyPackages."x86_64-linux";
          extraSpecialArgs = { inherit inputs outputs; };
          modules = [
            inputs.impermanence.nixosModules.home-manager.impermanence
            ./modules/users
            ./users/actoriu
          ];
        };
      };

      nixOnDroidConfigurations = {
        oneplus5 = nix-on-droid.lib.nixOnDroidConfiguration {
          system = "aarch64-linux";
          extraSpecialArgs = { inherit inputs outputs; };
          config = { ... }: {
            nixpkgs = { inherit (legacyPackages."aarch64-linux") config overlays; };
            imports = [
              {
                home-manager = {
                  useGlobalPkgs = true;
                  useUserPackages = true;
                  extraSpecialArgs = { inherit inputs outputs; };
                  config = { ... }: {
                    home.stateVersion = "22.11";
                    manual.manpages.enable = false;
                    imports = [
                      ./modules/users
                      ./users/nix-on-droid
                    ];
                  };
                };
              }
              ./hosts/oneplus5
            ];
          };
        };
      };

    };
}
