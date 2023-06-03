{
  description = "Nix configuration with flakes";

  /*
  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      # "https://mirrors.bfsu.edu.cn/nix-channels/store"
      # "https://mirrors.ustc.edu.cn/nix-channels/store"
      "https://cache.nixos.org"
    ];
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://pre-commit-hooks.cachix.org"
      "https://nix-actions.cachix.org"
      "https://nix-on-droid.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "nix-actions.cachix.org-1:0QFVMkhyCHpNBpxW1XyI8/+OGW81Vt1KpeeN/IdjEYg="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
    ];
  };
  */

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs = {
        nixpkgs-lib.follows = "nixpkgs";
      };
    };

    flake-root = {
      url = github:srid/flake-root;
    };

    mission-control = {
      url = github:Platonic-Systems/mission-control;
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    # cachix-deploy-flake = {
    #   url = "github:cachix/cachix-deploy-flake";
    #   inputs = {
    #     home-manager.follows = "home-manager";
    #     nixpkgs.follows = "nixpkgs";
    #   };
    # };

    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    disko = {
      url = "github:nix-community/disko";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    # guix-overlay = {
    #   url = "github:foo-dogsquared/nix-overlay-guix";
    #   inputs = {
    #     nixpkgs.follows = "nixpkgs";
    #   };
    # };

    haumea = {
      url = "github:nix-community/haumea";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    impermanence = {
      url = "github:nix-community/impermanence";
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };

    # nixos-generators = {
    #   url = "github:nix-community/nixos-generators";
    #   inputs = {
    #     nixpkgs.follows = "nixpkgs";
    #   };
    # };

    nur = {
      url = "github:nix-community/NUR";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
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

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = {
    self,
      # cachix-deploy-flake,
      flake-parts,
      nixpkgs,
      home-manager,
      ...
  } @ inputs: let
    # Use our custom lib enhanced with nixpkgs and hm one
    lib = import ./lib {
      inherit inputs;
      lib = nixpkgs.lib;
    } // nixpkgs.lib // home-manager.lib;

    stateversion = nixpkgs.lib.fileContents ./.version;
  in
    (flake-parts.lib.evalFlakeModule
      {
        inherit inputs;
        specialArgs = {inherit lib;};
      }
      {
        debug = true;
        systems = ["aarch64-linux" "x86_64-linux"];
        imports = [
          (_: {
            perSystem = {inputs', ...}: {
              # make pkgs available to all `perSystem` functions
              _module.args.pkgs = inputs'.nixpkgs.legacyPackages;
              # make custom lib available to all `perSystem` functions
              _module.args.lib = lib;
            };
          })
          # devshell.flakeModule
          # flake-root.flakeModule
          # mission-control.flakeModule
          # pre-commit-hooks.flakeModule
          # treefmt-nix.flakeModule
          ./flake-parts
        ];
      }).config.flake;

  /*
    devShells = forEachSystem (system: {
      default = let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [inputs.devshell.overlays.default];
        };
      in
        import ./shell/devshell.nix {inherit formatterPackArgsFor pkgs self system;};
    });
    */

  /*
    devShells = forEachSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          cachix
          nvfetcher
          (formatterPackArgsFor.${system})
        ];

        shellHook = ''
          ${config.pre-commit.installationScript}
          echo 1>&2 "Welcome to the development shell!"
        '';
      };
    });

    formatter = config.treefmt.build.wrapper;

    overlays = import ./overlays {inherit inputs;};

    packages = forEachSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        import ./pkgs {inherit pkgs;}
    );

    nixosConfigurations = {
      d630 = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs version;
          desktop = null;
          hostname = "d630";
          non-nixos = false;
          username = "actoriu";
          system = "x86_64-linux";
        };
        modules = [./hosts/nixos/shared];
      };
    };

    homeConfigurations = {
      "actoriu@d630" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = {
          inherit inputs outputs version;
          desktop = null;
          hostname = "d630";
          non-nixos = true;
          username = "actoriu";
          system = "x86_64-linux";
        };
        modules = [./users/shared];
      };
    };

    nixOnDroidConfigurations = {
      oneplus5 = nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import nixpkgs {
          system = "aarch64-linux";
          overlays = [
            nix-on-droid.overlays.default
          ];
        };
        extraSpecialArgs = {
          inherit inputs outputs version;
          devicename = "oneplus5";
          username = "nix-on-droid";
        };
        home-manager-path = home-manager.outPath;
        modules = [./hosts/droid/oneplus5];
      };
    };
    */
}
