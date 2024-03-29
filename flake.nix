{
  description = "Nix configuration with flakes";

  nixConfig = {
    experimental-features = ["nix-command" "flakes"];
    substituters = [
      "https://mirrors.cernet.edu.cn/nix-channels/store"
      "https://cache.nixos.org/"
    ];
    extra-substituters = [
      "https://nix-actions.cachix.org"
      "https://nix-community.cachix.org"
      "https://nix-on-droid.cachix.org"
      "https://pre-commit-hooks.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-actions.cachix.org-1:0QFVMkhyCHpNBpxW1XyI8/+OGW81Vt1KpeeN/IdjEYg="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
    ];
  };

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    flake-parts = {
      url = github:hercules-ci/flake-parts;
      inputs.nixpkgs-lib.follows = "nixpkgs";
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

    /*
    alacritty-theme = {
      url = "github:alacritty/alacritty-theme";
      flake = false;
    };
    */

    /*
    cachix-deploy-flake = {
      url = "github:cachix/cachix-deploy-flake";
      inputs = {
        disko.follows = "disko";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };

    chemacs2 = {
      url = "github:plexus/chemacs2";
      flake = false;
    };

    doom-emacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    */

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

    nix-doom-emacs = {
      url = "github:nix-community/nix-doom-emacs";
      # inputs = {
      #   flake-compat.follows = "flake-compat";
      #   flake-utils.follows = "flake-utils";
      #   nixpkgs.follows = "nixpkgs";
      # };
    };

    # guix-overlay = {
    #   url = "github:foo-dogsquared/nix-overlay-guix";
    #   inputs = {
    #     nixpkgs.follows = "nixpkgs";
    #   };
    # };

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

    /*
    nur = {
      url = "github:nix-community/NUR";
    };
    */

    /*
    nvimdots = {
      url = "github:ayamir/nvimdots";
    };
    */

    pre-commit-hooks-nix = {
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

    /*
    spacemacs = {
      url = "github:syl20bnr/spacemacs/develop";
      flake = false;
    };
    */

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["aarch64-linux" "x86_64-linux"];
      imports = [./flake-parts];
    };

  /*
  outputs = {
    # self,
    # nixpkgs,
    flake-parts,
    # flake-root,
    # mission-control,
    # home-manager,
    # nix-on-droid,
    # cachix-deploy-flake,
    # treefmt-nix,
    ...
  } @ inputs: let
    inherit (self) outputs;

    forEachSystem = nixpkgs.lib.genAttrs ["aarch64-linux" "x86_64-linux"];

    forEachPkgs = f: forEachSystem (system: f nixpkgs.legacyPackages.${system});

    # cachixDeployLibFor = forEachPkgs (pkgs: cachix-deploy-flake.lib);

    formatterPackArgsFor = forEachPkgs (pkgs: treefmt-nix.lib.evalModule pkgs ./flake-parts/treefmt.nix);

    # lib = nixpkgs.lib.extend (
    #   final: prev:
    #     {
    #       my = import ./lib {
    #         inherit inputs;
    #         lib = final;
    #       };
    #     }
    #     // nixpkgs.lib
    #     // home-manager.lib
    # );

    stateVersion = nixpkgs.lib.fileContents ./.version;
  in {
    # lib = lib.my;

    checks = forEachPkgs (pkgs: {
      formatting = formatterPackArgsFor.${pkgs.system}.config.build.check self;
      pre-commit-check = import ./flake-parts/pre-commit.nix {
        inherit formatterPackArgsFor inputs pkgs;
      };
    });

    # devShells = forEachPkgs (pkgs: import ./shell.nix {inherit pkgs;});

    devShells = forEachSystem (system: {
      default = let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
          };
          overlays = [inputs.devshell.overlays.default];
        };
      in
        import ./flake-parts/shell/devshell.nix {
          inherit formatterPackArgsFor pkgs self;
        };
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
        ${self.checks.${system}.pre-commit-check.shellHook}
        echo 1>&2 "Welcome to the development shell!"
      '';
    };
  });
  */

  /*
    formatter = forEachPkgs (pkgs: formatterPackArgsFor.${pkgs.system}.config.build.wrapper);

    overlays = import ./overlays {inherit inputs;};

    packages = forEachPkgs (pkgs: import ./pkgs {inherit pkgs;});

    nixosConfigurations = {
      d630 = nixpkgs.lib.nixosSystem {
        specialArgs = {
          inherit inputs outputs stateVersion;
          desktop = null;
          hostname = "d630";
          non-nixos = false;
          username = "actoriu";
          system = "x86_64-linux";
        };
        modules = [
          inputs.disko.nixosModules.disko
          inputs.impermanence.nixosModules.impermanence
          # inputs.nur.nixosModules.nur
          inputs.sops-nix.nixosModules.sops
          ./flake-parts/nixos.nix
        ];
      };
    };

    homeConfigurations = {
      "actoriu@d630" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = {
          inherit inputs outputs stateVersion;
          desktop = null;
          hostname = "d630";
          non-nixos = true;
          username = "actoriu";
          system = "x86_64-linux";
        };
        modules = [
          ./flake-parts/home.nix
        ];
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
          inherit inputs outputs stateVersion;
          hostname = "oneplus5";
          non-nixos = false;
          username = "nix-on-droid";
        };
        home-manager-path = home-manager.outPath;
        modules = [
          ./flake-parts/droid.nix
        ];
      };
    };
  };
  */
}
