{
  description = "Nix configuration with flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    nixos-latest.url = "github:NixOS/nixpkgs/master";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils = { url = "github:numtide/flake-utils"; };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        flake-compat.follows = "flake-compat";
        utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    guix-overlay = {
      url = "github:foo-dogsquared/nix-overlay-guix";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };

    impermanence = { url = "github:nix-community/impermanence"; };

    neovim-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
      };
    };

    nixos-cn = {
      url = "github:nixos-cn/flakes";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };

    nix-on-droid = {
      url = "github:t184256/nix-on-droid";
      inputs = {
        flake-utils.follows = "flake-utils";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };

    # nixos-hardware = {
    #   url = "github:NixOS/nixos-hardware";
    # };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };

    nur = { url = "github:nix-community/NUR"; };

    nvfetcher = {
      url = "github:berberman/nvfetcher";
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    ragenix = {
      url = "github:yaxitech/ragenix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };

    spacemacs = {
      url = "github:syl20bnr/spacemacs/develop";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs: {
    # nixos
    nixosConfigurations = import ./nix/nixos.nix { inherit self inputs; };

    # home-manager
    homeConfigurations = import ./nix/home-manager.nix { inherit self inputs; };
    # actoriu = self.homeConfigurations.actoriu.activationPackage;
    # packages.x86_64-linux.default = self.homeConfigurations.actoriu.activationPackage;

    # nix-on-droid
    nixOnDroidConfigurations = import ./nix/nix-on-droid.nix { inherit self inputs; };
  };
}
