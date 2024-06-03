{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    my-fonts = {
      url = "git+ssh://git@github.com/benley/my-fonts.git";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    memsql-provisioning = {
      url = "git+ssh://git@gitlab.cloud.memcompute.com/cloud/provisioning.git?ref=nix-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = {self, nixpkgs, ...}@inputs: {
    nixosConfigurations.mintaka = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [ ./machines/mintaka/configuration.nix ];
    };

    nixosConfigurations.alnilam = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [ ./machines/alnilam/configuration.nix ];
    };

    nixosConfigurations.ein = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [ ./machines/ein/configuration.nix ];
    };
  };
}
