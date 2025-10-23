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
  };

  outputs = {self, nixpkgs, ...}@inputs: {
    nixosConfigurations.chungus = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [ ./configuration.nix ];
    };
  };
}
