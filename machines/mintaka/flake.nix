{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.home-manager = {
    url = "github:nix-community/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.my-fonts = {
    url = "git+ssh://git@github.com/benley/my-fonts.git";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {self, nixpkgs, ...}@inputs: {
    nixosConfigurations.mintaka = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [ ./configuration.nix ];
    };
  };
}
