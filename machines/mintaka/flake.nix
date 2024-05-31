{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.home-manager = {
    url = github:nix-community/home-manager;
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.my-fonts = {
    url = "path:/home/benley/p/my-fonts";
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
