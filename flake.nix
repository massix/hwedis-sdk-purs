{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.easy-purescript.url = "github:justinwoo/easy-purescript-nix";

  outputs = { nixpkgs, easy-purescript, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      easy-ps = easy-purescript.packages.${system};
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = with easy-ps; [
          purescript
          spago
          purs-tidy
          purs

          pkgs.nodejs-18_x
          pkgs.esbuild
        ];
      };
    };
}
