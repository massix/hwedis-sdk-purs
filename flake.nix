{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.purescript-overlay.url = "github:thomashoneyman/purescript-overlay";

  outputs = { nixpkgs, purescript-overlay, ... }:
    let
      system = "x86_64-linux";
      overlays = [ purescript-overlay.overlays.default ];
      pkgs = import nixpkgs {
        inherit system overlays;
        config.allowBroken = true;
      };
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = with pkgs; [
          spago-unstable
          purs-tidy-bin.purs-tidy-0_10_0
          purs-backend-es
          purs

          pkgs.nodejs-18_x
          pkgs.esbuild
        ];
      };
    };
}
