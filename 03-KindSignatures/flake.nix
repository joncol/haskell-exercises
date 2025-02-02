{
  description = "exercise03";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlay = self: _: {
          hsPkgs = self.haskell-nix.project' rec {
            src = ./.;
            compiler-nix-name = "ghc8107";
            shell = {
              tools = {
                cabal = { };
                ghcid = { };
                haskell-language-server = { };
                hlint = { };
                hoogle = { };
                stylish-haskell = { };
              };

              # Non-Haskell shell tools.
              buildInputs = with pkgs; [ graphviz ];

              withHoogle = false;
            };
          };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay overlay ];
        };
        flake = pkgs.hsPkgs.flake { };
      in flake // {
        defaultPackage = flake.packages."exercise03:exe:exercise03";
      });
}
