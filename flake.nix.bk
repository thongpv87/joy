{
  description = "A very basic flake";

  inputs= {
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          hsProject =
            final.haskell-nix.stackProject' {
              name = "joy";
              src = ./.;
              shell = {
                tools = {
                  cabal = {};
                  haskell-language-server = {};
                };
                buildInputs = with pkgs;
                  [ stack protobuf haskellPackages.implicit-hie pkg-config zlib git haskellPackages.hls-eval-plugin
                    #SDL2 xorg.libXi xorg.libXrandr xorg.libXxf86vm xorg.libXcursor xorg.libXinerama xorg.libXext
                  ];
              };
            };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.hsProject.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."joy:exe:TUI";
    }
    );
}
