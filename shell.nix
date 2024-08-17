let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  unstable = import <nixos-unstable> { };
  compilerVersion = "ghc96"; 
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
let pkg =
      compiler.developPackage {
        root = ./.;
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv
            (with pkgs.haskellPackages;
              [ cabal-install
                ghcid
                haskell-language-server
              ]);
      };
in pkg
