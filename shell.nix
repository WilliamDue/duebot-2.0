let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  unstable = import <nixos-unstable> { };
  compilerVersion = "ghc98"; 
  compiler = pkgs. haskell.packages."${compilerVersion}";
in
let pkg =
      compiler.developPackage {
        root = ./. ;
        overrides = self:  super: {
          discord-haskell = pkgs.haskell.lib. overrideCabal super.discord-haskell (old: {
            version = "1.18.0";
            src = pkgs.fetchurl {
              url = "https://hackage.haskell.org/package/discord-haskell-1.18.0/discord-haskell-1.18.0.tar.gz";
              sha256 = "sha256-DB9OstockuzDWVCXmvYExemGQQX4P1dMMZtq7CSkfTw=";
            };
            doCheck = false;
          });
        };
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv
            (with compiler;
              [ cabal-install
                ghcid
                haskell-language-server
              ]);
      };
in pkg
