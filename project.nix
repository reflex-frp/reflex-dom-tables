{ config, nix-haskell-patches, ... }:

let nix-thunk = config.importing.nix-thunk;
    deps = with nix-thunk; mapSubdirectories thunkSource ./deps;

in {
  imports = [
    "${nix-haskell-patches}/js/splitmix"
  ];

  source-repository-packages = {
    "reflex-dom-attrs" = deps.reflex-dom-attrs;
  };

  name = "reflex-dom-tables";
  src = ./.;
  compiler-nix-name = "ghc912";

  shell = {
    crossPlatforms = ps: with ps; [ ghcjs ];
    packages = ps: with ps; [ reflex-dom-tables ];
    withHaddock = false;
    withHoogle = false;
  };
}
