{ config, nix-haskell-patches, ... }:

let nix-thunk = config.importing.nix-thunk;
    deps = with nix-thunk; mapSubdirectories thunkSource ./deps;

in {
  imports = [
    "${nix-haskell-patches}/js/splitmix"
  ];

  source-repository-packages = {
    "higher-kinded" = deps.higher-kinded + "/higher-kinded";
    "higher-kinded-data" = deps.higher-kinded + "/higher-kinded-data";
    "higher-kinded-types" = deps.higher-kinded + "/higher-kinded-types";
    "higher-kinded-instance-base" = deps.higher-kinded + "/higher-kinded-instance-base";

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
