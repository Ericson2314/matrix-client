{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, hackGet, ... }:
let
  source = {
    beam = hackGet ./dep/beam;
  };
in {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  packages = {
    beam-core = source.beam + /beam-core;
    beam-keyed = hackGet ./dep/beam-keyed;
    beam-migrate = source.beam + /beam-migrate;
    beam-migrate-cli = source.beam + /beam-migrate-cli;
    beam-postgres = source.beam + /beam-postgres;
    beam-sqlite = source.beam + /beam-sqlite;
    constraints-extras = hackGet ./dep/constraints-extras;
    dependent-monoidal-map = hackGet ./dep/dependent-monoidal-map;
    dependent-sum-aeson-orphans = hackGet ./dep/dependent-sum-aeson-orphans;
    semantic-reflex = (hackGet ./dep/semantic-reflex) + /semantic-reflex;
    vessel = hackGet ./dep/vessel;
  };
  overrides = self: super: {
    beam-migrate = pkgs.haskell.lib.doJailbreak super.beam-migrate;
    # TODO: Upstream these version bounds.
    dependent-sum-aeson-orphans = pkgs.haskell.lib.doJailbreak super.dependent-sum-aeson-orphans;
    dependent-monoidal-map = pkgs.haskell.lib.doJailbreak super.dependent-monoidal-map;
    semantic-reflex = pkgs.haskell.lib.dontHaddock super.semantic-reflex;
  };
})
