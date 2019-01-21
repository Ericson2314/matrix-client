{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  packages = {
    constraints-extras = hackGet ./dep/constraints-extras;
    dependent-map = hackGet ./dep/dependent-map;
    dependent-monoidal-map = hackGet ./dep/dependent-monoidal-map;
    dependent-sum-aeson-orphans = hackGet ./dep/dependent-sum-aeson-orphans;
    # TODO: Remove once https://github.com/reflex-frp/reflex/pull/245 is in obelisk.
    reflex = hackGet ./dep/reflex;
    reflex-dom-forms = hackGet ./dep/reflex-dom-forms;
    semantic-reflex = (hackGet ./dep/semantic-reflex) + /semantic-reflex;
    vessel = hackGet ./dep/vessel;
  };
  overrides = self: super: {
    # TODO: Upstream these version bounds.
    dependent-sum-aeson-orphans = pkgs.haskell.lib.doJailbreak super.dependent-sum-aeson-orphans;
    dependent-monoidal-map = pkgs.haskell.lib.doJailbreak super.dependent-monoidal-map;
    # TODO: Remove once upstream is used via obelisk.
    reflex = pkgs.haskell.lib.dontCheck super.reflex;
    semantic-reflex = pkgs.haskell.lib.dontHaddock super.semantic-reflex;
  };
})
