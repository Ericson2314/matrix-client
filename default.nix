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
    semantic-reflex = (hackGet ./dep/semantic-reflex) + /semantic-reflex;
  };
  overrides = self: super: {
    semantic-reflex = pkgs.haskell.lib.dontHaddock super.semantic-reflex;
  };
})
