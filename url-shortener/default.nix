{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
let deps = obelisk.nixpkgs.thunkSet ./dep;
    haskellLib = obelisk.nixpkgs.haskell.lib;
in with obelisk;
project ./. ({ ... }: {
  packages = {
    database-id-class = deps.database-id + /class;
    database-id-obelisk = deps.database-id + /obelisk;
  };
  overrides = self: super: {
    database-id-class = haskellLib.doJailbreak super.database-id-class;
    database-id-obelisk = haskellLib.doJailbreak super.database-id-obelisk;
  };
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
