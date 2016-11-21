{ pkgs ? (import <nixpkgs> {}), reflex-platform, try-reflex }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    reflex-dom-contrib = self.callPackage (reflex-platform.cabal2nixResult (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom-contrib";
        rev = "02a5cf4fe63ff74faf96f11a9bb9ecdf6ee7f0e5";
        sha256 = "09iaq9xgqsb3vwixhna2wa94kz28plrynk9nkhqvlz1kgn02nqv2";
      })) {};
    reflex-dom-semui = self.callPackage (reflex-platform.cabal2nixResult (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom-semui";
        rev = "fc1fceff46ad677cdede13cad8c6db6b981fc0cb";
        sha256 = "0dg8cpxgchazkqwhns7lm99k1j40zd04p18l4fi8vlw3v2mqi4a7";
      })) {};
  };
}
