self: super:
{
  haskellPackages = super.haskell.packages.ghc842.override (args: {
    overrides = self: super_:
      let
        super = (args.overrides or (self: super: super)) self super_;
      in
        super // {
          #time = self.callPackage ./time.nix {};
        };
  });
}
