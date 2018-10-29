# How to get Sha256 for a specific revision?
#
# There are two modes for getting resources into Nix: packed or unpacked
# `fetchurl` fetches a resource and leave it packed
# `fetchTarball` and `fetchFromGitHub` fetches a resource and unpacks it
#
# So for getting the right Sha256 you have to call
# `nix-prefetch-url <URL>` for packed resources
# `nix-prefetch-url --unpack <URL>` for unpacked resources
{ nixpkgsRev ? "8e2c3b93bae2d0bcc020f93ed0dcdb7a206eaf52"
, nixpkgsSha256 ? "1an7zr829vgmvknfiy9alv4m4w07q4l1a6w8qafhj79qx8ca4la0"
, cabalHashesRev ? "c7a535a9098ac8093a6d6263e2da567a3fab35ad"
, cabalHashesSha256 ? "1xwgb615jjs6gqnfrglpsm0y4mmwprz84midp1hplng0rnv5p9kp"
, servantClientGhcjsRev ? "544bb8184e1adbcc2359767c172b4922f8b2d650"
, servantClientGhcjsSha256 ? "0hkyim72sk0c1p7rwv0dggk3j8zlxpgkjl14skqkrm9yas88r5yn"
, misoRev ? "a9c4f0a3a6e7d87f9432dc6cf4b1d0c052bf7ef1"
, misoSha256 ? "1yq5y4vffslm6abnfnggp5m3k6vla6mzcwmhmm23l8yizsvdkwv2"
, hienixRev ? "e3113da93b479bec3046e67c0123860732335dd9"
, hienixSha256 ? "05rkzjvzywsg66iafm84xgjlkf27yfbagrdcb8sc9fd59hrzyiqk"
, cabalPlanRev ? "67d6b9b3f15fde3f3fc38d4bccc589d2e1a5420c"
, cabalPlanSha256 ? "1rl4xaln0akcx8n7vai6ajyp16v5bg7x23f1g0ly7cvi5mvi945w"
}:
let
  # do not load the version of nixpkgs that the users account points to
  # load the same specific version on all accounts instead
  # How to get the rev:
  #   - look at https://github.com/NixOS/nixpkgs for a revision that suits you.
  nix-pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha256;
  };
  pkgs = import nix-pkgs {};

  # sometimes the versions provided by nixpkgs are not the versions we need. In this case we can
  # use the function `callHackage` which accepts a hackage version number. nixpkgs comes with a list of
  # hackage version number that map to specific cabal builds.
  # Because hackage version numbers do not pin an exact cabal build of a package (in case of bugs a newer build 
  # can be published keeping the version number), this mapping can be outdated. To be sure to use the latest mapping
  # one can supply nix with a newer mapping. 
  # How to get the rev:
  #   - look at https://github.com/commercialhaskell/all-cabal-hashes in branch 'hackage' for a revision that suits you.
  cabal-hashes = builtins.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${cabalHashesRev}.tar.gz";
    sha256 = cabalHashesSha256;
  };

  # to get a specific version of miso, get it directly from GitHub
  miso-pkgs = pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = misoRev;
    sha256 = misoSha256;
  };

  # For using servant with GHCJS we need the newest version from GitHub
  servant-pkgs = pkgs.fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant";
    rev = servantClientGhcjsRev;
    sha256 = servantClientGhcjsSha256;
  };

  # I do not know why we need cabal-plan from GitHub
  cabal-plan-pkgs = pkgs.fetchFromGitHub {
    owner = "hvr";
    repo = "cabal-plan";
    rev = cabalPlanRev;
    sha256 =cabalPlanSha256;
  };

  # Haskell language server using the newest version
  hienix-pkgs = pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = hienixRev;
    sha256 =hienixSha256;
  };


  # cabal generates some directories and files that confuse nix
  # ignore them
  occ-src = pkgs.lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(pkgs.lib.hasPrefix ".ghc.environment." base) &&
         !(pkgs.lib.hasSuffix ".nix" base)
    );
    src = pkgs.lib.cleanSource ./.;
  };

  # these are all the packages and tools we need for a GHC based build
  ghcPackages = pkgs.haskell.packages.ghc822.override(old: {
    all-cabal-hashes = cabal-hashes;
    overrides = self: super: {
      # use local source and convert the cabal file to build and get the version
      occ = super.callCabal2nix "occ" occ-src {};
    };
  });

  # and these are tools with odd dependencies wie need for a GHC based build
  ghcOddTools = pkgs.haskell.packages.ghc822.override(old: {
    all-cabal-hashes = cabal-hashes;
    overrides = self: super: {
      base-compat = super.callHackage "base-compat" "0.9.3" {};
      cabal-plan = pkgs.haskell.lib.overrideCabal (
        super.callCabal2nix "cabal-plan" (cabal-plan-pkgs) {})
        { editedCabalFile = null; };
    };
  });
  

  # these are all the packages and tools we need for a GHCJS based build
  ghcjsPackages = pkgs.haskell.packages.ghcjs80.override(old: {
    all-cabal-hashes = cabal-hashes;
    overrides = self: super: {
      # use local source and convert the cabal file to build and get the version
      occ = super.callCabal2nix "occ" occ-src {};

      # use the versions and the versioning scheme from hackage 
      http-types = super.callHackage "http-types" "0.11" {};
      servant = super.callHackage "servant" "0.12.1" {};
      servant-client-ghcjs = pkgs.haskell.lib.doJailbreak (super.callCabal2nix "servant-client-ghcjs" (servant-pkgs + /servant-client-ghcjs) {});
      servant-client-core = super.callHackage "servant-client-core" "0.12" {};

      # use source from github and convert the cabal file to build and get the version
      miso = super.callCabal2nix "miso" miso-pkgs {};
    };
  });

  hienix = import hienix-pkgs {};

in rec
{ 
  # server build
  server = pkgs.haskell.lib.justStaticExecutables ghcPackages.occ;

  # sever shell for working with GHC
  server-shell = ghcPackages.shellFor {
    packages = p: [p.occ];
    buildInputs = [pkgs.cabal-install ghcOddTools.cabal-plan ghcPackages.brittany];
  };

  # client build
  client = ghcjsPackages.occ;

  # client shell for working with GHCJS
  client-shell = ghcjsPackages.shellFor {
    packages = p: [p.occ];
    buildInputs = [pkgs.cabal-install ghcOddTools.cabal-plan ghcPackages.brittany hienix.hie80];
  };
}
