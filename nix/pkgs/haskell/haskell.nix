############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, rPackages
, haskell-nix
, gitignore-nix
, z3
, R
, libsodium-vrf
, checkMaterialization
, compiler-nix-name
, enableHaskellProfiling
  # Whether to set the `defer-plugin-errors` flag on those packages that need
  # it. If set to true, we will also build the haddocks for those packages.
, deferPluginErrors
, actus-tests
, source-repo-override
}:
let
  r-packages = with rPackages; [ R tidyverse dplyr stringr MASS plotly shiny shinyjs purrr ];
  project = haskell-nix.cabalProject' ({ pkgs, ... }: {
    inherit compiler-nix-name;
    # This is incredibly difficult to get right, almost everything goes wrong, see https://github.com/input-output-hk/haskell.nix/issues/496
    src = let root = ../../../.; in
      haskell-nix.haskellLib.cleanSourceWith {
        filter = gitignore-nix.gitignoreFilter root;
        src = root;
        # Otherwise this depends on the name in the parent directory, which reduces caching, and is
        # particularly bad on Hercules, see https://github.com/hercules-ci/support/issues/40
        name = "plutus";
      };
    # These files need to be regenerated when you change the cabal files.
    # See ../CONTRIBUTING.doc for more information.
    # Unfortuntely, they are *not* constant across all possible systems, so in some circumstances we need different sets of files
    # At the moment, we only need one but conceivably we might need one for darwin in future.
    # See https://github.com/input-output-hk/nix-tools/issues/97
    materialized =
      if source-repo-override != { } then null
      else if pkgs.stdenv.hostPlatform.isLinux then ./materialized-linux
      else if pkgs.stdenv.hostPlatform.isDarwin then ./materialized-darwin
      else if pkgs.stdenv.hostPlatform.isWindows then ./materialized-windows
      else builtins.error "Don't have materialized files for this platform";
    # If true, we check that the generated files are correct. Set in the CI so we don't make mistakes.
    inherit checkMaterialization source-repo-override;
    sha256map = {
      "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
      "https://github.com/input-output-hk/cardano-base"."4ea7e2d927c9a7f78ddc69738409a5827ab66b98" = "0n0hxbr0l95cdc25jmmgs7apmmw17i91chhj5rzzv1k7f3iymf6d";
      "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "06sdx5ndn2g722jhpicmg96vsrys89fl81k8290b3lr6b1b0w4m3";
      "https://github.com/input-output-hk/cardano-ledger"."bf008ce028751cae9fb0b53c3bef20f07c06e333" = "0my3801w1vinc0kf5yh9lxl6saqxgwm6ccg0vvzi104pafcwwcqx";
      "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "02jddik1yw0222wd6q0vv10f7y8rdgrlqaiy83ph002f9kjx7mh6";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "1il8fx3misp3650ryj368b3x95ksz01zz3x0z9k00807j93d0ka0";
      "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
      "https://github.com/input-output-hk/ouroboros-network"."1f4973f36f689d6da75b5d351fb124d66ef1057d" = "186056rvzdzy4jhvamjjbcmjyr94hs5hcyr8x6a0ch21hv5f014p";
      "https://github.com/input-output-hk/cardano-node.git"."b6ca519f97a0e795611a63174687e6bb70c9f752" = "0z5lpmqc98fwg3xzpzxkfslbxdjwfyyw8bn8yq0574sf4942vqdn";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" = "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
      "https://github.com/input-output-hk/cardano-wallet"."ae7569293e94241ef6829139ec02bd91abd069df" = "1mv1dhpkdj9ridm1fvq6jc85qs6zvbp172228rq72gyawjwrgvi6";
      "https://github.com/input-output-hk/cardano-addresses"."d2f86caa085402a953920c6714a0de6a50b655ec" = "0p6jbnd7ky2yf7bwb1350k8880py8dgqg39k49q02a6ij4ld01ay";
      "https://github.com/input-output-hk/plutus"."3f089ccf0ca746b399c99afe51e063b0640af547" = "1nx8xmdgwmnsla4qg4k67f5md8vm3p1p9i25ndalrqdg40z90486";
      "https://github.com/input-output-hk/plutus-apps"."404af7ac3e27ebcb218c05f79d9a70ca966407c9" = "00pv5ds99lf6lmws3a3ipsn9amg56ayc9b0wqki2gky464dm6gzr";
      "https://github.com/j-mueller/cardano-wallet"."6be73ab852c0592713dfe78218856d4a8a0ee69e" = "0rx5hvmbdv5dwb4qq39vyhisj0v75j21jbiivn3s3q9za6m6x1p4";
    };
    # Configuration settings needed for cabal configure to work when cross compiling
    # for windows. We can't use `modules` for these as `modules` are only applied
    # after cabal has been configured.
    cabalProjectLocal = lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
      -- When cross compiling for windows we don't have a `ghc` package, so use
      -- the `plutus-ghc-stub` package instead.
      package plutus-tx-plugin
        flags: +use-ghc-stub

      -- Exlcude test that use `doctest`.  They will not work for windows
      -- cross compilation and `cabal` will not be able to make a plan.
      package marlowe
        tests: False
      package prettyprinter-configurable
        tests: False
    '';
    modules = [
      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        packages = {
          # Things that need plutus-tx-plugin
          marlowe.package.buildable = false; # Would also require libpq
          marlowe-actus.package.buildable = false;
          marlowe-cli.package.buildable = false;
          marlowe-dashboard-server.package.buildable = false;
          marlowe-playground-server.package.buildable = false; # Would also require libpq
          marlowe-symbolic.package.buildable = false;
          playground-common.package.buildable = false;
          plutus-benchmark.package.buildable = false;
          plutus-chain-index-core.package.buildable = false;
          plutus-contract.package.buildable = false;
          plutus-errors.package.buildable = false;
          plutus-ledger.package.buildable = false;
          plutus-pab.package.buildable = false;
          plutus-playground-server.package.buildable = false; # Would also require libpq
          plutus-tx-plugin.package.buildable = false;
          plutus-use-cases.package.buildable = false;
          web-ghc.package.buildable = false;
          # These need R
          plutus-core.components.benchmarks.cost-model-test.buildable = lib.mkForce false;
          plutus-core.components.benchmarks.update-cost-model.buildable = lib.mkForce false;
        };
      })
      ({ pkgs, ... }:
        let
          # Add symlinks to the DLLs used by executable code to the `bin` directory
          # of the components with we are going to run.
          # We should try to find a way to automate this will in haskell.nix.
          symlinkDlls = ''
            ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
            ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libgcc_s_seh-1.dll $out/bin/libgcc_s_seh-1.dll
            ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libstdc++-6.dll $out/bin/libstdc++-6.dll
            ln -s ${pkgs.windows.mcfgthreads}/bin/mcfgthread-12.dll $out/bin/mcfgthread-12.dll
          '';
        in
        lib.mkIf (pkgs.stdenv.hostPlatform.isWindows) {
          packages = {
            # Add dll symlinks to the compoents we want to run.
            plutus-core.components.tests.plutus-core-test.postInstall = symlinkDlls;
            plutus-core.components.tests.plutus-ir-test.postInstall = symlinkDlls;
            plutus-core.components.tests.untyped-plutus-core-test.postInstall = symlinkDlls;
            plutus-ledger-api.components.tests.plutus-ledger-api-test.postInstall = symlinkDlls;

            # These three tests try to use `diff` and the following could be used to make the
            # linux version of diff available.  Unfortunately the paths passed to it are windows style.
            # plutus-core.components.tests.plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
            # plutus-core.components.tests.plutus-ir-test.build-tools = [ pkgs.buildPackages.diffutils ];
            # plutus-core.components.tests.untyped-plutus-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
            plutus-core.components.tests.plutus-core-test.buildable = lib.mkForce false;
            plutus-core.components.tests.plutus-ir-test.buildable = lib.mkForce false;
            plutus-core.components.tests.untyped-plutus-core-test.buildable = lib.mkForce false;
          };
        }
      )
      ({ pkgs, config, ... }: {
        packages = {
          # See https://github.com/input-output-hk/plutus/issues/1213 and
          # https://github.com/input-output-hk/plutus/pull/2865.
          marlowe.doHaddock = deferPluginErrors;
          marlowe.flags.defer-plugin-errors = deferPluginErrors;

          plutus-contract.doHaddock = deferPluginErrors;
          plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

          plutus-use-cases.doHaddock = deferPluginErrors;
          plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

          plutus-ledger.doHaddock = deferPluginErrors;
          plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

          # Packages we just don't want docs for
          plutus-benchmark.doHaddock = false;
          # FIXME: Haddock mysteriously gives a spurious missing-home-modules warning
          plutus-tx-plugin.doHaddock = false;

          # Fix missing executables on the paths of the test runners. This is arguably
          # a bug, and the fix is a bit of a hack.
          marlowe.components.tests.marlowe-test.preCheck = ''
            PATH=${lib.makeBinPath [ z3 ]}:$PATH
          '';

          # Relies on cabal-doctest, just turn it off in the Nix build
          prettyprinter-configurable.components.tests.prettyprinter-configurable-doctest.buildable = lib.mkForce false;

          plutus-core.components.benchmarks.update-cost-model = {
            build-tools = r-packages;
            # Seems to be broken on darwin for some reason
            platforms = lib.platforms.linux;
          };

          plutus-core.components.benchmarks.cost-model-test = {
            build-tools = r-packages;
            # Seems to be broken on darwin for some reason
            platforms = lib.platforms.linux;
          };

          marlowe-actus.components.exes.marlowe-shiny = {
            build-tools = r-packages;
            # Seems to be broken on darwin for some reason
            platforms = lib.platforms.linux;
          };

          # The marlowe-actus tests depend on external data which is
          # provided from Nix (as niv dependency)
          marlowe-actus.components.tests.marlowe-actus-test.preCheck = ''
            export ACTUS_TEST_DATA_DIR=${actus-tests}/tests/
          '';


          # Note: The following two statements say that these tests should
          # _only_ run on linux. In actual fact we just don't want them
          # running on the 'mac-mini' instances, because these tests time out
          # there. In an ideal world this would be reflected here more
          # accurately.
          # TODO: Resolve this situation in a better way.
          marlowe.components.tests.marlowe-test-long-running = {
            platforms = lib.platforms.linux;
          };

          plutus-pab.components.tests.plutus-pab-test-full-long-running = {
            platforms = lib.platforms.linux;
          };

          # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
          iohk-monitoring.doHaddock = false;

          # Werror everything. This is a pain, see https://github.com/input-output-hk/haskell.nix/issues/519
          plutus-core.ghcOptions = [ "-Werror" ];
          marlowe.ghcOptions = [ "-Werror" ];
          marlowe-symbolic.ghcOptions = [ "-Werror" ];
          marlowe-actus.ghcOptions = [ "-Werror" ];
          marlowe-playground-server.ghcOptions = [ "-Werror" ];
          marlowe-dashboard-server.ghcOptions = [ "-Werror" ];
          playground-common.ghcOptions = [ "-Werror" ];
          plutus-contract.ghcOptions = [ "-Werror" ];
          plutus-ledger.ghcOptions = [ "-Werror" ];
          plutus-ledger-api.ghcOptions = [ "-Werror" ];
          plutus-playground-server.ghcOptions = [ "-Werror" ];
          plutus-pab.ghcOptions = [ "-Werror" ];
          plutus-tx.ghcOptions = [ "-Werror" ];
          plutus-tx-plugin.ghcOptions = [ "-Werror" ];
          plutus-doc.ghcOptions = [ "-Werror" ];
          plutus-use-cases.ghcOptions = [ "-Werror" ];

          # External package settings

          inline-r.ghcOptions = [ "-XStandaloneKindSignatures" ];

          # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
          # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
          ieee.components.library.libs = lib.mkForce [ ];

          # See https://github.com/input-output-hk/iohk-nix/pull/488
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
        };
      })
    ] ++ lib.optional enableHaskellProfiling {
      enableLibraryProfiling = true;
      enableExecutableProfiling = true;
    };
  });

in
project
