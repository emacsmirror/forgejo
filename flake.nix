{
  description = "Forgejo client for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  # Exact Package-Requires minimum, independent of the project's default pin.
  inputs.nixpkgs-emacs291.url = "github:NixOS/nixpkgs/057f9aecfb71c4437d2b27d3323df7f93c010b7e";

  outputs = { self, nixpkgs, nixpkgs-emacs291 }:
    let
      systems = [
        # Note: Most of the testing I've done is x86_64-linux and
        # aarch64-linux.  If you find any issues with darwin feel free
        # to report/submit a PR.
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkForgejo = system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacs = pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          minimum = (import nixpkgs-emacs291 { inherit system; }).emacs29-nox;
          manifest = lib.splitString "\n" (lib.removeSuffix "\n"
            (builtins.readFile ./admin/sources));
          # Check recursively before filtering; omitted sources must not disappear.
          lispFiles = map (path: lib.removePrefix (toString ./. + "/") (toString path))
            (lib.filter (path: lib.hasSuffix ".el" (toString path))
              (lib.filesystem.listFilesRecursive ./tests ++ lib.filesystem.listFilesRecursive ./lisp));
          source = assert lib.assertMsg (lib.all (file: builtins.elem file manifest) lispFiles)
            "Lisp input missing from admin/sources";
            lib.fileset.toSource {
              root = ./.;
              fileset = lib.fileset.unions ([ ./Makefile ] ++ map (file: ./. + "/${file}") manifest);
            };

          keymapPopupVersion = "0.3.1";

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = pkgs.fetchurl {
              url = "https://elpa.gnu.org/packages/keymap-popup-${keymapPopupVersion}.tar.lz";
              hash = "sha256-gljSXx0mrtFL+ep5MqRaG01benpUhlyn7CB1Qb50ONw=";
            };
            nativeBuildInputs = [ pkgs.lzip ];
            packageRequires = [ ];
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            keymapPopup
            epkgs.markdown-mode
          ]);

          # Share dependency sources, never default-runtime bytecode.
          testDependencies = pkgs.runCommand "forgejo-test-dependencies" { } ''
            mkdir -p $out
            find -L ${keymapPopup} ${emacsPackages.markdown-mode} -name '*.el' -type f \
              -exec cp '{}' $out/ \;
          '';
          matrixTools = [ pkgs.python3 pkgs.gnumake pkgs.git pkgs.sqlite ];
          matrixShell = runtime: pkgs.mkShellNoCC {
            packages = matrixTools ++ [ runtime ];
            FORGEJO_MATRIX_DEPS = testDependencies;
            FORGEJO_MATRIX_VERSION = runtime.version;
          };
          matrixCheck = name: runtime: pkgs.stdenvNoCC.mkDerivation {
            pname = "forgejo-matrix-${name}";
            version = "git";
            src = source;
            nativeBuildInputs = matrixTools ++ [ runtime ];
            FORGEJO_MATRIX_DEPS = testDependencies;
            dontConfigure = true;
            buildPhase = ''
              python3 admin/test-matrix --lane ${name} --expected ${runtime.version} \
                --root "$TMPDIR/lane"
            '';
            installPhase = ''
              mkdir -p $out
              cp -r "$TMPDIR/lane/ert" "$TMPDIR/lane/runtime.json" "$TMPDIR/lane/passed" $out/
            '';
          };

          tests = pkgs.stdenv.mkDerivation {
            pname = "emacs-forgejo-tests";
            version = "git";
            src = source;
            nativeBuildInputs = [
              emacsWithPackages
              pkgs.gnumake
              pkgs.python3
              pkgs.git
            ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild
              unset EMACSDATA EMACSDOC EMACSLOADPATH EMACSPATH GREP_OPTIONS
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/share"
              export XDG_STATE_HOME="$TMPDIR/state"
              mkdir -p "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" \
                "$XDG_DATA_HOME" "$XDG_STATE_HOME"
              export FORGEJO_TEST_RESULTS="$TMPDIR/ert" FORGEJO_FULL_SUITE=1
              EMACS_CMD=emacs \
                FORGEJO_ENV_WRAPPED=1 \
                make dev
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              cp -r "$TMPDIR/ert" $out/
              touch $out/tests-passed
              runHook postInstall
            '';
          };
        in {
          inherit emacs emacsWithPackages keymapPopup pkgs tests source minimum matrixShell matrixCheck testDependencies;
        };
    in {
      checks = forAllSystems (system:
        let forgejo = mkForgejo system;
        in {
          test = forgejo.tests;
          matrix-minimum = forgejo.matrixCheck "minimum" forgejo.minimum;
          matrix-default = forgejo.matrixCheck "default" forgejo.emacs;
          matrix-runner = forgejo.pkgs.runCommand "forgejo-matrix-runner" {
            nativeBuildInputs = [ forgejo.pkgs.python3 forgejo.pkgs.gnumake forgejo.emacs ];
            FORGEJO_MATRIX_DEPS = forgejo.testDependencies;
          } ''
            python3 ${forgejo.source}/admin/test-matrix-runner.py
            touch $out
          '';
        });

      devShells = forAllSystems (system:
        let forgejo = mkForgejo system;
        in {
          matrix-minimum = forgejo.matrixShell forgejo.minimum;
          matrix-default = forgejo.matrixShell forgejo.emacs;
          default = forgejo.pkgs.mkShell {
            packages = with forgejo.pkgs; [
              cacert
              forgejo.emacsWithPackages
              git
              gnumake
              python3
              sqlite
            ];

            shellHook = ''
              export EMACS_CMD=emacs
            '';
          };
        });
    };
}
