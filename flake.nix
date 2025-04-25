{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    ocaml-overlay.url = "github:nix-ocaml/nix-overlays";
    ocaml-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      treefmt-nix,
      ocaml-overlay,
      systems,
    }:
    let
      lib = nixpkgs.lib;
      lang-name = "parrot";
      eachSystem =
        f:
        nixpkgs.lib.genAttrs (import systems) (
          system:
          f (
            import nixpkgs {
              inherit system;
              overlays = [ ocaml-overlay.overlays.default ];
            }
          )
        );
      treefmtEval = eachSystem (pkgs: treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
    in
    {
      packages = eachSystem (pkgs: {
        default = pkgs.ocamlPackages.buildDunePackage {
          pname = lang-name;
          version = "0.1.0";
          src = ./.;

          buildInputs = with pkgs.ocamlPackages; [
            extlib
            bisect_ppx
            ounit2
            ppxlib
            qcheck
            menhir
            ppx_variants_conv
            ppx_deriving
            visitors
            zarith
          ];

          preBuild = ''
            export DUNE_CACHE=disabled
          '';
        };
      });

      formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);

      checks = eachSystem (pkgs: {
        project =
          let
            patchDuneCommand =
              let
                subcmds = [
                  "build"
                  "test"
                  "runtest"
                  "install"
                ];
              in
              lib.replaceStrings (lib.lists.map (subcmd: "dune ${subcmd}") subcmds) (
                lib.lists.map (subcmd: "dune ${subcmd} --display=short") subcmds
              );
          in

          self.packages.${pkgs.system}.default.overrideAttrs (oldAttrs: {
            name = "check-${oldAttrs.name}";
            doCheck = true;
            buildPhase = patchDuneCommand oldAttrs.buildPhase;
            checkPhase = patchDuneCommand oldAttrs.checkPhase;
            # installPhase = patchDuneCommand oldAttrs.checkPhase;
          });

        formatting = treefmtEval.${pkgs.system}.config.build.check self;
      });

      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          packages =
            [
              treefmtEval.${pkgs.system}.config.build.wrapper
            ]
            ++ (with pkgs; [
              fswatch
              ocaml
              dune_3
              nasm
              clang
              lldb
              git # ari's computer is stupid
              zig
            ])
            ++ (with pkgs.ocamlPackages; [
              findlib
              utop
              odoc
              ocamlformat
              ocamlbuild
              ocaml-lsp
              merlin
            ])
            # todo: this should still work on x86_64-darwin
            ++ (lib.optionals pkgs.stdenv.isLinux [ pkgs.valgrind ]);

          inputsFrom = [
            self.packages.${pkgs.system}.default
          ];
        };
      });
    };
}
