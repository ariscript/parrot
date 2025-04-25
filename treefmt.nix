{ pkgs, ... }:
{
  projectRootFile = "flake.nix";
  settings.global.excludes = [
    ".envrc"
    ".ocamlformat"
    "*.opam"
    "Makefile"
    "*.zip"
    "_tags"
    ".clang-format"
    "*.mll"
    "*.mly"
    "test/input/**/*"
    "test/input/*"
    "test/output/**/*"
    "test/output/*"
    "**/.gitkeep"
    "run"
    "submit"
    "debug"
    "runsmall"
  ];
  programs = {
    deadnix.enable = true;
    nixfmt.enable = true;
    ocamlformat.enable = true;
    jsonfmt.enable = true;
    mdformat.enable = true;
    clang-format.enable = true;
    asmfmt.enable = true;
    shfmt.enable = true;
  };
  settings.formatter = {
    "dune-file" = {
      command = "${pkgs.bash}/bin/bash";
      options = [
        "-euc"
        ''
          for file in "$@"; do
            tmp_file=$(mktemp)
            ${pkgs.dune_3}/bin/dune format-dune-file $file > $tmp_file
            mv $tmp_file $file
          done
        ''
        "--"
      ];
      includes = [
        "dune-project"
        "**/dune"
        "dune"
      ];
    };
  };
}
