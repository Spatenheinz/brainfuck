{
  description = "OCaml development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ocamlPackages = pkgs.ocamlPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # OCaml compiler
            ocaml

            # Language server & editor tooling
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat

            # Commonly used libraries
            ocamlPackages.utop          # Interactive REPL
          ];

          shellHook = ''
            echo "🐫 OCaml development environment"
            echo "OCaml:   $(ocaml --version)"
            echo "opam:    $(opam --version)"
          '';
        };

        # Optional: define a package if you have a dune project
        # packages.default = ocamlPackages.buildDunitPackage {
        #   pname = "my-project";
        #   version = "0.1.0";
        #   src = ./.;
        #   buildInputs = with ocamlPackages; [ base stdio ];
        # };
      }
    );
}
