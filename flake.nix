{
  description = "flywire development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    propcheck = {
      url = "github:Wilfred/propcheck";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, propcheck }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        propcheckPkg = pkgs.runCommand "propcheck-0.1" {
          src = propcheck;
        } ''
          mkdir -p $out/share/emacs/site-lisp/elpa/propcheck-0.1
          cp $src/propcheck.el $out/share/emacs/site-lisp/elpa/propcheck-0.1/
          echo "(define-package \"propcheck\" \"0.1\" \"Property based testing\" '((dash \"2.12\")))" > $out/share/emacs/site-lisp/elpa/propcheck-0.1/propcheck-pkg.el
        '';
        
        myEmacs = pkgs.emacsPackages.emacsWithPackages (epkgs: [
          epkgs.ert-async
          epkgs.dash
          epkgs.s
          epkgs.package-lint
          propcheckPkg
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [ myEmacs ];
        };

        packages.propcheck = propcheckPkg;

        apps.test = flake-utils.lib.mkApp {
          drv = pkgs.writeShellScriptBin "run-tests" ''
            ${myEmacs}/bin/emacs -q --batch -l test/run-tests.el
          '';
        };

        apps.lint = flake-utils.lib.mkApp {
          drv = pkgs.writeShellScriptBin "run-lint" ''
            ${myEmacs}/bin/emacs -q --batch -l test/run-lint.el
          '';
        };
      }
    );
}
