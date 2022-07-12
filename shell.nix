let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in

let
  # Wrap Stack to configure Nix integration and target the correct Stack-Nix file
  #
  # - nix: Enable Nix support
  # - no-nix-pure: Pass environment variables, like `NIX_PATH`
  # - nix-shell-file: Specify the Nix file to use (otherwise it uses `shell.nix` by default)
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --no-nix-pure \
          --nix-shell-file=nix/stack-integration.nix \
        "
    '';
  };

in
pkgs.mkShell {
  buildInputs = [
    stack-wrapped
    pkgs.haskell-language-server
    # pkgs.haskell-language-server.override { supportedGhcVersions = ["884", "901"] }
    pkgs.postgresql
  ];

  shellHook = ''
    export PGDATA=$PWD/.nix-shell/db
    export PGHOST=$PWD/.nix-shell/db
    export LOG_PATH=$PGDATA/log
    export PGDATABASE=uaa
    export DATABASE_URL="postgresql:///uaa?host=$PGDATA"
    if [ ! -d $PGDATA ]; then
      mkdir -p $PGDATA
      pg_ctl initdb -D $PGDATA
    fi
    if ! pg_ctl status
    then
      pg_ctl                                                  \
        -D $PGDATA                                            \
        -l $PGDATA/postgres.log                               \
        -o "-c unix_socket_directories='$PGDATA'"             \
        -o "-c listen_addresses='*'"                          \
        -o "-c log_destination='stderr'"                      \
        -o "-c logging_collector=on"                          \
        -o "-c log_directory='log'"                           \
        -o "-c log_filename='postgresql-%Y-%m-%d_%H%M%S.log'" \
        -o "-c log_min_messages=info"                         \
        -o "-c log_min_error_statement=info"                  \
        -o "-c log_connections=on"                            \
        start
    fi
  '';

  # Configure the Nix path to our own `pkgs`, to ensure Stack-with-Nix uses the correct one rather than the global <nixpkgs> when looking for the right `ghc` argument to pass in `nix/stack-integration.nix`
  # See https://nixos.org/nixos/nix-pills/nix-search-paths.html for more information
  NIX_PATH = "nixpkgs=" + pkgs.path;
}