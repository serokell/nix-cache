let
  overlay = final: previous: with final; {
    erlang = erlangR20;

    nifpp = stdenv.mkDerivation rec {
      name = "nifpp-${rev}";
      rev = "2da3733a15c5e2fbae55350a96c09825318c2d05";

      src = fetchzip {
        url = "https://github.com/goertzenator/nifpp/archive/${rev}.tar.gz";
        sha256 = "1v47k44f952vlxnz6a28sllsqgd4zw9cx76k9dy7chcy0zf0afh6";
      };

      buildCommand = "mkdir -p $out/include && cp $src/nifpp.h $_";
    };

    rebar3 = previous.rebar3.override { erlang = erlangR20; };
  };
in

with import <nixpkgs> { overlays = [ overlay ]; };

stdenv.mkDerivation {
  name = "nix-cache";

  buildInputs = [
    boost
    erlang
    nifpp
    nix
    rebar3
  ];

  shellHook = ''
    export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -DSYSTEM=\"${stdenv.system}\" -isystem ${pkgs.erlang}/lib/erlang/usr/include"
    export NIX_LDFLAGS="$NIX_LDFLAGS -L${pkgs.erlang}/lib/erlang/usr/lib"
  '';
}
