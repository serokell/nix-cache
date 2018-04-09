{ pkgs? import <nixpkgs> {} }:
let
  inherit (pkgs) stdenv;
nifpp = stdenv.mkDerivation {
  name = "nifpp";
  src = pkgs.fetchurl {
    url = https://raw.githubusercontent.com/goertzenator/nifpp/2da3733a15c5e2fbae55350a96c09825318c2d05/nifpp.h;
    sha256 = "0ap1fddw8jv8p8wq3l06mf3idljf8m6yxdn5a66i5ps4jl2afglp";
  };
  buildCommand = ''
    mkdir -p $out/include
    cp $src $_/nifpp.h
  '';
};
in
pkgs.stdenv.mkDerivation {
  name = "nix-cache";
  buildInputs = with pkgs; [
    erlangR20 (rebar3.override { erlang=erlangR20; }) nix.dev nix.out boost.dev nifpp
  ];
  shellHook = ''
    export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -DSYSTEM=\"${stdenv.system}\" -isystem ${pkgs.erlang}/lib/erlang/usr/include"
    export NIX_LDFLAGS="$NIX_LDFLAGS -L${pkgs.erlang}/lib/erlang/usr/lib"
  '';
}
