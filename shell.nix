{ pkgs ? import <nixpkgs> {}, compiler ? "default" }@args:

let
  drv = import ./. args;
in

if pkgs.lib.inNixShell then drv.env else drv
