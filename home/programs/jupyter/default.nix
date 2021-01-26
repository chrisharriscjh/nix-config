{ config, pkgs, ... }:

{
  home.file."/lab/user-settings/@jupyterlab/shortcuts-extension".source = /cfg/home/programs/jupyter/shortcuts.jupyterlab-settings;
}
