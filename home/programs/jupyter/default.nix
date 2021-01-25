{ config, pkgs, ... }:

#let setJupyterConfig = pkgs.writeTextFile {
  #name = "shortcuts.jupyterlab-settings";
  #destination = /home/chris;
  ##destination = "/home/chris/.jupyter/lab/user-settings/@jupyterlab/shortcuts-extension";
  #text = ''
    #"shortcuts": [
        #{
            #"command": "runmenu:restart-and-run-all",
            #"keys": [
                #"0",
                #"0"
            #],
            #"selector": "[data-jp-code-runner]",
            #"title": "Restart Kernel and Run All",
            #"category": "Run Menu"
        #},
        #{
            #"command": "notebook:move-cell-down",
            #"keys": [
                #"Shift J"
            #],
            #"selector": ".jp-Notebook:focus"
        #},
        #{
            #"command": "notebook:move-cell-up",
            #"keys": [
                #"Shift K"
            #],
            #"selector": ".jp-Notebook:focus"
        #},
        #{
            #"command": "notebook:extend-marked-cells-above",
            #"keys": [
                #"Shift K"
            #],
            #"selector": ".jp-Notebook:focus",
            #"disabled": true
        #}
    #]
  #''
#};
{
  home.file."/lab/user-settings/@jupyterlab/shortcuts-extension".source = /cfg/home/programs/jupyter/shortcuts.jupyterlab-settings;
}
