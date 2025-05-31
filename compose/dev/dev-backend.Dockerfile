FROM nixos/nix:2.28.3

WORKDIR /app
#cache nix dependencies
COPY nixpkgs.nix .
COPY shell.nix .
#from dockerhub page
RUN nix-channel --update
#provision shell first (installs nix packages)...
RUN nix-shell


# never stop container so dev can use it e.g. via exec tab in docker desktop
ENTRYPOINT tail -f /dev/null