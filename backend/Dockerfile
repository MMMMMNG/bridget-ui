FROM nixos/nix:2.28.3

WORKDIR /app
#copy nix dependency declarations first, so they can be cached
COPY nixpkgs.nix .
COPY shell.nix .
#from dockerhub page
RUN nix-channel --update
#provision shell first (installs nix packages)...
RUN nix-shell


#copy the contents of the backend folder
COPY . .
#...then build
RUN nix-shell --pure --run "bazel build //:bridget_backend"

ENTRYPOINT ["./bazel-bin/bridget_backend"]