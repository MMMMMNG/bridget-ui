FROM nixos/nix:2.28.3

WORKDIR /app
#copy nix dependency declarations first, so they can be cached
COPY nixpkgs.nix .
COPY shell.nix .
#from dockerhub page
RUN nix-channel --update
#provision shell first (installs nix packages)...
RUN nix-shell

#also cache the bazel extraction (not sure this actually works)
RUN nix-shell --pure --run "bazel version"

#cache stackage because it takes for ever
COPY .bazelrc .
COPY WORKSPACE.bazel .
COPY BUILD.bazel .
COPY stackage_snapshot.json .
COPY snapshot-9.0.2.yaml .
RUN nix-shell --pure --run "bazel build @stackage//:scotty"
#also cache inline-java lib
RUN nix-shell --pure --run "bazel build @inline-java//:inline-java"
#...and some others
RUN nix-shell --pure --run "bazel build @stackage//:wai-cors"


# never stop container so dev can use it e.g. via exec tab in docker desktop
ENTRYPOINT tail -f /dev/null