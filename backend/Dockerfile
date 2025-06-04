FROM nixos/nix:2.28.3 as build

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
#copy the contents of the backend folder
COPY . .
#...then build. And gather all the dependencies into a tar
# so we can easily copy it into the next stage which is much
# more lightweight (hopefully)
RUN nix-shell --pure --run "bazel build //:bridget_backend_bundle"

# extract the tarball again, because I couldn't find a way
# to just gather the deps without compressing 'em
RUN mkdir -p copy_to_distroless/ && tar -xf bazel-bin/bridget_backend_bundle.tar -C ./copy_to_distroless

#recreate expected directory structure...
RUN cd ./copy_to_distroless && \
    mkdir -p bridget_backend.runfiles/ && \
    find . -maxdepth 1 -mindepth 1 ! -name "bridget_backend.runfiles" ! -name "bridget_backend" ! -name "*.so*" -exec mv {} bridget_backend.runfiles/ \;

#second stage
FROM eclipse-temurin:11-jre-noble

WORKDIR /home

COPY --from=build /app/copy_to_distroless/. /home/

#ENTRYPOINT ["/bridget_backend"]
ENTRYPOINT ["tail", "-f", "/dev/null"]