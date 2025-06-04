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
#...then build.
RUN nix-shell --pure --run "bazel build //:bridget_backend"

# ok, this will be hacky. I will copy all of the .so files that are output by ldd into a single folder,
# then remove the libjvm.so because it needs further libs from the openjdk folder
# and then I'll just copy the openjdk folder.
# this will probably be at least 700MB (600MB jdk + 115MB .so files). Maybe later use jlink to get a
# slimmer jre
RUN mkdir -p copy_to_distroless/
                                                                                # | took a couple retries to get that escaping right
                                                                                # V 
RUN nix-shell --pure --run "ldd bazel-bin/bridget_backend | awk \"/=>/ {print \\\$3}\" | xargs -I {} cp -L {} copy_to_distroless/"
RUN rm copy_to_distroless/libjvm.so

# another gemini gem:
# gets the openjdk path and copies the entire folder (700MB).
RUN nix-shell --pure --run "ldd bazel-bin/bridget_backend | \
    awk \"/openjdk/ {print \\\$3}\" | \
    head -n 1 | \
    sed -E \"s|(.*openjdk-11[^/]*).*|\\\\1|\" | \
    xargs -I {} cp -rL {} /app/copy_to_distroless/openjdk"



#second stage
FROM gcr.io/distroless/static-debian12

WORKDIR /app

COPY --from=build /app/copy_to_distroless/. .
# get the bazel-built java code too
COPY --from=build /app/bazel-bin/bridget_backend.runfiles/bridget_ui_ws/jar_deploy.jar /app/bridget_backend.runfiles/bridget_ui_ws/jar_deploy.jar
#and copy the actual executable pls
COPY --from=build /app/bazel-bin/bridget_backend .

ENTRYPOINT ["/app/ld-linux-x86-64.so.2", "--library-path", "/app:/app/openjdk/lib/openjdk/lib/server", "/app/bridget_backend"]