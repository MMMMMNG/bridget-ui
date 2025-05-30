FROM tweag/inline-java

# so it ddoesn't error out with error: evaluation aborted with the following error message: '
# This version of Nixpkgs requires Nix >= 2.2, please upgrade
#RUN nix-shell --run upgrade-nix
# replacing old 'nix-2.1.3'
# installing 'nix-2.28.3'
RUN nix-env --install --file '<nixpkgs>' --attr nix cacert -I nixpkgs=channel:nixpkgs-unstable

WORKDIR /app
ENTRYPOINT tail -f /dev/null
#RUN cd /app/inline-java && nix-shell --pure --run "bazel build //..."