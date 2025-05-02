# COPIED from https://github.com/tweag/inline-java/blob/81768558746dede0850bb6e839d8efd5cae66bbb/Dockerfile
FROM tweag/stack-docker-nix
LABEL maintainer="Mathieu Boespflug <m@tweag.io>"

ADD shell.nix /
ADD nixpkgs.nix /
# Clean up non-essential downloaded archives after provisioning a shell.
RUN nix-shell /shell.nix --indirect --add-root /nix-shell-gc-root \
    && nix-collect-garbage