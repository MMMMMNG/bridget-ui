# bridget-ui
A  Web UI for the game [Bridget](https://boardgamegeek.com/boardgame/286904/bridget) written in elm.  
Using benajakuhn's [bridget implementation](https://github.com/benajakuhn/wodsBridget) (written in java) to handle game logic.

The bridge between elm and java is a haskell webserver (scotty) using [inline-java](https://github.com/tweag/inline-java)

## Download Source
...including submodule:
```shell
git clone --recurse-submodules https://github.com/MMMMMNG/bridget-ui.git
cd bridget-ui/
```

## Running the full app:
```shell
cd compose/prod
docker compose up
```
Note: The backend image is 10GB+  
In my opinion that's better than installing 10GB+ worth of nix and bazel and haskell dependencies on my PC, though.

## Development
frontend: `elm reactor`


backend: 
```shell
cd compose/dev
docker compose up -d
```
then get an interactive shell in the container (e.g. the exec tab of the container in docker desktop), so you can build the app:
```shell
nix-shell
bazel build //:bridget_backend
```
and if there are no compiler errors, you can run the compiled polyglot app:
```shell
bazel-bin/bridget_backend
```
### Update stackage_snapshot.json
This is probably not necessary but it seems like it version-locks transitive deps, which is very good for reproducibility.
```shell
bazel run @stackage-unpinned//:pin
```
see also: [haskell.build](https://release.api.haskell.build/haskell/cabal#stack_snapshot)