# How to install, compile and run this occ prototype

## Installing, compiling and running the occ mock server

See the readme of the server.

## Install the tools neccessary for building, compiling and running the client

You need a Linux system to be able to compile and run this demo. NixOS is a fine choice.

First install Nix (if you have NixOS skip this step):

```shell
curl https://nixos.org/nix/install | sh
```

then run the following command. Depending on the state of the global nix caches this can take a very long time.

```shell
nix-build
```

When finished start the static web server by calling

```shell
./runServer.sh
```

Now start a haskell development environment in the client shell with

```shell
./developClient.sh
```

You should now be dropped into a new shell loaded with the haskell environment.
Now compile the client with

```shell
cabal new-build --ghcjs
```

The first time you also have to link the output of the build into the folder for the web server

```shell
./symlink.sh
```

Now open a browser at http://localhost:8080/index.html

Have fun!
