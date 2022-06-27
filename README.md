﻿# Overview

A simple betting game for two players (“Rock-Paper-Scissors”) played inside a Hydra head.

# Goals

- Explore the implementation possibilities of a simple but still non-trivial distributed application with Hydra Node WebSocket API
- Relate any feedback or pain points encoutered during development to the Hydra team
- Produce an experience report for the whole process that can be used as reference both for the Hydra team to understand the hurdles encountered and for other parties wanting to develop on Hydra.

## Advantages of Hydra in this use case

* Rapid games in close to realtime
* Zero fees once funds are in the hydra head

# The demonstration

## Overall architecture
We'll be running a local devnet consisting of the following components:

* a single block producing Cardano Node
* For each player
    * a Hydra Node communicating with the Cardano Node through its Unix socket
    * an application which communicates with this Hydra Node through WS API and also reads commands
from the user's terminal and produces some diagnostic output reflecting the Hydra Head state transitions

## Building and running
We'll need Docker, Docker Compose, and Nix with `nix-command` and `flakes` features enabled.

First, clone the git repository at `https://github.com/mlabs-haskell/hydra-demo.git` and build the application.
Building may take a while the first time.

```
$ git clone https://github.com/mlabs-haskell/hydra-demo.git
$ cd hydra-demo
$ nix build .
```

Then pull all the necessary Docker images, and spin-up the devnet. From the repository root:

```
$ docker-compose pull
Pulling cardano-node     ... done
Pulling hydra-node-alice ... done
Pulling hydra-node-bob   ... done

$ ./spin-up-devnet-from-scratch
Removing network hydra-demo_default
WARNING: Network hydra-demo_default not found.
Removing network hydra-demo_hydra_net
WARNING: Network hydra-demo_hydra_net not found.
Cleaning up directory devnet
[sudo] password for <user>: 
Prepared devnet, you can start the cluster now
Creating network "hydra-demo_default" with the default driver
Creating network "hydra-demo_hydra_net" with driver "bridge"
Creating hydra-demo_hydra-node-alice_1 ... done
Creating hydra-demo_cardano-node_1     ... done
Creating hydra-demo_hydra-node-bob_1   ... done
Waiting for the node socket ..................................................................................... done
./seed-alice.tx
Transaction successfully submitted.
./seed-alice-fuel.tx
Transaction successfully submitted.
./seed-bob.tx
Transaction successfully submitted.
./seed-bob-fuel.tx
Transaction successfully submitted.
```

Yes, `spin-up-devnet-from-scratch` invokes `sudo` and may prompt for your password, but fear not! It is there just to remove any stale `./devnet` data directory from previous runs.

Now that the Hydra Head is operational, we can finally start the application instances.

From the repository root in two separate terminals execute:
```
nix run . 127.0.0.1 4001 devnet/credentials/alice.sk devnet/protocol-parameters.json
```
and
```
nix run . 127.0.0.1 4002 devnet/credentials/bob.sk devnet/protocol-parameters.json
```
