## Get Erlang
#### MAC OSX in terminal
`ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

`brew install erlang`

## How to play?
Compile: `erl -make`

Run Erlang VM: `erl -pa ebin/`

Start game: `application:start(solar).`

## How can i play with others?

In the GUI:

1. click ```Communication -> Set name``` to set your nodes name, this also sets the cookie.
2. click ```Communication -> Connect``` and enter the full name of another node, this connects your node to all nodes he is connected to.

## About the game
You are in control of a solar system with limited resources. Every solar system contains harvestable metals and 2 out of 3 rare materials. In order to dominate the galaxy you need to strategically trade resources with other players in order to build a death ray that has the capability to wipe out all resources in the galaxy.

## Building cost for each ship
|    Item    | Metals | Water | Carbon | Crystals |
|:----------:|:------:|:-----:|:------:|:--------:|
| Harvester  |   25   |   1   |    1   |    2     |
| Cargo Ship |  200   |   2   |    3   |    1     |
|   Escort   |  500   |   9   |    6   |    7     |
| Spy Drone  |  800   |  21   |   24   |   23     |
| Death Ray  |  15000 | 1000  |  1000  | 1000     |
