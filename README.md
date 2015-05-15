## Get Erlang
#### Linux in terminal
`sudo apt-get install erlang`
#### MAC OSX in terminal
`ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

`brew install erlang`
#### Windows
[Download](http://www.erlang.org/download.html)
## How to play?
Compile: `erl -make`

Run Erlang VM: `erl -pa ebin/`

Start game: `application:start(solar).`

## How can I play with others?

In the GUI:

1. click ```Communication -> Set name``` to set your nodes name, this also sets the cookie.
2. click ```Communication -> Connect``` and enter the full name of another node, this connects your node to all nodes he is connected to.

## About the game
You are in control of a solar system with limited resources. Every solar system contains harvestable metals and 2 out of 3 rare materials. In order to dominate the galaxy you need to strategically trade resources with other players in order to build a death ray that has the capability to wipe out all resources in the galaxy.

## About the ships
*Harvesters*: can be sent to harvest any resource available in the solar system. 

*Cargo ships*: are used for trading with other players. 

*Escorts*: can be sent with cargo ships to protect them from pirates. The higher the number of escorts, the lower the chance of being attacked. 

*Spy drones*: can be sent to spy on other players. If the spy mission is succesful it reveals the resources and ships currently owned by the player. 

*Death ray*: Activating the death ray destroyes the resources and ships belonging to each other player. However ships currently on mission are not destroyed. A death ray can only be used once.

Pirates can potentially attack any of your ships. Escorts are the only type that reduces pirate attacks, and can only be sent with cargo ships. The more cargo the carg ship is carrying, the higher the chance is of it being attacked. 

When a trade offer is sent to another player, the resources and ships included are removed from the pool of currently available resources and ships. They appear again if the offer is cancelled, otherwise the ships return with new resources after the trade has been accepted and performed. 

## Building cost for each ship
|    Item    | Metals | Water | Carbon | Crystals |
|:----------:|:------:|:-----:|:------:|:--------:|
| Harvester  |   25   |   1   |    1   |    2     |
| Cargo Ship |  200   |   2   |    3   |    1     |
|   Escort   |  500   |   9   |    6   |    7     |
| Spy Drone  |  800   |  21   |   24   |   23     |
| Death Ray  |  15000 | 1000  |  1000  | 1000     |
