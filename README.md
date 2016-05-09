# Better mobGriefing GameRule

![Minecraft 1.7.10](https://img.shields.io/badge/Minecraft-1.7.10-lightgrey.svg)
![Forge 1.7.10-10.13.4.1558](https://img.shields.io/badge/Forge-1.7.10--10.13.4.1558-lightgrey.svg)  
[![License MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

## About
"Better mobGriefing GameRule" is a Minecraft mod which improves the
"mobGriefing" game rule, allowing separate values to be specified for
individual types of entity.

## Features

The below game rules can be used to override the mobGriefing behaviour
for the associated entity type, the value of these rules will be used
instead of the original "mobGriefing" game rule.

| Entity      | GameRule                |
| ----------- | ----------------------- |
| Creeper     | mobGriefing Creeper     |
| EnderDragon | mobGriefing EnderDragon |
| Enderman    | mobGriefing Enderman    |
| Ghast       | mobGriefing Ghast       |
| Sheep       | mobGriefing Sheep       |
| Silverfish  | mobGriefing Silverfish  |
| Wither      | mobGriefing WitherBoss  |
| Zombie      | mobGriefing Zombie      |

Each rule can be set to "true", "false" or "inherit", when the value is
"inherit" the entity's mobGriefing behaviour will be inherited from the
original "mobGriefing" game rule.

Full details of the effects of each game rule can be found in this
projects wiki.

### Configuration
The default values of the mobGriefing game rules can be set using the
in-game configuration menu. Defaults can be changed for both then global
mobGriefing game rule and each individual entity.

The specified default values will be used to set the initial mobGriefing
values during world creation as well as for any existing world in which
a given rule does not already exist.

All entities supported out of the box will be added to the configuration
file automatically, additional entities can be added manually to the
configuration file. Once added the entity can then be managed using the
in-game configuration menu.
The entity name specified must match the name as it appears in the
EntityList for that entity, for example "S:PigZombie=true".

The structure of the configuration file follows the Forge structure:
```
# Configuration file
defaultmobgriefingvalues {

    globalrules {
        S:mobGriefing=true|false
    }

    entityrules {
        S:<entity name 1>=true|false|inherit
        ...
        S:<entity name n>=true|false|inherit
    }
}
```

## Versioning
The versioning used for this project uses a combination of the target
Minecraft version and [SemVer](http://semver.org) in the format
"MCVERSION-MAJOR.MINOR.PATCH".

Each target Minecraft version is contained within it's own separate
branches using the
[GitFlow](https://github.com/petervanderdoes/gitflow-avh) branching
model. Ensure that the correct branch is selected before using any code
from this repository.

## License
This project is licensed under the [MIT License (MIT)](LICENSE).
