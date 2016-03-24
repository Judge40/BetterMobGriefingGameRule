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

| Entity     | GameRule              |
| ---------- | --------------------- |
| Creeper    | mobGriefingCreeper    |
| Enderman   | mobGriefingEnderman   |
| Ghast      | mobGriefingGhast      |
| Silverfish | mobGriefingSilverfish |
| Wither     | mobGriefingWither     |
| Zombie     | mobGriefingZombie     |

Full details of the effects of each game rule can be found in this
projects wiki.

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
