# FisheRy

FisheRy is an R package to assess the impact of management choices on the sustainability of fisheries. FisheRy runs an agent-based age-size-structured biological model of the concerned fish, coupled to a  socio-economic model of the fishery. It predicts the emergent properties of the fish population such as spawning stock biomass, as well as socio-economic outputs of the fishery, such as yield, employment, and net revenue. It can also compute satisfaction assessmnts of multiple stakeholders, such as convervationists, government, industry, and recreational users. It can thus be used to calculate the safe operating spaces, i.e., management regimes in which high joint stakeholder satisfaction is achieved.  

## Installation

### Prerequisites

- Install the latest version of R.
- Install a C++ compiler that supports C++11. This is already available on Linux. On Windows, you will have to install [Rtools](http://cran.r-project.org/bin/windows/Rtools/). 
- Install the devtools package 

### Installing fisheRy

You can install `fisheRy` directly from github. Currently, you should install from the `new_model_restart` branch, like so: 

```r  
devtools::install_github(repo = "jaideep777/rfish", ref = "new_model_restart")
```

**NOTE**: Currently, the package name is rfish, so load the packaged using 
```r
library(rfish)
```

### Stable release

Coming soon.

### Development version

Latest development version can be found here: https://github.com/jaideep777/rfish/tree/new_model_restart 

## Usage

To solve a fishery model, you need to create four objects:

1. A `fish`, which will be used as a prototype to construct all fish in the population. 
2. A reference `population`, which will be simulated to equilibrium with no fishing.
3. A population which will be used to perform simulaitons with fishing.
4. A `simulator`, which will simulate the population under different management settings (defined via two control parameters - minimum size limit and harvest proportion). 

The simulator allows for simultaneously simulating multiple populations with different control parameters, and also allows for calculation of utilties, stakeholder satisfaction, and joint stakeholder satisfaction. 

For details on the usage, please see the tutorials [here](https://jaideep777.github.io/rfish/index.html).

## Documentaion 

Detailed documentation and totorials are available [here](please see the tutorials [here](https://jaideep777.github.io/rfish/index.html).

## Author and contact

Jaideep Joshi
joshi@iiasa.ac.at


## Acknowledgement

This project was funded by XXX.



