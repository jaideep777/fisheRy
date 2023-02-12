# FisheRy

FisheRy is an R package to assess the impact of management choices on the sustainability of fisheries. FisheRy runs an agent-based age-size-structured biological model of the concerned fish, coupled to a socio-economic model of the fishery. It predicts the emergent properties of the fish population such as spawning stock biomass, as well as socio-economic outputs of the fishery, such as yield, employment, and net revenue. It can also compute satisfaction assessments of different stakeholders, such as convervationists, government, industry, and recreational users. It can thus be used to calculate safe operating spaces for fisheries, i.e., management regimes in which high joint stakeholder satisfaction is achieved.  

## Installation

### Prerequisites

- Install the latest version of R.
- Install a C++ compiler that supports C++11. This is already available on Linux. On Windows, you will have to install [Rtools](http://cran.r-project.org/bin/windows/Rtools/). 
- Install the devtools package 

### Installing fisheRy

You can install `fisheRy` directly from github. Currently, you should install from the `develop` branch, like so: 

```r  
devtools::install_github(repo = "jaideep777/fisheRy", ref = "develop")
```

The package can be loaded by  
```r
library(fisheRy)
```

### Stable release

Coming soon.

### Development version

Latest development version can be found here: https://github.com/jaideep777/fisheRy/tree/develop 

## Usage

To solve a fishery model, you need to create three objects:

1. A `Fish`, which will serve as a prototype to construct all fish in the population. 
2. A `Population` to simulate.
3. A `Simulator`, which will simulate the population under different temperature and management settings (defined via two control parameters - minimum size limit and harvest proportion). 

The simulator allows for simultaneously simulating multiple populations with different temperature and control parameters, and also allows for calculation of utilties, stakeholder satisfaction, and joint stakeholder satisfaction. 

For details on the usage, please see the tutorials [here](https://jaideep777.github.io/fisheRy/index.html).

## Documentaion 

Detailed documentation and totorials are available [here](please see the tutorials [here](https://jaideep777.github.io/fisheRy/index.html).

## Author and contact

Jaideep Joshi
joshi@iiasa.ac.at


## Acknowledgement

This project was funded by XXX.



