#ifndef FISHERY_POPULATION_H_
#define FISHERY_POPULATION_H_

#include <vector>
#include <random>

#ifndef NATIVE_CPP
#include <Rcpp.h>
#endif

#include "fish.h"

class PopulationParams {	
	public:
	bool update_env = true;
	bool simulate_bio_only = false;
	
	// reproduction
	//double r0 = 21.77072;		// recruitment rate per kg SSB 
	double rmax = 1e20;
//	double Bhalf = 3.65e8*10; ///5000;	// Half saturation constant of recruitment

//	double s0 = 0.1126797; //0.11;          // Egg survival propbability
	int recruitmentAge = 3;

	// management / fishing selectivity
	double sf = 0.1222;	// steepness of selectivity curve
	double lf50 = 45; //61.4806;  // threshold fish length

	// environmental stochasticity
	double sigmaf = 0.4858775;

	// effort dynamics and employment
	double q = 2.83e-6;		// scaling parameter relating to catchability and density
	double dsea = 0.054;	// Required Person-years per vessel day
	double dmax = 30000e20;	// max available person-years
	double dshr = 0.000004;	// FTE/kg
	double b = 0.75;		// density dependence

	// revenue and profit 
	double price_sea = 13.13;		// landing price NOK/kg
	double price_shore = 17.0;		// selling price NOK/kg

	double salary_sea = 1078000;			// employment cost sea NOK/FTE
	double salary_shore = 348000;			// employment cost shore NOK/FTE
	double fixed_costs_sea = 351123000;	// fixed costs sea NOK (= average per unit * #units)
	double fixed_costs_shore = 1032468000;	// fixed costs shore NOK
	double variable_costs_sea = 65000; 		// variable costs NOK/vessel day
	double scale_catch = 0.356; //0.53; 		// percentage of total codfish catch that is cod
	
	double h = 0;

	double n = 5e6;	// superfish size

	double f_harvest_spg = 0.2;

	// ***
	// calculated variables
	double mort_fishing_mature = 0; 
	double mort_fishing_immature = 0; 

	// OLD EFFORT DYNAMICS
//	bool use_old_model_effort = false;
	int a_thresh;	// threshold age over which fishing selectivity is > 0.5

	//// function to init
	//PopulationParams(double _h){
	//    h = _h;
	//    mort_fishing_mature = -log(1-h);
	//    mort_fishing_immature = -log(1-h);
	//}
};


// TODO: Enable systematic storing and operating population distributions
class PopulationSummary{
	std::vector<double> vage, vfreq, vlen, vmat;
};


class SeaEnvironment{
	public:
	double temperature = 5.61;
	double recruitment_noise_multiplier = 1;
};


class Population{
	private:
	std::vector<double> vage, vfreq, vlen, vmat;
	std::vector<double> carrying_capacity;

	std::default_random_engine generator;
	std::normal_distribution<double> normal_dist;
	std::vector<double> nrecruits_vec;

	public:
	// names of variables returned by Population::upodate()
	std::vector<std::string> colnames = 
	    {"ssb", "yield", "employment", "profit",
	     "employment.sea", "employment.shore", 
	     "profit.sea", "profit.shore", "tsb", 
	     "r0", "nrecruits", "nfish_ra", "nsuperfish",
	     "factor_dg", "factor_dr", "max_length", "length90", 
	     "survival_mean", "maturity", "Nrel_sea", "Nrel_spg"};

	public:
	// SeaEnvironment
	SeaEnvironment env;
	std::vector<int> t_env;
	std::vector<SeaEnvironment> v_env;

	bool verbose = false;          ///< Should population summary be printed at every update?
	
	public:
	double K_fishableBiomass = 0;  ///< Fishable biomass under zero fishing pressure. This is set by the simulator
	double K_ssb = 0;              ///< Spawning stock biomass under zero fishing pressure. This is set by the simulator

	public:
	double current_year = 1;       ///< Current simulation year

	Fish proto_fish;	           ///< Prototype fish. A copy of this fish is always used to initialize new fish in population.
	std::vector<Fish> fishes;      ///< Vector of all fish in the population
	
	PopulationParams par;	       ///< Socioeconomic parameters
	
	public:
	Population(Fish f);
	
	void set_superFishSize(double _n);

	int readEnvironmentFile(std::string filename);
	void updateEnv(double t);

	// OLD MODEL EFFORT DYNAMICS
	void calc_athresh(double tsb0, double temp);

	void set_harvestProp(double _h);
	void set_minSizeLimit(double _lf50);
	void set_traitVariances(std::vector<double>var);
	void init(int n, double tsb, double temp);	// initialize population with n individuals

	std::vector<double> noFishingEquilibriate(double tsb0, double temp);	

	double calcSSB();
	double calcTSB();
	std::vector<double> calcSB();

	double selectivity(double len);
	double fishableBiomass();

	//double calcRealizedFishingMortality();
	double effort(double Nr, double F, double temp);

	std::vector<double> update(double temp = 5.6);

	int nfish();
	void summarize();
	void print_summary();

#ifndef NATIVE_CPP
	Rcpp::DataFrame get_state();
	Rcpp::DataFrame get_traits();
#endif

};


#endif
