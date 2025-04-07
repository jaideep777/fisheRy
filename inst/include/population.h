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
	bool update_env = false;
	bool simulate_bio_only = false;
	
	// reproduction
	//double r0 = 21.77072;		// recruitment rate per kg SSB 
	double rmax = 1e12;
//	double Bhalf = 3.65e8*10; ///5000;	// Half saturation constant of recruitment

//	double s0 = 0.1126797; //0.11;          // Egg survival propbability
	int recruitmentAge; // = 3;

	// ---------------- DEPRECATED ------------------------------
	// management / fishing selectivity
	double lmin_sq;  // status quo minimum size limit, for which the selectivity curve is calibrated
	double F3_sq;    // F3 for status quo fishery
	double F5_sq;    // F5 for status quo fishery

	double lmin;
	double F1;
	double F2;
	double F3;
	double F4;
	double F5;
	double F6;

	// double sf; // = 0.1222;	// steepness of selectivity curve
	// double lf50; // = 45; //61.4806;  // threshold fish length

	// environmental stochasticity
	double sigmaf; // = 0.4858775;

	// effort dynamics and employment
	// Fleet specific
	double q; // = 2.83e-6;		// scaling parameter relating to catchability and density
	double dsea; // = 0.054;	// Required Person-years per vessel day
	double dmax; // = 30000e20;	// max available person-years // DEPREACATED, remove entirely
	double dshr; // = 0.000004;	// FTE/kg
	double b; // = 0.75;		// density dependence

	// revenue and profit 
	double price_sea; // = 13.13;		// landing price NOK/kg
	double price_shore; // = 17.0;		// selling price NOK/kg
	double fee_ratio;                            // fees as proportion of landed value

	double salary_sea; // = 1078000;			// employment cost sea NOK/FTE
	double salary_shore; // = 348000;			// employment cost shore NOK/FTE
	double fixed_costs_sea; // = 351123000;	// fixed costs sea NOK (= average per unit * #units)
	double fixed_costs_shore; // = 1032468000;	// fixed costs shore NOK
	double variable_costs_sea; // = 65000; 		// variable costs NOK/vessel day
	double scale_catch; // = 0.356; //0.53; 		// percentage of total codfish catch that is cod
	// ---------------------------------------------------
	
	double rho;   // ratio of spawning grounds F to total (control) F 
	double f_spf_before; // percent of spawning grounds fishing that happens before spawning

	double h = 0;
	double Fc = 0;

	double n = 5e6;	// superfish size


	// ***
	// calculated variables
	// double mort_fishing_mature = 0; 
	// double mort_fishing_immature = 0; 
	// double F_spf; // Fishing mortality rate in the spawning grounds
	// double F_fgf; // Fishing mortality rate in the feeding grounds

	// OLD EFFORT DYNAMICS
//	bool use_old_model_effort = false;
	int a_thresh;	// threshold age over which fishing selectivity is > 0.5

	//// function to init
	//PopulationParams(double _h){
	//    h = _h;
	//    mort_fishing_mature = -log(1-h);
	//    mort_fishing_immature = -log(1-h);
	//}

	void initFromFile(std::string params_file, bool verbose=false);
	void print();
};


// TODO: Enable systematic storing and operating population distributions
class PopulationSummary{
	public:
	// std::vector<double> vage, vfreq, vlen, vmat;
	std::vector<double> n_a, w_a, mat_a, nc_a, wc_a;
};


class SeaEnvironment{
	public:
	double year = 0;
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

	double std_missing_value = -1e20;

	template<class Func>
	std::vector<double> aggregateByAge(Func get_property){
		int amax = proto_fish.par.amax+2;
		std::vector<double> val(amax, 0);
		for (auto& f : fishes){
			val[f.age] += get_property(f);
		}
		return val;
	}

	public:
	PopulationSummary pop_summary;

	// names of variables returned by Population::upodate()
	std::vector<std::string> colnames = 
	    {"ssb", "yield", "employment", "profit",
	     "employment.sea", "employment.shore", 
	     "profit.sea", "profit.shore", "tsb", 
	     "r0", "nrecruits", "nfish_ra", "nsuperfish",
	     "factor_dg", "factor_dr", "max_length", "length90", 
	     "survival_mean", "maturity", "Nrel",
		 "ssb_spawning", "ssb_spawning_ref", "ssb_after_spawning", "ssb_after_spawning_ref", "ssbn", "ssbn_ref", "yield_spf", "yield_spf_ref",
		 "tsb_before_mort", "tsb_after_mort", "to_sea_bed",
		 "chi", "Fref_ref", "Mort_ref", "Mat_ref", "F_spf"
		 };

	public:
	// SeaEnvironment
	SeaEnvironment env;
	std::vector<int> t_env;
	std::vector<SeaEnvironment> v_env;

	bool verbose = false;          ///< Should population summary be printed at every update?
	std::string output_file = "";

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
	int readParams(std::string filename, bool verbose=false);
	
	void set_superFishSize(double _n);

	int readEnvironmentFile(std::string filename);
	void updateEnv(double t);

	// OLD MODEL EFFORT DYNAMICS
	// void calc_athresh(double tsb0, double temp);

	void set_harvestProp(double _h);
	// void set_fishingMortality(double _F_fgf);
	void set_minSizeLimit(double _lf50);
	void set_traitVariances(std::vector<double>var);
	void init(int n, double temp);	// initialize population with n individuals

	std::vector<double> noFishingEquilibriate(double temp);	

	double calcSSB(double min_age = 0);
	double calcTSB(double min_age = 0);
	std::vector<double> calcSB();

	/// @brief Fishing mortality rate as a function of length under status quo conditions, used as a selectivity function
	/// @param len 
	/// @return Reference fishing mortality
	double fishingMortalityRef(double len);

	/// @brief  Is this fish fishable?
	/// @param f fish to test
	/// @return true if fishable, false otherwise
	bool isFishable(const Fish &f);

	/// @brief Fishable biomass in the population
	/// @return fishable biomass [kg]
	double fishableBiomass();

	/// @brief Calculate average fishing mortality rate for each age class, including spawning grounds fishery
	/// @return A vector containing the average fishing mortality rate for each age group, indexed by age
	std::vector<double> fishingMortRefByAge();

	/// @brief Calculate average maturity for each age class
	/// @return A vector containing the average maturity rate for each age group, indexed by age
	std::vector<double> maturityByAge();

	/// @brief Calculate average natural mortality rate for each age class, including spawning mortality
	/// @param temp The current temperature affecting natural mortality rates.
	/// @return A vector containing the average natural mortality rate for each age group, indexed by age
	std::vector<double> naturalMortByAge(double temp);

	/// @brief Calculate average natural mortality rate over all fishable individuals
	/// @param temp The current temperature affecting natural mortality rates.
	/// @return average natural mortality rate
	double naturalMortFishable(double temp);

	/// @brief Calculate average reference fishing mortality rate over all fishable individuals
	/// @return Average reference fishing mortality rate
	double fishingMortRefFishable();

	/// @brief Calculate average maturity over fishable population
	/// @return A vector containing the average maturity rate for each age group, indexed by age
	double maturityFishable();

	/// @brief Average an age-dependent quantity Q over the given age range, excluding missing values
	/// @param Qa     vector containing the age-dependent quantity Q. The vector is indexed by age, so Q[0] will be a garbage value
	/// @param amin   minimum age (inclusive)
	/// @param amax   maximum age (inclusive)
	/// @param missing_value  value in Qa to ignore while averaging
	/// @return averaged quantity
	double avgOverAges(const std::vector<double>& Qa, int amin, int amax, double missing_value = -1e20);

	/// @brief Calculates the fishing effort
	/// @param F Fishing mortality.
	/// @param M Natural mortality.
	/// @param Nr Population size as a fraction of carrying capacity
	/// @return Fishing effort [vessel-days/year].
	double effort1(double Nr, double F, double M);

	/// @brief Updates the population dynamics for one time step.
	/// @param temp The current temperature affecting fish biology and dynamics.
	/// @return A vector containing various population metrics and dynamics for analysis.
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
