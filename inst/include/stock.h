#ifndef FISHERY_STOCK_H_
#define FISHERY_STOCK_H_

#include <vector>
#include <random>

#ifndef NATIVE_CPP
#include <Rcpp.h>
#endif

#include "fish.h"

class StockParams {	
	public:
	int recruitmentAge; ///< Age at recruitment
	double n = 5e6;	    ///< superfish size
};


// Struct to hold all summary statistics computed during population update
struct StockSummary {
	// Population metrics
	double nfish_start = 0;          ///< Total number of fish at the beginning of the season
	double survival_mean = 0;        ///< Mean survival probability

	// Maturation statistics
	double maturity = 0;             ///< Population maturity rate

	// Biomass metrics
	double ssb = 0;                  ///< Spawning stock biomass
	double tsb = 0;                  ///< Total stock biomass

	// Spawning and spf metrics
	double ssb0;                     ///< SSB just before spawning and SPF
	double ssb_spawning = 0;         ///< SSB at spawning time
	double ssb_spawning_ref = 0;     ///< Reference SSB at spawning 
	double ssb_after_spawning = 0;   ///< SSB after spawning
	double ssb_after_spawning_ref = 0; ///< Reference SSB after spawning
	double ssbn = 0;                 ///< Final SSB after all mortality
	double ssbn_ref = 0;             ///< Reference final SSB

	// Mortality metrics
	double tsb_before_mort = 0;      ///< Total stock biomass before mortality
	double tsb_after_mort = 0;       ///< Total stock biomass after mortality

	// Recruitment metrics
	double nrecruits_real = 0;       ///< Actual number of recruits
	double nrecruits_potential = 0;  ///< Potential number of recruits
	double nrecruits_per_fish = 0;   ///< Recruits per spawning fish
	double factor_dr = 0;            ///< Density-dependent recruitment factor
	double r0_avg = 0;               ///< Average recruitment rate
	double nspawners = 0;            ///< Number of spawning fish
	double nfish_ra = 0;             ///< Number of fish at recruitment age

	// Growth and density metrics
	double factor_dg = 0;            ///< Density-dependent growth factor
	double lmax = 0;                 ///< Maximum length
	double length90 = 0;             ///< 90th percentile length

	// Fishing reference metrics
	double Mort_fishable = 0;        ///< Natural mortality among fishable individuals
	double Mat_fishable = 0;         ///< Maturity among fishable individuals

	// At-age distributions
	std::vector<double> n_a = {};    ///< Number of fish at age
	std::vector<double> w_a = {};    ///< Weight of fish at age
	std::vector<double> mat_a = {};  ///< Maturity at age
	std::vector<double> nc_a = {};   ///< Catch at age
	std::vector<double> wc_a = {};   ///< Catch weight at age
};


class StockUtilities{
	// Mortality metrics
	double yield_spf = 0;            ///< Yield from spawning ground fishery
	double yield_spf_ref = 0;        ///< Reference yield from spawning ground fishery
};

class Stock{
	private:
	double std_missing_value = -1e20;

	std::default_random_engine generator;
	std::normal_distribution<double> normal_dist;
	std::vector<double> nrecruits_vec;
	
	template<class Func>
	std::vector<double> aggregateByAge(Func get_property){
		int amax = proto_fish.par.amax+2;
		std::vector<double> val(amax, 0);
		for (auto& f : fishes){
			val[f.age] += get_property(f);
		}
		return val;
	}

	template<class Func>
	double avgOverFishable(Func get_property){
		double mu = 0, n = 0;
		for (auto& f : fishes){
			if (f.isAlive && isFishable(f)){
				mu += get_property(f); // f.naturalMortalityRate(temp) + double(f.isMature)*f.par.Mspawning;
				n += 1;
			}
		} 
		if (n == 0) return 0;
		else return mu/n;
	}
	
	public:
	StockParams par;

	Fish proto_fish;	           ///< Prototype fish. A copy of this fish is always used to initialize new fish in population.
	std::vector<Fish> fishes;      ///< Vector of all fish in the population
	
	public:
	Stock(Fish f);

	int readParams(std::string filename, bool verbose=false);
	void init(int n, double temp);	// initialize stock with n individuals

	double calcSSB(double min_age = 0);
	double calcTSB(double min_age = 0);
	double calcMaturity(double min_age = 0);
    double calcAbundanceAtAge(int age);

    /// @brief  Is this fish fishable?
	/// @param f fish to test
	/// @return true if fishable, false otherwise
	bool isFishable(const Fish &f);

	/// @brief Fishable biomass in the population
	/// @return fishable biomass [kg]
	double fishableBiomass();

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

	std::vector<double> noFishingEquilibriate(double temp);	

	int nfish();
	void summarize();
	void print_summary();

#ifndef NATIVE_CPP
	Rcpp::DataFrame get_state();
	Rcpp::DataFrame get_traits();
#endif

};


#endif
