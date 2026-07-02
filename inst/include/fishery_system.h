#ifndef FISHERY_FISHERY_SYSTEM_H
#define FISHERY_FISHERY_SYSTEM_H

#include <vector>
#include <string>
#include "stock.h"
#include "fleet.h"
#include "initializer_v2.h"
#include "tensor.h"

#ifndef NATIVE_CPP
#include <Rcpp.h>
#endif

class FisheryParams {
    public:
	// Spawning grounds fishery
	double rho;   // ratio of spawning grounds F to total (control) F 
    double f_spf_before; // percent of spawning grounds fishing that happens before spawning

	// fishing selectivity
	double lmin_sq;  // status quo minimum size limit, for which the selectivity curve is calibrated
	double F3_sq;    // F3 for status quo fishery
	double F5_sq;    // F5 for status quo fishery

	double F1;
	double F2;
	double F3;
	double F4;
	double F5;
	double F6;
	double lmin;

	bool using_empirical_fref;
	std::string Fref_empirical_file;
	
	int initFromFile(std::string filename, bool verbose=false);
    void print();
};

class Fishery {
	private:
	std::string params_file;
	io::Initializer I;
	Stock no_fishing_pop;

	public:
	std::vector<std::string> colnames = {
		"ssb", "yield", "employment", "profit", "effort",
		"tsb", "maturity", "quota", "recruits",
		"quota_fgf", "yield_fgf", "effort_fgf", "employment_sea_fgf", "employment_shore_fgf", "profit_sea_fgf", "profit_shore_fgf", 
		"quota_spf", "yield_spf", "effort_spf", "employment_sea_spf", "employment_shore_spf", "profit_sea_spf", "profit_shore_spf", 
		"ssb0", "ssb_spawning", "ssb_spawning_ref", "ssb_after_spawning", "ssb_after_spawning_ref", "ssbn", "ssbn_ref"
	};

	bool debug = false; // Should debugging calculations be done?
	bool update_env = false;
	bool simulate_bio_only = false;

	Stock pop;

	Fleet fleet_effective;      ///< Hypothetical fleet representing the total effective fishing mortality from all fleets, with fishing parameters ususally set from data. Used for calculating total quota, which is then divided into actual fleets
	std::vector<Fleet> fleets;  ///< Actual fleets which may have different fishing mortality parameters. Users need to ensure that selectivity of individual fleets are equal and equal to fleet_effective.
	std::vector<Fleet> spawner_fleets; ///< Actual fleets which operate in the spawning grounds

	FisheryParams par;

	double harvest_prop;

	StockSummary stock_summary;	
	// Tensor<double> profit_mask;

	public:
	Fishery(std::string _params_file, const Fish& f);

	void set_debug(bool b);

	// Functions to specify fishery-level control parameters
	void set_harvestProp(double _h);
	void set_minSizeLimit(double _lf50);

	void set_referenceFishingMortalityCurve(Fleet& fleet);
    void update_referenceFishingMortalityCurve_AllFleets();

    double calc_quota(double temp);
	std::vector<double> harvest(double quota, double temp, bool return_progress);

	// Wrapper functions for enabling R interface
	void set_superFishSize(double _n);
	void set_traitVariances(std::vector<double> var);

	int readParams(std::string filename, bool verbose = false);
	int readEnvironmentFile(std::string filename);
	void updateEnv(double t);
	void init(int n, double t_init, double temp);

	// Fishery functions
	std::vector<double> equilibriateNaturalPopulation(double temp, double _n, int nsteps);
	std::vector<double> equilibriateWithoutFishing(double temp, int nsteps);
	void addFleet(std::string params_file, bool verbose = false);
	void addSpawnerFleet(std::string params_file, bool verbose = false);

    void summarize_population_metrics();
    void summarize_catch_metrics(bool use_average_weight);
	void summarize_spawner_fishery_metrics(const std::vector<double>& spf_summary_before, const std::vector<double>& spf_summary_after, double quota_spf);
	
	std::vector<double> spawner_fishery(double quota);

    std::vector<double> update(double temp, double rec_noise_multiplier, double K);
    // std::vector<double> update(double temp, double K);

    double get_fref(int fleet_id, double len); // debug function to verify reference fishing mortality rate in R

	Tensor<double> scan(std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, std::vector<double> rec_noise_t, double tsb0, int niters, bool re_init);

    // std::vector<double> max_avg_utils(std::vector<int> dims, std::vector<double> data);
    // std::vector<double> stakeholder_satisfaction(std::vector<int> dims, std::vector<double> data);
    // std::vector<double> stakeholder_satisfaction_t(std::vector<int> dims, std::vector<double> data);

#ifndef NATIVE_CPP
	// Rcpp::NumericVector get_profit_mask();
    // Rcpp::DataFrame simulate_r(double lf, double h, int nyears, double tsb0, double temp, bool re_init, std::string output_file);
    Rcpp::DataFrame simulate_r(double lf, double h, int nyears, double tsb0, std::vector<double> temp_t, std::vector<double> rec_noise_t, bool re_init, std::string output_file);
    Rcpp::NumericVector simulate_multi_r(std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, std::vector<double> rec_noise_t, double tsb0, int niters, bool re_init);
#endif

};

#endif // FISHERY_SYSTEM_H
