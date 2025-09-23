#ifndef FISHERY_FISHERY_SYSTEM_H
#define FISHERY_FISHERY_SYSTEM_H

#include <vector>
#include "stock.h"
#include "fleet.h"
#include "initializer_v2.h"

#ifndef NATIVE_CPP
#include <Rcpp.h>
#endif

class FisheryParams {
    public:
    double rho;   // ratio of spawning grounds F to total (control) F 
    double f_spf_before; // percent of spawning grounds fishing that happens before spawning

    void print() {
        std::cout << "FisheryParams:" << std::endl;
        std::cout << "  rho: " << rho << std::endl;
        std::cout << "  f_spf_before: " << f_spf_before << std::endl;
    }
};

class Fishery {
	private:
	std::string params_file;
	io::Initializer I;
	Stock no_fishing_pop;

	std::vector<std::string> colnames = {"ssb", "tsb", "maturity", "quota_fgf", "yield", "effort"};

	public:
	bool debug = true; // Should debugging calculations be done 
	bool update_env = false;
	bool simulate_bio_only = false;

	Stock pop;
	std::vector<Fleet> fleets;
	FisheryParams par;

	double harvest_prop;
	double min_size_limit;

	StockSummary stock_summary;	

	public:
	Fishery(std::string _params_file, const Fish& f);

	// Functions to specify fishery-level control parameters
	void set_harvestProp(double _h);
	void set_minSizeLimit(double _lf50);

	double calc_quota(double temp);

	// Wrapper functions for enabling R interface
	void set_superFishSize(double _n);
	void set_traitVariances(std::vector<double> var);

	int readParams(std::string filename, bool verbose = false);
	int readEnvironmentFile(std::string filename);
	void updateEnv(double t);
	void init(int n, double t_init, double temp);

	// Fishery functions
	std::vector<double> equilibriateNaturalPopulation(double temp, double _n);
	void addFleet(std::string params_file, bool verbose = false);

    void summarize_population_metrics();
    void summarize_catch_metrics();

    std::vector<double> update(double temp);
    
#ifndef NATIVE_CPP	
	Rcpp::DataFrame simulate_r(double lf, double h, int nyears, double tsb0, double temp, bool re_init, std::string output_file);
#endif

};

#endif // FISHERY_SYSTEM_H
