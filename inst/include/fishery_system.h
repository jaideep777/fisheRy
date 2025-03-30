#ifndef FISHERY_FISHERY_SYSTEM_H
#define FISHERY_FISHERY_SYSTEM_H

#include <vector>
#include "population.h"
#include "fleet.h"
#include "initializer_v2.h"

class Fishery {
	private:
	std::string params_file;
	io::Initializer I;
	Population no_fishing_pop;

	private:
	Population pop;
	std::vector<Fleet> fleets;

	public:
	Fishery(std::string _params_file, const Fish& f);
	
	std::vector<double> equilibriateNaturalPopulation(double temp, double _n);
	
	std::vector<double> harvest();
	std::vector<double> harvest_dry_run();

	void update();

	Population& get_pop();

	// Wrapper functions for enabling R interface
	int readParams(std::string filename, bool verbose = false);
	void set_superFishSize(double _n);
	int readEnvironmentFile(std::string filename);
	void updateEnv(double t);
	void set_harvestProp(double _h);
	void set_minSizeLimit(double _lf50);
	void set_traitVariances(std::vector<double> var);
	void init(int n, double temp);
	void noFishingEquilibriate(double temp);
};

#endif // FISHERY_SYSTEM_H
