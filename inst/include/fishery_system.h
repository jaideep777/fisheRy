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

	public:
	Population pop;
	std::vector<Fleet> fleets;

	public:
	Fishery(std::string _params_file, const Fish& f);
	vector<double> equilibriateNaturalPopulation(double temp, double _n);
	
	harvest();
	harvest_dry_run();

	void initialize();
	void update();
	void shutdown();


};

#endif // FISHERY_SYSTEM_H
