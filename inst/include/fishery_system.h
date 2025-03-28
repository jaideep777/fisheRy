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

	void init(int n, double temp);
	void update();

	Population& get_pop();

};

#endif // FISHERY_SYSTEM_H
