#ifndef FISHERY_FLEET_H
#define FISHERY_FLEET_H

#include "population.h"

class Fleet{
	private:
	std::random_device rd;
	std::mt19937 g;
	
	public:
	double chi = 1;
	double k = 0.05;
	std::string control_model = "exp";

	public:

	Fleet();
	std::vector<double> harvest_dry_run(Population pop, double h, double temp);
	std::vector<double> harvest(Population& pop, double h, double temp);
};

#endif

