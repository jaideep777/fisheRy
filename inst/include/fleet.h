#ifndef FISHERY_FLEET_H
#define FISHERY_FLEET_H

#include "population.h"

class Fleet{
	private:
	std::random_device rd;
	std::mt19937 g;
	
	public:
	double chi = 1;
	std::string control_model = "exp";

	public:

	Fleet();

	void update_chi(const std::vector<double>& chi_in_windows, 
					const std::vector<double>& yield_in_windows, 
					const std::vector<double>& bs_in_windows,
					double yield_remainder, double bs_remainder);

	std::vector<double> harvest_dry_run(Population pop, double h, double temp);
	std::vector<double> harvest(Population& pop, double h, double temp);
};

#endif

