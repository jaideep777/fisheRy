#ifndef FISHERY_FLEET_H
#define FISHERY_FLEET_H

#include "population.h"


class WindowProps{
	public:
	double B_start = 0;     ///< fishable biomass at the start of the window (for calculating Nrel)
	double Cbar = 0;        ///< Catch rate (annualized yield) during window

	double chi = 0;         ///< Fishing mortality scalar applied during window
	double yield = 0;       ///< yield acquired within window
	double B_sampled = 0;   ///< Fishable biomass sampled during this window
	double F_fishable = 0;  ///< Average fishing mortality rate among fishable individuals within window
	double M_fishable = 0;  ///< Average natural mortality rate among fishable individuals within window
	double n_fishable = 0;  ///< Number of fishable individuals sampled in window
};

class Fleet{
	private:
	std::random_device rd;
	std::mt19937 g;
	
	public:
	std::vector<WindowProps> window_props_vec;

	double chi = 1;
	std::string control_model = "exp";
	double window_dt = 0.1; // window length [years]

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

