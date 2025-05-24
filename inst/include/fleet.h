#ifndef FISHERY_FLEET_H
#define FISHERY_FLEET_H

#include "population.h"
#include "stock.h"

class WindowProps{
	public:
	double B_start = 0;     ///< fishable biomass at the start of the window (for calculating Nrel)
	double C_rate = 0;      ///< Catch rate (annualized yield) during window

	double chi = 0;         ///< Fishing mortality scalar applied during window
	double yield = 0;       ///< yield acquired within window
	double B_sampled = 0;   ///< Fishable biomass sampled during this window
	double F_fishable = 0;  ///< Average fishing mortality rate among fishable individuals within window
	double M_fishable = 0;  ///< Average natural mortality rate among fishable individuals within window
	double n_fishable = 0;  ///< Number of fishable individuals sampled in window
};

class FleetParams{
	public:

	// management / fishing selectivity
	double lmin_sq;  // status quo minimum size limit, for which the selectivity curve is calibrated
	double F3_sq;    // F3 for status quo fishery
	double F5_sq;    // F5 for status quo fishery

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

	// Share in a multi-fleet fishery
	double quota;      // Quota assigned to this fleet (fraction of total harvest among all fleets)

	public:
	void initFromFile(std::string params_file, bool verbose=false);
	void print();
};

class Fleet{
	private:
	std::random_device rd;
	std::mt19937 g;
	
	double h;
	double Fc;

	public:
	FleetParams par;

	std::vector<WindowProps> window_props_vec;

	double chi = 1;
	// FIXME: below can be moved to FleetParams
	double chi0_scalar_slope = 1.5;
	std::string control_model = "exp";
	double window_dt = 0.1; // window length [years]

	double Fref_fishable = 0; ///< Reference fishing mortality rate averaged over fishable individuals

	public:

	Fleet();

	void readParams(std::string params_file, bool verbose=false);

	void set_harvestProportion(double _h);
	void set_minSizeLimit(double _lf50);

	double fishingMortalityRef(double len);

	bool isFishable(const Fish &f);

	/// @group stuff calculated over fishable individuals
	/// @brief Calculate natural mortality, maturity, and fishing mortality rates averaged over fishable individuals of Stock stock	
	double naturalMortFishable(const Stock &stock, double temp);
	double maturityFishable(const Stock &stock);
	double fishingMortRefFishable(const Stock& stock);
	/// @}

	/// @brief         Initialize chi, the fishing mortality scalar
	/// @param pop     Stock to fish
	/// @param F       Fishing mortality rate realized in feeding grounds ( = f_fgf * Fc)
	/// @param temp    Temperature
	void init_chi(Stock &pop, double F_fgf, double temp);

	void update_chi(const std::vector<double>& chi_in_windows, 
					const std::vector<double>& yield_in_windows, 
					const std::vector<double>& bs_in_windows,
					double yield_remainder, double bs_remainder);

	std::vector<double> harvest_dry_run(Population pop, double quota, double temp);
	std::vector<double> harvest(Population& pop, double quota, double temp, bool return_progress = false);

	double effort_constantC(double q, double b, double K);
	double effort_constantF(double q, double b, double K);

	private:

	template<class Func>
	double avgOverFishable(Func get_property, const Stock &stock){
		double mu = 0, n = 0;
		for (auto& f : stock.fishes){
			if (f.isAlive && isFishable(f)){
				mu += get_property(f); 
				n += 1;
			}
		} 
		if (n == 0) return 0;
		else return mu/n;
	}

};

#endif

