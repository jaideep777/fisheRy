#ifndef FISHERY_FLEET_H
#define FISHERY_FLEET_H

#include "stock.h"
#include "cubic_spline.h"

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
	// double lmin_sq;  // status quo minimum size limit, for which the selectivity curve is calibrated
	// double F3_sq;    // F3 for status quo fishery
	// double F5_sq;    // F5 for status quo fishery

	double F1 = 0;
	double F2 = 0;
	double F3 = 0;
	double F4 = 0;
	double F5 = 0;
	double F6 = 0;
	double lmin = 0;

	// *********** Empirical Selectivity *****************
	bool using_empirical_fref = false;
	Spline Fref_fn_spline; // Spline to store empirical selectivity, if using it
	// ***************************************************

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

	// control model
	std::string control_model;

	// technical parameters
	double max_chi; ///< Maximum value of chi, the fishing mortality scalar
	double chi0_scalar_slope;
	double window_dt; // window length [years]

	public:
	void initFromFile(std::string params_file, bool verbose=false);
	void print();
};

extern std::random_device rd;

class Fleet{
	private:
	std::mt19937 g;
	
	// double h;
	// double Fc;

	public:
	FleetParams par;

	std::vector<WindowProps> window_props_vec;

	double chi = 1;
	// FIXME: below can be moved to FleetParams

	bool debug = false; // debug mode, prints additional info

	public:

	Fleet();

	void readParams(std::string params_file, bool verbose=false);

	// void set_harvestProportion(double _h);
	// void set_minSizeLimit(double _lf50);
	void set_referenceFishingMortalityCurveLogistic(double F1, double F2, double F3, double F4, double F5, double F6, double lmin);
    void set_referenceFishingMortalityCurveEmpirical(std::string filename, double lmin);

    double fishingMortalityRef(double len);
	double fishingMortality(double len);

    double FishingMortalityRef_avgl(double lmax, int n);

    // bool isFishable(const Fish &f);

    double fishability(double length);

	/// @brief 
	/// @param stock 
	/// @param min_age 
	/// @return sum(w * Fref) over fish below and above lmin
	std::vector<double> cummulativeFishingMortalityRef(const Stock &stock, double min_age = 0);

    /// @brief Calculate natural mortality, maturity, and fishing mortality rates averaged over fishable individuals of Stock stock	
	double biomassFishable(const Stock &stock, double min_age);

	/// @brief         Initialize chi, the fishing mortality scalar
	/// @param pop     Stock to fish
	/// @param F       Fishing mortality rate realized in feeding grounds ( = f_fgf * Fc)
	/// @param temp    Temperature
	void init_chi(Stock &pop, double F_fgf, double temp);

	void update_chi(const std::vector<double>& chi_in_windows, 
					const std::vector<double>& yield_in_windows, 
					const std::vector<double>& bs_in_windows,
					double yield_remainder, double bs_remainder);

	std::vector<double> harvest_dry_run(Stock pop, double quota, double temp);
	std::vector<double> harvest(Stock& pop, double quota, double temp, bool return_progress = false);

	double effort_constantC(double q, double b, double K);
	double effort_constantF(double q, double b, double K);

    double fishing_mort_constantC(const WindowProps &w, double K);
    double catch_rate_constantF(const WindowProps &w, double K);

private:

	template<class Func>
	double avgOverFishable(Func get_property, const Stock &stock, double min_age = 0){
		double mu = 0, n = 0;
		for (auto& f : stock.fishes){
			if (f.isAlive && f.age >= min_age){
				mu += fishability(f.length) * get_property(f); 
				n += fishability(f.length);
			}
		} 
		if (n == 0) return 0;
		else return mu/n;
	}

};

#endif

