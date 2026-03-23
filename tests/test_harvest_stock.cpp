#include <iostream>
#include <fish.h>
#include <fleet.h>
#include <cmath>
using namespace std;

int main(){

	srand(0);

	string params_file = "params/cod_params.ini";
	string params_file_fleet = "params/fleet_1_params.ini";

	Fish fish(params_file);
	fish.setMortalityCurveEmpirical("data/naturalmort.spline.csv");

	Stock pop(fish);
	pop.readParams(params_file, false);
	pop.superfish_size = 2e6;

	auto v = pop.equilibriate_without_fishing(5.61, 200);
	double ssb0 = v[199*6 + 0];
	double tsb0 = v[199*6 + 1];
	double maturity0 = v[199*6 + 2];
	double nrecruits_real = v[199*6 + 3];
	double factor_dr = v[199*6 + 4];
	double nfish = v[199*6 + 5];
	cout << "Equilibrium without fishing:\n";
	cout << "  SSB0 = " << ssb0 << " kg\n";
	cout << "  TSB0 = " << tsb0 << " kg\n";
	cout << "  Maturity0 = " << maturity0 << "\n";
	cout << "  Recruits = " << nrecruits_real << "\n";
	cout << "  Factor dr = " << factor_dr << "\n";
	cout << "  Number of fish = " << nfish << "\n";


	double ssb_nf = pop.calcSSB(pop.par.recruitmentAge);
	cout << "SSB at equilibrium without fishing: " << ssb_nf << " kg\n";

	double h = 0.6;
	double F_fgf = -log(1-h);
	double quota = h*ssb_nf;

	Fleet fleet;
	fleet.readParams(params_file_fleet, true);
	fleet.set_referenceFishingMortalityCurveEmpirical("data/selection.spline.reduced.csv", 45);
	fleet.par.print();
	// fleet.set_harvestProportion(0.99);

	fleet.debug = false;
	fleet.chi = 100;
	fleet.init_chi(pop, F_fgf, 5.61);

	cout << "Fleet initial chi = " << fleet.chi << endl;

	vector<Fleet> fleets = {fleet};
	auto out = pop.get_fished_dry_run(fleets, {quota}, 5.61, true, true);

	vector<std::string> colnames = {
		"age",
		"B",
		"B_sampled",
		"yield",
		"yield_expected",
		"chi",
		"chi_w",
		"B_sampled",
		"B_start",
		"yield",
		"F_fishable",
		// Below effort calc is only for debugging
		"effort_C",
		"effort_F"
	};
	for (int i=0; i<colnames.size(); ++i) cout << colnames[i] << " "; cout << endl;
	
	for(int nrow=0; nrow < out.size()/colnames.size(); ++nrow){
		for (int i=0; i<colnames.size(); ++i) cout << out[nrow*colnames.size() + i] << " ";
		cout << "\n";
	}

	double effort = fleets[0].effort_constantC(fleet.par.q, fleet.par.b, fleet.biomassFishable(pop,0,true))*fleet.par.dsea;
	cout << "Effort (constant C) = " << effort << " vessel-days\n";

	int nrow = out.size()/colnames.size();
	int i_yield = (nrow-1)*colnames.size()+3;
	int i_yield_exp = (nrow-1)*colnames.size()+4;
	cout << "Yield expected = " << out[i_yield_exp] << " kg\n";
	cout << "Yield          = " << out[i_yield] << " kg\n";
	
	double yield_error = (out[i_yield] - out[i_yield_exp])/out[i_yield_exp]*100;
	cout << "Yield error    = " << yield_error << "%\n";

	return 0;
}

