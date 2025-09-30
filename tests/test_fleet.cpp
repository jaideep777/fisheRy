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

	Stock pop(fish);
	pop.readParams(params_file, false);
	pop.superfish_size = 2e6;

	auto v = pop.equilibriate_without_fishing(5.61);
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

	double h = 0.99;
	double F_fgf = -log(1-h);
	double quota = h*ssb_nf;

	Fleet fleet;
	fleet.readParams(params_file_fleet, true);
	fleet.par.print();
	// fleet.set_harvestProportion(0.99);

	fleet.debug = true;
	fleet.chi = 100;
	fleet.init_chi(pop, F_fgf, 5.61);

	cout << "Fleet initial chi = " << fleet.chi << endl;

	auto out = fleet.harvest_dry_run(pop, quota, 5.61);

	double effort = fleet.effort_constantC(fleet.par.q, fleet.par.b, fleet.biomassFishable(pop,0))*fleet.par.dsea;
	cout << "Effort (constant C) = " << effort << " vessel-days\n";

	double n = out.size();
	cout << "Yield expected = " << out[n-6] << " kg\n";
	cout << "Yield          = " << out[n-7] << " kg\n";
	
	double yield_error = (out[n-6] - out[n-7])/out[n-7]*100;
	cout << "Yield error    = " << yield_error << "%\n";

	return 0;
}

