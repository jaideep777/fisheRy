#include <iostream>
#include <fish.h>
#include <fleet.h>
#include <cmath>
using namespace std;

int main(){

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

	Fleet fleet;
	fleet.readParams(params_file_fleet, true);
	fleet.par.print();
	fleet.set_harvestProportion(0.99);

	fleet.debug = true;
	fleet.control_model = "exp";
	fleet.chi = 100;

	cout << "Fleet initial chi = \n";
	for(double h = 0.0; h <= 0.99; h += 0.04){
		double F_fgf = -log(1-h);	
		fleet.init_chi(pop, F_fgf, 5.61);
		cout << h << "\t" << fleet.chi << endl;
	}

	return 0;
}

