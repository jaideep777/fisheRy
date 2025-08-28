#include <iostream>
#include <fish.h>
#include <fishery_system.h>
using namespace std;

int main(){

	string params_file_fleet = "params/fleet_1_params.ini";
	string params_file_fish  = "params/cod_params.ini";

    // Fleet fleet;
	// fleet.readParams(params_file_fleet, true);
    // fleet.par.print();

    Fish fish(params_file_fish);

    Fishery fishery(params_file_fish, fish);
    fishery.par.print();

    fishery.set_harvestProp(0.5);
    fishery.addFleet(params_file_fleet, true);

    fishery.equilibriateNaturalPopulation(5.61, 2e6);

    fishery.init(1000, 0, 5.61);

    double quota = fishery.calc_quota(5.61);
    cout << "Quota: " << quota << endl;

    ofstream fout("test_fisherysystem.csv"); 
    for (int i=0; i< 100; ++i){
        auto out = fishery.update(5.61);
        cout << "i = "  << i << " "
             << "ssb = " << out[0] << " "
             << "tsb = " << out[1] << " "
             << "maturity = " << out[2] << " "
             << "quota = " << out[3] << " "
             << "yield = " << out[4] << " "
             << "effort = " << out[5] << "\n";

        fout << i << ","
             << out[0] << ","
             << out[1] << ","
             << out[2] << ","
             << out[3] << ","
             << out[4] << ","
             << out[5] << "\n";
    }
    fout.close();

    return 0;
}
 