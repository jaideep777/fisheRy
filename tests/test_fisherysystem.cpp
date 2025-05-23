#include <iostream>
#include <fish.h>
#include <fishery_system.h>
using namespace std;

int main(){

	string params_file_fleet = "params/fleet_1_params.ini";
	string params_file_fish  = "params/cod_params.ini";

    Fleet fleet;
	fleet.readParams(params_file_fleet, true);
    fleet.set_harvestProportion(0.5);
    fleet.par.print();

    Fish fish(params_file_fish);

    Fishery fishery(params_file_fish, fish);
    
    fishery.par.print();

    return 0;
}
 