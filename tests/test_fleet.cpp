#include <iostream>
#include <fish.h>
#include <fleet.h>
using namespace std;

int main(){

	string params_file = "params/fleet_1_params.ini";

    Fleet fleet;
	fleet.readParams(params_file, true);
    fleet.set_harvestProportion(0.5);
    fleet.par.print();

    return 0;
}
 