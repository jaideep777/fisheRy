#include <iostream>
#include <fish.h>
#include <population.h>
using namespace std;

int main(){

	string params_file = "params/cod_params.ini";
	Fish f(params_file);
	f.init(1.93e3, 5.61);

	cout << "Length = " << f.length << endl;

	Population pop(f);
	pop.verbose = true;
	// pop.noFishingEquilibriate(1.93e3, 5.61);

	pop.init(1000, 1.93e3, 5.61);
	for (int t=0; t<10; ++t){
		pop.update();
	}

	cout << "Fishable biomass = " << pop.fishableBiomass()/1e9 << " MT" << endl;

}
