#include <fishery_system.h>

Fishery::Fishery(std::string _params_file, const Fish& f) : I(), no_fishing_pop(f), pop(f) {
	params_file = _params_file;
	// I.parse(params_file, false, true);
	pop.readParams(params_file);
	no_fishing_pop.readParams(params_file);
}


std::vector<double> Fishery::equilibriateNaturalPopulation(double temp, double _n){
	no_fishing_pop.set_superFishSize(_n);
	no_fishing_pop.set_traitVariances({0,0,0,0,0,0});
	return no_fishing_pop.noFishingEquilibriate(temp);
}

void Fishery::init(int n, double temp){
	pop.init(n, temp);
}

Population& Fishery::get_pop(){
    return pop;
}
