#include <fishery_system.h>

Fishery::Fishery(std::string _params_file, const Fish& f) : I(), no_fishing_pop(f), pop(f) {
	params_file = _params_file;
	// I.parse(params_file, false, true);
	pop.readParams(params_file);
	no_fishing_pop.readParams(params_file);
}

// ---------------------------------------------------------
// Wrapper functions for enabling R interface for Fishery
// ---------------------------------------------------------
int Fishery::readParams(std::string filename, bool verbose) {
	return pop.readParams(filename, verbose);
}

void Fishery::set_superFishSize(double _n) {
	pop.set_superFishSize(_n);
}

int Fishery::readEnvironmentFile(std::string filename) {
	return pop.readEnvironmentFile(filename);
}

void Fishery::updateEnv(double t) {
	pop.updateEnv(t);
}

void Fishery::set_harvestProp(double _h) {
	pop.set_harvestProp(_h);
}

void Fishery::set_minSizeLimit(double _lf50) {
	pop.set_minSizeLimit(_lf50);
}

void Fishery::set_traitVariances(std::vector<double> var) {
	pop.set_traitVariances(var);
}

void Fishery::init(int n, double temp){
	pop.init(n, temp);
}

void Fishery::noFishingEquilibriate(double temp){
	pop.noFishingEquilibriate(temp);
}


// ---------------------------------------------------------
// Fishery functions
// ---------------------------------------------------------

std::vector<double> Fishery::equilibriateNaturalPopulation(double temp, double _n){
	no_fishing_pop.set_superFishSize(_n);
	no_fishing_pop.set_traitVariances({0,0,0,0,0,0});
	return no_fishing_pop.noFishingEquilibriate(temp);
}

std::vector<double> Fishery::update(double temp){
	if (debug){
		// at the start of the step, ensure that all fish are alive and not caught
		for (auto& f : pop.fishes) assert(f.isAlive);
		for (auto& f : pop.fishes) assert(!f.isCaught);
	}

	int nfish_start = pop.fishes.size();

	
    return pop.update(temp); // Update the population state and return the state vector
}

/*
Questions:
1. Should lmin a fleet-specific parameter?
2. Thus, is fishable biomass of the population only valid from the perspective of a given fleet?
3. Which all socio-economic parameters should move to Fleet?
*/
