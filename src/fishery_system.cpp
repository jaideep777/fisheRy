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
	harvest_prop = _h;
	for (auto& fl : fleets) {
		fl.set_harvestProportion(_h);
	}
}

void Fishery::set_minSizeLimit(double _lf50) {
	min_size_limit = _lf50;
	for (auto& fl : fleets) {
		fl.set_minSizeLimit(_lf50);
	}
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


void Fishery::addFleet(std::string params_file, bool verbose){
	Fleet fleet;
	fleet.readParams(params_file, verbose);
	fleets.push_back(fleet);
}


std::vector<double> Fishery::update(double temp){
	if (debug){
		// at the start of the step, ensure that all fish are alive and not caught
		for (auto& f : pop.fishes) assert(f.isAlive);
		for (auto& f : pop.fishes) assert(!f.isCaught);
	}

	// Reset the stock summary variables
	stock_summary = StockSummary();

	// Calculate number of fish and average mortality/maturity at the beginning of the season
	stock_summary.nfish_start = pop.fishes.size();

	stock_summary.Mort_fishable = pop.avgOverFishable(
		[temp](const Fish& f) { 
			return f.naturalMortalityRate(temp) + double(f.isMature)*f.par.Mspawning;
		}
	);

	stock_summary.Mat_fishable = pop.avgOverFishable(
		[temp](const Fish& f) { 
			return (f.isMature)? 1:0;
		}
	);

	// Calculate by-age metrics
	stock_summary.n_a = aggregateByAge([this](const Fish &f){
		return (f.isAlive)? pop.par.n : 0;
	});

	stock_summary.w_a = aggregateByAge([this](const Fish &f){
			return (f.isAlive)? pop.par.n*f.weight : 0;
	});
	for (int i=0; i<stock_summary.w_a.size(); ++i) stock_summary.w_a[i] /= (stock_summary.n_a[i]+1e-20);

	stock_summary.mat_a = aggregateByAge([this](const Fish &f){
		return (f.isAlive && f.isMature)? pop.par.n : 0;
	});
	for (int i=0; i<stock_summary.mat_a.size(); ++i) stock_summary.mat_a[i] /= (stock_summary.n_a[i]+1e-20);


	stock_summary.maturity = std::accumulate(fishes.begin(), fishes.end(), 0.0, 
		[](double sum, const Fish& f) { return sum + ((f.isAlive && f.isMature) ? 1 : 0); }) / fishes.size();

	stock_summary.nfish_ra = std::accumulate(fishes.begin(), fishes.end(), 0.0, 
		[this](double sum, const Fish& f) { return sum + ((f.isAlive && f.age == pop.par.recruitmentAge) ? pop.par.n : 0); });


	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//  0. Initialize fleets
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// compute average fishing mortality rate for each fleet - needed by fleets to initialize chi 
	for (auto& fl : fleets){
		fl.Fref_fishable = pop.avgOverFishable(
			[&fl](const Fish& f) { 
				return fl.fishingMortalityRef(f.length);
			}
		);
	}

	// Initialize fishing mortality rates of fleets
	for (auto& fl : fleets) fl.init_chi(1-stock_summary.Mat_fishable*par.rho);  

	// Initialize the fishing mortality rate for the spawning grounds fishery based on total harvest proportion
	double F_spf = par.rho * (-log(1-harvest_prop));

	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//  1. Maturation 
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// update maturity 
	for (auto& f : fishes){
		f.updateMaturity(temp);
	}

	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//  2. Growth
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stock_summary.ssb = pop.calcSSB(pop.par.recruitmentAge);
	stock_summary.tsb = pop.calcTSB(pop.par.recruitmentAge);

	for (auto& f : fishes){
		f.grow(stock_summary.tsb/1e6, temp); // convert tsb to kT
	}
	
	// calculate metrics to analyse density-inhibition on growth
	stock_summary.factor_dg = std::accumulate(fishes.begin(), fishes.end(), 0.0, 
		[](double sum, const Fish& f) { 
			return sum + f.dl_real/(f.dl_potential+1e-12); 
		}
	) / fishes.size();
	
	// calc max length
	stock_summary.lmax = std::accumulate(fishes.begin(), fishes.end(), 0.0, 
		[](double lmax, const Fish& f) { 
			return fmax(lmax,  f.length); 
		}
	);

	// Calculate average length of the top 5% of fish
	vector<Fish> ff = fishes;
	std::sort(ff.begin(), ff.end(), [](const Fish &f1, const Fish &f2){return f1.length > f2.length;});  // sort fishes descending by length
	for (int i=1; i<ff.size(); ++i) assert(ff[i].length <= ff[i-1].length); // Fixme: This is just checking whether the array got sorted, can go

	stock_summary.length90 = 0;
	double cut = 0.05;
	for (int i=0; i < ceil(cut*ff.size()); ++i) stock_summary.length90 += ff[i].length;
	stock_summary.length90 /= ceil(cut*ff.size());

	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//  3. Reproduction and Spawning grounds fishery
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	stock_summary.ssb0 = pop.calcSSB(pop.par.recruitmentAge);

	// 3a. pre-spawning part of the SPF
	double yield_spf = 0;
	double ssb_spawning = 0;
	double h_spf = 1-exp(-F_spf*1);
	double p_survival_spf_before = 1 - par.f_spf_before*h_spf;
	for (int k=0; k<fishes.size(); ++k) {
		auto &f = fishes[k];
		if (f.isAlive && f.isMature){ // only alive and mature fish are exposed to SPF
			f.isAlive = f.isAlive && (runif() <= p_survival_spf_before);

			if (f.isAlive) ssb_spawning += par.n * f.weight; // surviving individuals contribute to SSB
			if (!f.isAlive) yield_spf += par.n * f.weight;   // dying individuals contribute to yield
			if (!f.isAlive) f.isCaught = true;               // mark fish as caught
		}
	}
	double ssb_spawning_ref = ssb0*p_survival_spf_before;
	if (verbose) cout << "ssb spawning = " << ssb_spawning << " / " << ssb_spawning_ref << endl;
	

}

/*
Questions:
1. Should lmin a fleet-specific parameter?
2. Thus, is fishable biomass of the population only valid from the perspective of a given fleet?
3. Which all socio-economic parameters should move to Fleet?

4. Summary variables are calculated in certain order. What happens when we make life sequence a parameter?
*/
