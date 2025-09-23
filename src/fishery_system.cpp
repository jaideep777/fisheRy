#include <fishery_system.h>
#include <fstream>

inline double runif(double rmin=0, double rmax=1){
	double r = double(rand())/RAND_MAX; 
	return rmin + (rmax-rmin)*r;
}


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
	pop.superfish_size = _n;
}

int Fishery::readEnvironmentFile(std::string filename) {
	// return pop.readEnvironmentFile(filename);
	return 0;
}

void Fishery::updateEnv(double t) {
	// pop.updateEnv(t);
}

void Fishery::set_harvestProp(double _h) {
	harvest_prop = _h;
}

void Fishery::set_minSizeLimit(double _lf50) {
	min_size_limit = _lf50;
	for (auto& fl : fleets) {
		fl.set_minSizeLimit(_lf50);
	}
}

double Fishery::calc_quota(double temp){
	if (fleets.empty()) {
		throw std::runtime_error("Fishery: No fleets defined, cannot calculate quota.");
	}
	
	auto& fl = fleets[0]; // Since lmin is the same across all fleets, we use the first fleet to calculate quota
	double Fc = -log(1-harvest_prop);
	
	// Calculate chi of the fleet to implement mortality
	fl.chi = Fc/fl.FishingMortalityRef_avgl(200, 100);

	// Calculate expected Catch with the given chi
	// Note: Be careful to not modify the fishes vector here
	double expected_catch = 0, to_sea_bed = 0;
	for (const auto& f: pop.fishes) {  // const auto& ensures we do not modify the fish 
		if (!f.isAlive) continue; // Skip already dead fishes

		double natural_mort_rate = f.naturalMortalityRate(temp);
		double fishing_mort_rate = fl.fishingMortality(f.length);
		double mortality_rate = natural_mort_rate + fishing_mort_rate;

		double survival_prob = exp(-mortality_rate*1.0);
		
		bool f_isAlive = f.isAlive && (runif() <= survival_prob);	// set the fish to die probabilistically, if not dead already.

		if (!f_isAlive){
			bool f_isCaught = runif() < fishing_mort_rate/mortality_rate; // check if fish is caught or goes to sea bed!
			
			if (f_isCaught) expected_catch += pop.superfish_size*f.weight; // if caught, add to yield
			else to_sea_bed += pop.superfish_size*f.weight;       // else, goes to sea bed
		}
			
	}

	return expected_catch;
}


// void Fishery::set_traitVariances(std::vector<double> var) {
// 	pop.set_traitVariances(var);
// }

void Fishery::init(int n, double t_init, double temp){
	pop.init(n, t_init, temp);
}


// ---------------------------------------------------------
// Fishery functions
// ---------------------------------------------------------

std::vector<double> Fishery::equilibriateNaturalPopulation(double temp, double _n){
	no_fishing_pop.superfish_size = _n;
	// no_fishing_pop.set_traitVariances({0,0,0,0,0,0});
	return no_fishing_pop.equilibriate_without_fishing(temp);
}


void Fishery::addFleet(std::string params_file, bool verbose){
	fleets.emplace_back();
	fleets.back().readParams(params_file, verbose);
}


// std::vector<double> Fishery::update(double temp){
// 	if (debug){
// 		// at the start of the step, ensure that all fish are alive and not caught
// 		for (auto& f : pop.fishes) assert(f.isAlive);
// 		for (auto& f : pop.fishes) assert(!f.isCaught);
// 	}

// 	// Reset the stock summary variables
// 	stock_summary = StockSummary();

// 	// Calculate number of fish and average mortality/maturity at the beginning of the season
// 	stock_summary.nfish_start = pop.fishes.size();

// 	stock_summary.Mort_fishable = pop.avgOverFishable(
// 		[temp](const Fish& f) { 
// 			return f.naturalMortalityRate(temp) + double(f.isMature)*f.par.Mspawning;
// 		}
// 	);

// 	stock_summary.Mat_fishable = pop.avgOverFishable(
// 		[temp](const Fish& f) { 
// 			return (f.isMature)? 1:0;
// 		}
// 	);

// 	// Adundance at age
// 	stock_summary.n_a = aggregateByAge([this](const Fish &f){
// 		return (f.isAlive)? pop.par.n : 0;
// 	});

// 	// Avg weight at age
// 	stock_summary.w_a = aggregateByAge([this](const Fish &f){
// 			return (f.isAlive)? pop.par.n*f.weight : 0;
// 	});
// 	for (int i=0; i<stock_summary.w_a.size(); ++i) stock_summary.w_a[i] /= (stock_summary.n_a[i]+1e-20);

// 	// Maturity at age
// 	stock_summary.mat_a = aggregateByAge([this](const Fish &f){
// 		return (f.isAlive && f.isMature)? pop.par.n : 0;
// 	});
// 	for (int i=0; i<stock_summary.mat_a.size(); ++i) stock_summary.mat_a[i] /= (stock_summary.n_a[i]+1e-20);

// 	// Overall maturity
// 	stock_summary.maturity = std::accumulate(fishes.begin(), fishes.end(), 0.0, 
// 		[](double sum, const Fish& f) { 
// 			return sum + ((f.isAlive && f.isMature) ? 1 : 0); 
// 		}
// 	) / fishes.size();

// 	// Number of fish at recruitment age
// 	stock_summary.nfish_ra = std::accumulate(fishes.begin(), fishes.end(), 0.0, 
// 		[this](double sum, const Fish& f) { 
// 			return sum + ((f.isAlive && f.age == pop.par.recruitmentAge) ? pop.par.n : 0); 
// 		}
// 	);

// 	// Spawing and total stock biomass
// 	stock_summary.ssb = pop.calcSSB(pop.par.recruitmentAge);
// 	stock_summary.tsb = pop.calcTSB(pop.par.recruitmentAge);

// 	// Max length
// 	stock_summary.lmax = std::accumulate(fishes.begin(), fishes.end(), 0.0, 
// 		[](double lmax, const Fish& f) { 
// 			return fmax(lmax,  f.length); 
// 		}
// 	);

// 	// Average length of the top 5% fish
// 	vector<Fish> ff = fishes;
// 	std::sort(ff.begin(), ff.end(), [](const Fish &f1, const Fish &f2){return f1.length > f2.length;});  // sort fishes descending by length
// 	for (int i=1; i<ff.size(); ++i) assert(ff[i].length <= ff[i-1].length); // Fixme: This is just checking whether the array got sorted, can go

// 	stock_summary.length90 = 0;
// 	double cut = 0.05;
// 	for (int i=0; i < ceil(cut*ff.size()); ++i) stock_summary.length90 += ff[i].length;
// 	stock_summary.length90 /= ceil(cut*ff.size());


// 	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 	//  0. Initialize fleets
// 	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 	// compute average fishing mortality rate for each fleet - needed by fleets to initialize chi 
// 	for (auto& fl : fleets){
// 		fl.Fref_fishable = pop.avgOverFishable(
// 			[&fl](const Fish& f) { 
// 				return fl.fishingMortalityRef(f.length);
// 			}
// 		);
// 	}

// 	// Initialize fishing mortality rates of fleets
// 	for (auto& fl : fleets) fl.init_chi(1-stock_summary.Mat_fishable*par.rho);  

// 	// Initialize the fishing mortality rate for the spawning grounds fishery based on total harvest proportion
// 	double F_spf = par.rho * (-log(1-harvest_prop));

// 	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 	//  1. Maturation 
// 	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 	// update maturity 
// 	for (auto& f : fishes){
// 		f.updateMaturity(temp);
// 	}

// 	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 	//  2. Growth
// 	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 	for (auto& f : fishes){
// 		f.grow(stock_summary.tsb/1e6, temp); // convert tsb to kT
// 	}
	
// 	// calculate metrics to analyse density-inhibition on growth
// 	stock_summary.factor_dg = std::accumulate(fishes.begin(), fishes.end(), 0.0, 
// 		[](double sum, const Fish& f) { 
// 			return sum + f.dl_real/(f.dl_potential+1e-12); 
// 		}
// 	) / fishes.size();
	
// 	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 	//  3. Reproduction and Spawning grounds fishery
// 	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 	stock_summary.ssb0 = pop.calcSSB(pop.par.recruitmentAge);
	
// 	// 3a. pre-spawning part of the SPF
// 	double yield_spf = 0;
// 	double ssb_spawning = 0;
// 	double h_spf = 1-exp(-F_spf*1);
// 	double p_survival_spf_before = 1 - par.f_spf_before*h_spf;
// 	for (int k=0; k<fishes.size(); ++k) {
// 		auto &f = fishes[k];
// 		if (f.isAlive && f.isMature){ // only alive and mature fish are exposed to SPF
// 			f.isAlive = f.isAlive && (runif() <= p_survival_spf_before);

// 			if (f.isAlive) ssb_spawning += par.n * f.weight; // surviving individuals contribute to SSB
// 			if (!f.isAlive) yield_spf += par.n * f.weight;   // dying individuals contribute to yield
// 			if (!f.isAlive) f.isCaught = true;               // mark fish as caught
// 		}
// 	}
// 	double ssb_spawning_ref = ssb0*p_survival_spf_before;
// 	if (verbose) cout << "ssb spawning = " << ssb_spawning << " / " << ssb_spawning_ref << endl;

// 	// 3b. Spawning
// 	// double nrecruits = par.r0*ssb / (1 + ssb/par.Bhalf); // * exp(rnorm(-par.sigmaf*par.sigmaf/2, par.sigmaf));
// 	double nspawners = 0;
// 	nrecruits_vec.resize(fishes.size());
// 	std::fill(nrecruits_vec.begin(), nrecruits_vec.end(), 0.0);
// 	double nrecruits_total = 0;
// 	double nrecruits_potential = 0;
// 	for (int k=0; k<fishes.size(); ++k) {
// 		auto &f = fishes[k];
// 		// nrecruits += par.r0*n*f.weight/(1+ssb/par.Bhalf);
// 		if (f.isAlive && f.isMature){  // fish survies the spawning-grounds fishery until actual spawning time
// 			// count as spawner (for analysis only)
// 			nspawners += par.n; 

// 			// Recruitment
// 			double nrecruits_fish = f.produceRecruits(ssb_spawning, temp) * par.n;
// 			nrecruits_vec[k] = nrecruits_fish;
// 			nrecruits_total     += nrecruits_fish; // * (1/(1+ssb/f.par.Bhalf));
// 			nrecruits_potential += f.produceRecruits(  0, temp) * par.n;

// 			// Mortality due to spawning
// 			double p_survival_spawning = exp(-f.par.Mspawning);
// 			f.isAlive = f.isAlive && (runif() <= p_survival_spawning);
// 		}
// 	}
// 	//nrecruits *= exp(rnorm(-par.sigmaf*par.sigmaf/2, par.sigmaf));
// 	double nrecruits_real = std::min(nrecruits_total, par.rmax);
// //	for (auto& nn : nrecruits_vec) nn = nn*nrecruits_real/(nrecruits_total+1e-20); 

// 	// ** for analysis
// 	double r0_avg = (ssb_spawning>0)? (nrecruits_real * (1 + ssb_spawning/proto_fish.par.Bhalf) / ssb_spawning) : -999;
// 	double factor_dr = nrecruits_real / (nrecruits_potential+1e-12);
// 	double nrecruits_per_fish = nrecruits_real/nspawners;
// 	double ssb_after_spawning = calcSSB(par.recruitmentAge);
// 	if (verbose) cout << "n_spawners / n_recruits = " << nspawners << " / " << nrecruits_real << endl;
// 	// **

// 	// Generate recruits (in a separate vector)
// 	int nr = nrecruits_real/par.n;
// 	if (nr <= 0) nr = 1;
// 	std::discrete_distribution<size_t> fitness_dist(nrecruits_vec.begin(), nrecruits_vec.end());
// 	++proto_fish.t_birth;
// 	if (verbose) cout << "n_recruits (actual) = " << nr << endl;

// 	vector<Fish> recruits;
// 	recruits.reserve(nr);

// 	for (int i=0; i<nr; ++i){
// 		vector<double> mother_traits = fishes[fitness_dist(generator)].get_traits();
// 		vector<double> father_traits = fishes[fitness_dist(generator)].get_traits();
// 		vector<double> offspring_traits(mother_traits.size());
// 		for (int k=0; k<mother_traits.size(); ++k){
// 			offspring_traits[k] = (mother_traits[k] + father_traits[k])/2 + sqrt(proto_fish.trait_variances[k])*proto_fish.trait_scalars[k]*normal_dist(generator);
// 		}
// 		proto_fish.set_traits(offspring_traits);
// 		proto_fish.init(tsb/1e6, temp);
// 		recruits.push_back(proto_fish);
// 	}

// 	double ssb_after_spawning_ref = ssb0*exp(-proto_fish.par.Mspawning)*(1-par.f_spf_before*h_spf);
// 	if (verbose) cout << "ssb after spawning = " << ssb_after_spawning << " / " << ssb_after_spawning_ref << endl;

// 	// 3c. post-spawning part of the SPF
// 	double p_survival_spf_after = (1-h_spf)/(1 - par.f_spf_before*h_spf);
// 	for (int k=0; k<fishes.size(); ++k) {
// 		auto &f = fishes[k];
// 		if (f.isAlive && f.isMature){ // only mature fish are exposed to SPF
// 			f.isAlive = f.isAlive && (runif() <= p_survival_spf_after);

// 			if (!f.isAlive) yield_spf += par.n * f.weight;
// 			if (!f.isAlive) f.isCaught = true;               // mark fish as caught

// 		}
// 	}

// 	double ssbn = calcSSB(par.recruitmentAge);

// 	double yield_spf_ref = ssb0*h_spf*(par.f_spf_before + (1-par.f_spf_before)*exp(-proto_fish.par.Mspawning));
// 	double ssbn_ref = ssb0*(1-h_spf)*exp(-proto_fish.par.Mspawning);


// }


void Fishery::summarize_population_metrics(){
	// Calc by-age metrics
	stock_summary.n_a = pop.aggregateByAge([this](const Fish &f){
		return (f.isAlive)? pop.superfish_size : 0;
	});

	stock_summary.w_a = pop.aggregateByAge([this](const Fish &f){
			return (f.isAlive)? pop.superfish_size*f.weight : 0;
		});
	for (int i=0; i<stock_summary.w_a.size(); ++i) stock_summary.w_a[i] /= (stock_summary.n_a[i]+1e-20);

	stock_summary.mat_a = pop.aggregateByAge([this](const Fish &f){
			return (f.isAlive && f.isMature)? pop.superfish_size : 0;
		});
	for (int i=0; i<stock_summary.mat_a.size(); ++i) stock_summary.mat_a[i] /= (stock_summary.n_a[i]+1e-20);
}


void Fishery::summarize_catch_metrics(){
	// Calc by-age metrics in catch
	stock_summary.nc_a = pop.aggregateByAge([this](const Fish &f){
			return (!f.isAlive && f.isCaught)? pop.superfish_size : 0;
		});

	stock_summary.wc_a = pop.aggregateByAge([this](const Fish &f){
			return (!f.isAlive && f.isCaught)? pop.superfish_size*f.weight : 0;
		});
	for (int i=0; i<stock_summary.wc_a.size(); ++i) stock_summary.wc_a[i] /= (stock_summary.nc_a[i]+1e-20);
}


std::vector<double> Fishery::update(double temp){
	stock_summary = StockSummary(); // reset stock summary for each year. FIXME: Maybe better to do outside 

	double ssb = pop.calcSSB(pop.par.recruitmentAge);
	double tsb = pop.calcTSB(pop.par.recruitmentAge);
	double maturity = pop.calcMaturity(pop.par.recruitmentAge);

	// 1. Maturation
	for (auto& f: pop.fishes) f.updateMaturity(temp);
	summarize_population_metrics();

	// 2. Growth
	for (auto& f: pop.fishes) f.grow(tsb/1e6, temp); // convert tsb to kT

	// 3. Calculate total quota (feeding + spawning grounds fishery)
	double quota = calc_quota(temp);
	double quota_fgf = quota * (1 - par.rho); // quota for the feeding grounds fishery
	double quota_spf = quota * par.rho; // quota for the spawning grounds fishery

	// 4. Reproduction
	std::vector<Fish> recruits = pop.spawn(ssb, tsb, temp, stock_summary);

	// 5. Feeding grounds fishery and natural mortality
	double yield = 0, effort = 0;
	if (!fleets.empty()) {
		fleets[0].init_chi(pop, -log(1-harvest_prop), temp); // initialize chi for the feeding grounds fishery
		std::vector<double> harvest_out = fleets[0].harvest(pop, quota_fgf, temp, false);
		yield = harvest_out[0]; // total yield from the feeding grounds fishery
		effort = 0;
	}
	summarize_catch_metrics();

	// 6. remove dead fish from population
	pop.fishes.erase(std::remove_if(pop.fishes.begin(), pop.fishes.end(), [](Fish &f){return !f.isAlive;}), pop.fishes.end());

	// 7. Increment age and advance to new year
	for (auto& f: pop.fishes)  f.set_age(f.age+1);

	// 8. Finally, add recruits to population 
	pop.fishes.insert(pop.fishes.end(), recruits.begin(), recruits.end());
	
	// 9. Calculate metrics for analysis
	return {
		ssb, 
		tsb,
		maturity,
		quota_fgf,
		yield,
		effort
	};
}


/*
Questions:
1. Should lmin a fleet-specific parameter?
2. Thus, is fishable biomass of the population only valid from the perspective of a given fleet?
3. Which all socio-economic parameters should move to Fleet?

4. Summary variables are calculated in certain order. What happens when we make life sequence a parameter?
*/

// ************ R stuff *****************
#ifndef NATIVE_CPP

Rcpp::DataFrame Fishery::simulate_r(double lf, double h, int nyears, double tsb0, double temp, bool re_init, std::string output_file){
	bool writestate = (output_file != "");

	std::ofstream fout;
	if (writestate){
		fout.open(output_file.c_str());
		fout << "Year" << ',' 
		     << "age" << ',' 
			 << "N" << ',' 
			 << "weight" << ',' 
			 << "mat" << ',' 
			 << "catch_N" << ',' 
			 << "catch_weight"
			 << '\n';

	}

	// no_fishing_pop.set_harvestProp(h);
	// no_fishing_pop.set_minSizeLimit(lf);
	// double K = no_fishing_pop.fishableBiomass();
	// std::cout << "h/lf = " << h << " / " << lf << " | K = " << K << std::endl;

	// pop.K_fishableBiomass = K;
	// pop.set_harvestProp(h);
	// pop.set_minSizeLimit(lf);
	if (re_init) pop.init(1000, 0, temp);

	std::vector<std::vector<double>> columns(colnames.size());
	for (auto& vec : columns) vec.reserve(nyears);
	
	Rcpp::DataFrame df = Rcpp::DataFrame::create();

	for (int i=0; i<nyears; ++i){
		std::vector<double> state_now = update(temp);
		
		for (int col=0; col<state_now.size(); ++col){
			columns[col].push_back(state_now[col]);
		}

		// write age-wise summaries to file
		if (writestate){
			for (int a=0; a < stock_summary.n_a.size(); ++a){
				fout << i << ',' 
					 << a << ',' 
					 << stock_summary.n_a[a] << ',' 
					 << stock_summary.w_a[a] << ',' 
					 << stock_summary.mat_a[a] << ',' 
					 << stock_summary.nc_a[a] << ',' 
					 << stock_summary.wc_a[a]
					 << '\n';
			}
		}

	}

	// put summarized population state in dataframe
	for (int i=0; i<columns.size(); ++i){	
		// if (verbose) std::cout << "Adding columns[" << i << "] = " << colnames[i] << std::endl; 
		df.push_back(columns[i], colnames[i]);
	}

	if (writestate) fout.close();

	return df;
}

#endif
