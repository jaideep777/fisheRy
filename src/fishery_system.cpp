#include <fishery_system.h>
#include <fstream>

inline double runif(double rmin=0, double rmax=1){
	double r = double(rand())/RAND_MAX; 
	return rmin + (rmax-rmin)*r;
}

int FisheryParams::initFromFile(std::string filename, bool verbose){
	io::Initializer I;
	I.parse(filename, false, verbose);

	#define READ_PAR(x) x = I.get<double>("fishery", #x)

	READ_PAR(rho);
	READ_PAR(f_spf_before);

	// get status quo lmin
	READ_PAR(lmin);
	lmin_sq = lmin;

	// management / fishing selectivity parameters at status quo lmin
	READ_PAR(F1);
	READ_PAR(F2);
	READ_PAR(F3);
	READ_PAR(F4);
	READ_PAR(F5);
	READ_PAR(F6);

	// save the values of F3 and F5 corresponding to status quo lmin
	F3_sq = F3;
	F5_sq = F5;

	using_empirical_fref = (I.get<std::string>("fishery", "using_empirical_fref") == "true")? true : false;
	Fref_empirical_file = I.get<std::string>("fishery", "Fref_empirical_file");

	#undef READ_PAR

	return 0;
}

void FisheryParams::print(){

	#define PRINT_PAR(x) std::cout << #x << " = " << x << "\n"

	PRINT_PAR(rho);
	PRINT_PAR(f_spf_before);

	// status quo lmin
	PRINT_PAR(lmin);
	// PRINT_PAR(lmin_sq);

	// management / fishing selectivity
	PRINT_PAR(F1);
	PRINT_PAR(F2);
	PRINT_PAR(F3);
	PRINT_PAR(F4);
	PRINT_PAR(F5);
	PRINT_PAR(F6);

	PRINT_PAR(using_empirical_fref);
	PRINT_PAR(Fref_empirical_file);

}


Fishery::Fishery(std::string _params_file, const Fish& f) : I(), no_fishing_pop(f), pop(f) {
	params_file = _params_file;
	// I.parse(params_file, false, true);
	no_fishing_pop.readParams(params_file);
	this->readParams(params_file, false);
}

void Fishery::set_referenceFishingMortalityCurve(Fleet &fleet){
	if (par.using_empirical_fref){
		fleet.set_referenceFishingMortalityCurveEmpirical(par.Fref_empirical_file, par.lmin);
	}
	else {
		fleet.set_referenceFishingMortalityCurveLogistic(par.F1, par.F2, par.F3, par.F4, par.F5, par.F6, par.lmin);
	}
}

// ---------------------------------------------------------
// Wrapper functions for enabling R interface for Fishery
// ---------------------------------------------------------
int Fishery::readParams(std::string filename, bool verbose) {
	// update parameters
	par.initFromFile(filename, verbose);

	// update population based on new parameters
	pop.readParams(filename, verbose);

	// Update the effective fleet and all other fleets based on new parameters
	set_referenceFishingMortalityCurve(fleet_effective);
	for (auto& fl : fleets) {
		set_referenceFishingMortalityCurve(fl);
	}
	
	return 0;
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

void Fishery::set_minSizeLimit(double _lf50){
	double dl = _lf50 - par.lmin_sq;

	par.lmin = par.lmin_sq + dl;
	par.F3   = par.F3_sq + dl;
	par.F5   = par.F5_sq + dl;

	// If using empirical fishing mort, throw error if lmin is changed
	if (par.using_empirical_fref && fabs(dl) > 1e-6) throw std::runtime_error("Cannot alter lmin when using empirical fishing mortality function");

	// Else update the effective fleet and all other fleets based on new parameters
	set_referenceFishingMortalityCurve(fleet_effective);
	for (auto& fl : fleets) {
		set_referenceFishingMortalityCurve(fl);
	}
}


/// This function calculates quota based on control Fishing mortality rate Fc. It can also take a notional harvest proportion h which is 
/// first converted to fishing mortality rate (Fc = -log(1-h)) and then used.
/// If different fleets have different selectivity curves Fref1, Fref2, ..., then Eq 1 must hold: X1*Fref1_avg + X2*Fref2_avg + ... = Fc = X_eff*Fref_effective_total_avg, where X1, X2, ... 
/// where X1, X2, ... are the scalars for each fleet. Then with the fishing mortality X_eff*Fref_effective_total, we get a quota of Q (say).
/// which can be divided into quotas of individual fleets Qi = (X1*Frefi_avg/Fc)*Q. However, for this to work, Eq. 1 must hold for all l, i.e., the selectivity curves must at most differ scalar multiples (absorbed in X). 
/// In that case, Qi = (Xi/(X1+X2+X3)))*Q. 
/// Note: Quota is calculated based on Census time weights (before growth)
/// Note: Quota should only count fish above min size limit
/// This function calculates the split between natural mort and fishing mort deterministically by catching a fraction of the superfish. This works here because we're not actually killing the fish.
double Fishery::calc_quota(double temp){
	// FishingMortalityRef curve applies to the entire fishery, so we calculate quota from the effective fleet
	auto& fl = fleet_effective; 
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
		double catch_prob = fishing_mort_rate/mortality_rate;

		bool f_isAlive = f.isAlive && (runif() <= survival_prob);	// set the fish to die probabilistically, if not dead already.

		if (!f_isAlive){
			if (f.length >= min_size_limit){
				expected_catch += catch_prob * pop.superfish_size * f.weight; // fraction catch_prob of this superfish goes to yield
			}
		}
			
	}

	return expected_catch;
}

std::vector<double> Fishery::harvest(double quota, double temp, bool return_progress){
    return std::vector<double>();
}

// void Fishery::set_traitVariances(std::vector<double> var) {
// 	pop.set_traitVariances(var);
// }


// ---------------------------------------------------------
// Fishery functions
// ---------------------------------------------------------

void Fishery::init(int n, double t_init, double temp){
	pop.init(n, t_init, temp);
}

std::vector<double> Fishery::equilibriateNaturalPopulation(double temp, double _n, int nsteps){
	no_fishing_pop.superfish_size = _n;
	// no_fishing_pop.set_traitVariances({0,0,0,0,0,0});
	return no_fishing_pop.equilibriate_without_fishing(temp, nsteps);
}

std::vector<double> Fishery::equilibriateWithoutFishing(double temp, int nsteps){
	return pop.equilibriate_without_fishing(temp, nsteps);

}

void Fishery::addFleet(std::string params_file, bool verbose){
	fleets.emplace_back();
	fleets.back().readParams(params_file, verbose);
	set_referenceFishingMortalityCurve(fleets.back());

	colnames.push_back("yield_"+std::to_string(fleets.size()));
	colnames.push_back("effort_"+std::to_string(fleets.size()));
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

	stock_summary.nfish_ra = stock_summary.n_a[pop.par.recruitmentAge];
}


void Fishery::summarize_catch_metrics(bool use_average_weight){
	// Calc by-age metrics in catch
	stock_summary.nc_a = pop.aggregateByAge([this](const Fish &f){
			return (!f.isAlive)? f.fraction_caught*pop.superfish_size : 0;
		});

	// This calc comes after growth so current weight is inclusive of dw, hence subtract dw/2 to get average weight
	stock_summary.wc_a = pop.aggregateByAge([use_average_weight, this](const Fish &f){
			double f_weight_avg = (use_average_weight)? (f.weight - f.delta_weight/2) : f.weight;
			return (!f.isAlive)? f.fraction_caught*pop.superfish_size*f_weight_avg : 0;
		});
	for (int i=0; i<stock_summary.wc_a.size(); ++i) stock_summary.wc_a[i] /= (stock_summary.nc_a[i]+1e-20);
}


void Fishery::summarize_spawner_fishery_metrics(const std::vector<double>& spf_summary_before, const std::vector<double>& spf_summary_after, double quota_spf){
	stock_summary.ssb0 = spf_summary_before[0]; // ssb_before;  ///< SSB just before spawning and SPF
	stock_summary.ssb_spawning = spf_summary_before[1]; // ssb_remaining   ///< SSB at spawning time
	stock_summary.ssb_after_spawning = spf_summary_after[0]; // ssb_before   ///< SSB after spawning
	stock_summary.ssbn = spf_summary_after[1]; // ssb_remaining  ///< Final SSB after all mortality

	double ssb0 = stock_summary.ssb0;
	double h_spf_ref = (ssb0 == 0)? 0 : quota_spf/ssb0;
	double p_survival_spf_before = 1 - par.f_spf_before*h_spf_ref;
	stock_summary.ssb_spawning_ref = ssb0*p_survival_spf_before;  ///< Reference SSB at spawning 
	stock_summary.ssb_after_spawning_ref = ssb0*exp(-pop.proto_fish.par.Mspawning)*(1-par.f_spf_before*h_spf_ref); ///< Reference SSB after spawning
	stock_summary.ssbn_ref = ssb0*(1-h_spf_ref)*exp(-pop.proto_fish.par.Mspawning); ///< Reference final SSB
}


// TODO: Move to a spawner fleet?
std::vector<double> Fishery::spawner_fishery(double quota){
	double ssb_before = pop.calcSSB(pop.par.recruitmentAge);
	double yield_spf = 0;

	double h_spf = std::clamp(quota/(ssb_before+1e-12), 0.0, 1.0);
	double p_survival = 1 - h_spf;
	double ssb_remaining = 0;
	for (auto &f : pop.fishes) {
		if (f.isAlive && f.isMature){ // only alive and mature fish are exposed to SPF
			f.isAlive = f.isAlive && (runif() <= p_survival);

			if (f.isAlive) ssb_remaining += pop.superfish_size*f.weight; // surviving individuals contribute to SSB
			if (!f.isAlive) yield_spf += pop.superfish_size*f.weight;   // dying individuals contribute to yield
			if (!f.isAlive) f.fraction_caught = 1;               // mark fish as caught
		}
	}
	
	return {ssb_before, ssb_remaining, yield_spf};
}

// Which weight should be used for quota calc? Before growth, so (w - dw)?
// Which for fishable biomass calc?
// Which for yield calc? --> Avg, so (w - dw/2)
//   Census     Spawner fishery   Maturation        Growth     FGF / Mortality     Age/Year increment
//    1 Jan ---->   1 Jan    ------> 1 May -----> June-Aug ----> Year round  ----->   31 Dec
//    Quota                                                  yield = avg weight
std::vector<double> Fishery::update(double temp){
	stock_summary = StockSummary(); // reset stock summary for each year. FIXME: Maybe better to do outside 

	// 1. Stock assessment (census) happens here at the beginning of the year, based on which quotas are decided
	double ssb = pop.calcSSB(pop.par.recruitmentAge);
	double tsb = pop.calcTSB(pop.par.recruitmentAge);
	double maturity = pop.calcMaturity(pop.par.recruitmentAge);

	// 2. Calculate total quota (feeding + spawning grounds fishery)
	double quota = calc_quota(temp);
	double quota_fgf = quota * (1 - par.rho); // quota for the feeding grounds fishery
	double quota_spf = quota * par.rho; // quota for the spawning grounds fishery

	// 3. Reproduction and spawning grounds fishery 
	std::vector<double> spf_summary_before = spawner_fishery(quota_spf * par.f_spf_before);
	std::vector<Fish> recruits = pop.spawn(ssb, tsb, temp, stock_summary);
	std::vector<double> spf_summary_after = spawner_fishery(quota_spf * (1-par.f_spf_before));
	summarize_spawner_fishery_metrics(spf_summary_before, spf_summary_after, quota_spf);
	double yield_spf = spf_summary_before[2] + spf_summary_after[2];

	// 4. Maturation
	for (auto& f: pop.fishes) f.updateMaturity(temp);
	summarize_population_metrics(); // l~a, w~a, mat~a, n~a

	// 5. Growth
	for (auto& f: pop.fishes) f.grow(tsb/1e6, temp); // convert tsb to kT

	// 6. Feeding grounds fishery and natural mortality - should always come after growth to access weights before and after growth
	std::vector<double> harvest_out, fleet_efforts;
	double yield_fgf = 0, effort = 0;
	bool _use_average_weight = true;  // Use average weight (pre and post growth) for yield calcs and catch summary. 
	if (!fleets.empty()) {
		// Calculate initial chi for all fleets
		for (auto& fl: fleets) fl.init_chi(pop, -log(1-fl.par.quota*harvest_prop), temp); // initialize chi for the feeding grounds fishery
		
		// harvest stock (by all fleets)
		harvest_out = pop.get_fished(fleets, {quota_fgf}, temp, _use_average_weight, false);
		yield_fgf = std::accumulate(harvest_out.begin(), harvest_out.end(), 0.0, std::plus<double>()); // total yield from the feeding grounds fishery
	}
	summarize_catch_metrics(_use_average_weight); // nc~a, wc~a.  

	// 7. remove dead fish from population
	pop.fishes.erase(std::remove_if(pop.fishes.begin(), pop.fishes.end(), [](Fish &f){return !f.isAlive;}), pop.fishes.end());

	// 8. Increment age and advance to new year, and reset delta_weight
	for (auto& f: pop.fishes)  f.set_age(f.age+1);

	// 9. Finally, add recruits to population 
	pop.fishes.insert(pop.fishes.end(), recruits.begin(), recruits.end());
	
	// 10. Calculate socioeconomic metrics for analysis
	double yield = yield_fgf + yield_spf;
	double employment_sea = 0, employment_shore = 0;
	double profit_sea = 0, profit_shore = 0;
	for (int k=0; k<fleets.size(); ++k) fleet_efforts.push_back(0);

	std::vector<double> out = {
		ssb, 
		tsb,
		maturity,
		quota,
		quota_fgf,
		quota_spf,
		yield,
		yield_fgf,
		yield_spf,
		effort,
		stock_summary.nfish_ra,

		stock_summary.ssb0,
		stock_summary.ssb_spawning,
		stock_summary.ssb_spawning_ref,
		stock_summary.ssb_after_spawning,
		stock_summary.ssb_after_spawning_ref,
		stock_summary.ssbn,
		stock_summary.ssbn_ref
	};

	for (int k=0; k<fleets.size(); ++k){
		out.push_back(harvest_out[k]);
		out.push_back(fleet_efforts[k]);
	}

	return out;
}

double Fishery::get_fref(int fleet_id, double len){
	if (fleets.empty()) return 0;
	else return fleets[fleet_id].fishingMortalityRef(len);
}

Tensor<double> Fishery::scan(std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, double tsb0, int niters, bool re_init){
	Tensor<double> res({niters, static_cast<int>(colnames.size()), static_cast<int>(Tvec.size()), static_cast<int>(lminvec.size()), static_cast<int>(hvec.size()), nyears});
	Stock pop_ref = pop;

	if (fleets.empty()) throw std::runtime_error("No fleets present in Fishery");

	for (int iter = 0; iter < niters; ++iter){  // loop over iterations
	for (int it=0; it<Tvec.size(); ++it){       // loop over parameter 3 (temperature)
	for (int il=0; il<lminvec.size(); ++il){    // loop over control parameter 2 (lmin)
	for (int ih=0; ih<hvec.size(); ++ih){       // loop over control parameter 1 (h)
		pop = pop_ref;
		// if (hvec[ih] > 0.5) pop.set_superFishSize(1e0);
		
		set_harvestProp(hvec[ih]);
		set_minSizeLimit(lminvec[il]);
		
		double K_fishable = fleets[0].biomassFishable(no_fishing_pop, no_fishing_pop.par.recruitmentAge, false);
		double K_ssb      = no_fishing_pop.calcSSB(no_fishing_pop.par.recruitmentAge);
		std::cout << "h = " << hvec[ih] << ", L50 = " << fleets[0].par.lmin << ", T = " << Tvec[it] << ", n = " << pop.superfish_size << " | K_fishable = " << K_fishable << ", K_ssb = " << K_ssb << std::endl;

		if (re_init) pop.init(1000, 0, Tvec[it]);
	
		for (int t=0; t<nyears; ++t){
			double Tnow;
			// if (pop.par.update_env){
			// 	// cout << "t = " << t << "pop.current_year = " << pop.current_year;
			// 	pop.updateEnv(pop.current_year);
			// 	Tnow = pop.env.temperature;
			// 	// cout << " | env.t = " << pop.env.year << ", T = " << pop.env.temperature << "\n";
			// }
			// else{
				Tnow = Tvec[it];
			// }

			std::vector<double> state_now = update(Tnow);
			
			for (int col=0; col<state_now.size(); ++col){
				res({iter, col, it, il, ih, t}) = state_now[col];
			}
		}
	}
	}
	}
	}
	//res.print();
	
	return res.avg_dim(5);	// average over iterations

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

Rcpp::NumericVector tensor2array(Tensor<double>& v){
	Rcpp::NumericVector out(v.vec.begin(), v.vec.end()); 
	std::vector<int> dims = v.dim;
	std::reverse(dims.begin(), dims.end());
	out.attr("dim") = dims;
	return out;
}

Rcpp::NumericVector Fishery::simulate_multi_r(std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, double tsb0, int niters, bool re_init){
	Tensor<double> res = scan(Tvec, lminvec, hvec, nyears, tsb0, niters, re_init);
	return tensor2array(res);
}

#endif

