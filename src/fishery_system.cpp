#include <fishery_system.h>
#include <fstream>
#include "random_utils.h"

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

void Fishery::update_referenceFishingMortalityCurve_AllFleets(){
	// Update the effective fleet and all other fleets based on new parameters
	set_referenceFishingMortalityCurve(fleet_effective);
	for (auto& fl : fleets) {
		set_referenceFishingMortalityCurve(fl);
	}
	for (auto& fl : spawner_fleets) {
		set_referenceFishingMortalityCurve(fl);
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

	// update all fleets based on new par object
	update_referenceFishingMortalityCurve_AllFleets();
	
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

	// update all fleets with modified par object
	update_referenceFishingMortalityCurve_AllFleets();

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
	// int count = 0;
	// std::cout << "Start catch: min size limit = " << par.lmin << '\n';
	for (const auto& f: pop.fishes) {  // const auto& ensures we do not modify the fish 
		if (!f.isAlive) continue; // Skip already dead fishes

		double natural_mort_rate = f.naturalMortalityRate(temp);
		double fishing_mort_rate = fl.fishingMortality(f.length);
		double mortality_rate = natural_mort_rate + fishing_mort_rate;

		double survival_prob = exp(-mortality_rate*1.0);
		double catch_prob = fishing_mort_rate/mortality_rate;

		bool f_isAlive = f.isAlive && (runif() <= survival_prob);	// set the fish to die probabilistically, if not dead already.

		if (!f_isAlive){
			if (f.length >= par.lmin){
				expected_catch += catch_prob * pop.superfish_size * f.weight; // fraction catch_prob of this superfish goes to yield
			}
		}
		
		// ++count;
		// std::cout << "Sr. / age / length / mu / F / f_isAlive / catch_prob / expt_catch: " << count << " / " << f.age << " / " << f.length << " / " << natural_mort_rate << " / " << fishing_mort_rate << " / " << f_isAlive << " / " << catch_prob << " / " << expected_catch << '\n';
	}

	if (pop.fishes.size() > 500 && Fc > 0 && expected_catch < 1e-6){
		std::cout << "Quota is 0 - probably spurious" << std::endl;
		std::cout << "  - nFish = " << pop.fishes.size() << std::endl;
		std::cout << "  - chi = " << fl.chi << std::endl;
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
	colnames.push_back("employment_sea_"+std::to_string(fleets.size()));
	colnames.push_back("employment_shore_"+std::to_string(fleets.size()));
	colnames.push_back("profit_sea_"+std::to_string(fleets.size()));
	colnames.push_back("profit_shore_"+std::to_string(fleets.size()));
}

void Fishery::addSpawnerFleet(std::string params_file, bool verbose){
	spawner_fleets.emplace_back();
	spawner_fleets.back().readParams(params_file, verbose);
	set_referenceFishingMortalityCurve(spawner_fleets.back());
}

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
	
	return {ssb_before, ssb_remaining, yield_spf, h_spf};
}

// Which weight should be used for quota calc? Before growth, so (w - dw)?
// Which for fishable biomass calc?
// Which for yield calc? --> Avg, so (w - dw/2)
//   Census     Spawner fishery   Maturation        Growth     FGF / Mortality     Age/Year increment
//    1 Jan ---->   1 Jan    ------> 1 May -----> June-Aug ----> Year round  ----->   31 Dec
//    Quota                                                  yield = avg weight
std::vector<double> Fishery::update(double temp, double rec_noise_multiplier, double K){
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
	std::vector<Fish> recruits = pop.spawn(ssb, tsb, temp, rec_noise_multiplier, stock_summary);
	std::vector<double> spf_summary_after = spawner_fishery(quota_spf * (1-par.f_spf_before));
	summarize_spawner_fishery_metrics(spf_summary_before, spf_summary_after, quota_spf);
	double yield_spf = spf_summary_before[2] + spf_summary_after[2];
	double h_spf_eff = spf_summary_before[3] + spf_summary_after[3];

	// 4. Maturation
	for (auto& f: pop.fishes) f.updateMaturity(temp);
	summarize_population_metrics(); // l~a, w~a, mat~a, n~a

	// 5. Growth
	for (auto& f: pop.fishes) f.grow(tsb/1e6, temp); // convert tsb to kT

	// 6. Feeding grounds fishery and natural mortality - should always come after growth to access weights before and after growth
	std::vector<double> fleet_harvests;
	bool _use_average_weight = true;  // Use average weight (pre and post growth) for yield calcs and catch summary. 
	if (!fleets.empty()) {
		// Calculate initial chi for all fleets
		for (auto& fl: fleets) fl.init_chi(pop, -log(1-fl.par.quota*harvest_prop), temp); // initialize chi for the feeding grounds fishery
		
		// harvest stock (by all fleets)
		fleet_harvests = pop.get_fished(fleets, {quota_fgf}, temp, _use_average_weight, false);
	}
	summarize_catch_metrics(_use_average_weight); // nc~a, wc~a.  

	// 7. remove dead fish from population
	pop.fishes.erase(std::remove_if(pop.fishes.begin(), pop.fishes.end(), [](Fish &f){return !f.isAlive;}), pop.fishes.end());

	// 8. Increment age and advance to new year, and reset delta_weight
	for (auto& f: pop.fishes)  f.set_age(f.age+1);

	// 9. Finally, add recruits to population 
	pop.fishes.insert(pop.fishes.end(), recruits.begin(), recruits.end());
	
	// 10a. Calculate socioeconomic metrics of feeding grounds fleets
	std::vector<FleetUtils> fleet_utils_fgf;
	for (int k=0; k<fleets.size(); ++k){
		fleet_utils_fgf.push_back(fleets[k].calc_socioeconomics(fleet_harvests[k], K, -9e99, false));
	}

	// 10b. Calculate socioeconomic metrics of spawner fleets
	if (spawner_fleets.size() == 0) throw std::runtime_error("No spawner fishery has been added");
	std::vector<FleetUtils> fleet_utils_spf;
	for (int k=0; k<spawner_fleets.size(); ++k){
		double fleet_k_yield = spawner_fleets[k].par.quota * yield_spf;
		double fleet_k_hspf  = spawner_fleets[k].par.quota * h_spf_eff;
		fleet_utils_spf.push_back(spawner_fleets[k].calc_socioeconomics(fleet_k_yield, -9e99, fleet_k_hspf, true));
	}

	// 11. Calculate total utils
	FleetUtils spf_total_utils = std::accumulate(fleet_utils_spf.begin(), fleet_utils_spf.end(), FleetUtils(), std::plus<FleetUtils>());
	FleetUtils fgf_total_utils = std::accumulate(fleet_utils_fgf.begin(), fleet_utils_fgf.end(), FleetUtils(), std::plus<FleetUtils>());
	FleetUtils total_utils = spf_total_utils+fgf_total_utils;

	std::vector<double> out = {
		ssb, 
		total_utils.yield,
		total_utils.employment_sea + total_utils.employment_shore,
		total_utils.profit_sea + total_utils.profit_shore,
		total_utils.effort,

		tsb,
		maturity,
		quota,
		stock_summary.nfish_ra,

		quota_fgf,
		fgf_total_utils.yield,
		fgf_total_utils.effort,
		fgf_total_utils.employment_sea,
		fgf_total_utils.employment_shore,
		fgf_total_utils.profit_sea,
		fgf_total_utils.profit_shore,

		quota_spf,
		spf_total_utils.yield,
		spf_total_utils.effort,
		spf_total_utils.employment_sea,
		spf_total_utils.employment_shore,
		spf_total_utils.profit_sea,
		spf_total_utils.profit_shore,

		stock_summary.ssb0,
		stock_summary.ssb_spawning,
		stock_summary.ssb_spawning_ref,
		stock_summary.ssb_after_spawning,
		stock_summary.ssb_after_spawning_ref,
		stock_summary.ssbn,
		stock_summary.ssbn_ref
	};

	for (int k=0; k<fleets.size(); ++k){
		out.push_back(fleet_utils_fgf[k].yield);
		out.push_back(fleet_utils_fgf[k].effort);
		out.push_back(fleet_utils_fgf[k].employment_sea);
		out.push_back(fleet_utils_fgf[k].employment_shore);
		out.push_back(fleet_utils_fgf[k].profit_sea);
		out.push_back(fleet_utils_fgf[k].profit_shore);
	}

	return out;
}

double Fishery::get_fref(int fleet_id, double len){
	if (fleets.empty()) return 0;
	else return fleets[fleet_id].fishingMortalityRef(len);
}

Tensor<double> Fishery::scan(std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, std::vector<double> rec_noise_t, double tsb0, int niters, bool re_init){
	Tensor<double> res({niters, static_cast<int>(colnames.size()), static_cast<int>(Tvec.size()), static_cast<int>(lminvec.size()), static_cast<int>(hvec.size()), nyears});
	Stock pop_ref = pop;

	if (fleets.empty()) throw std::runtime_error("No fleets present in Fishery");
	if (rec_noise_t.size() < nyears) throw std::runtime_error("noise vector has "+std::to_string(rec_noise_t.size())+" values, at least "+std::to_string(nyears)+" expected");

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

			std::vector<double> state_now = update(Tnow, rec_noise_t[t], K_fishable);
			
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


// std::vector<double> Fishery::max_avg_utils(std::vector<int> dims, std::vector<double> data){
// 	Tensor<double> res(dims);
// 	res.vec = data;		// res is {u, T, c2, c1, t}

// 	profit_mask = res.slice(4, 3, 3).avg_dim(0); // Dim 4 is u, index 3 is profit --> {Profit, T, c2, c1, t}. average over t --> {Profit, T, c2, c1}
// 	profit_mask.mask([](double x){return x > 0;});   // {profit_mask, T, c2, c1}
// 	profit_mask.print();

// 	// profit_mask.repeat_outer(dims[4]);  // Repeat mask over all utils

// 	Tensor<double> res2 = res.avg_dim(0).max_dim(0).max_dim(0).max_dim(0);	// avg over t, then max over c1, then max over c2, then max over T
// 	res2.print();

// 	res.transform(4, std::divides<double>(), res2.vec); // divide u dimension by res2

// 	return res.avg_dim(0).vec; // average over t
// }


// std::vector<double> Fishery::stakeholder_satisfaction(std::vector<int> dims, std::vector<double> data){
// 	Tensor<double> res(dims);
// 	res.vec = data;		// res is {u, T, c2, c1, t}

// 	Tensor<double> res2 = res.avg_dim(0).max_dim(0).max_dim(0).max_dim(0);	// avg over t, then max over c1, then max over c2, then max over T
// 	res.transform(4, std::divides<double>(), res2.vec); // divide u dimension by res2

// 	Tensor<double> sp({5,4});	// spvec is {s, u}
// 	//        ssb yield emp  profit  
// 	sp.vec = {0.0, 0.3, 0.0, 0.7,	// industrial
// 			  0.3, 0.5, 0.1, 0.1,	// artisanal
// 			  0.3, 0.2, 0.5, 0.0,	// employment-maximizing policymakers
// 			  0.2, 0.2, 0.0, 0.6,	// profit-maximizing policymakers
// 			  0.5, 0.1, 0.2, 0.2	// conservationists
// 			 };

// 	sp.print();

// 	Tensor<double> Ssucy = sp.repeat_inner(res.dim[1]).repeat_inner(res.dim[2]).repeat_inner(res.dim[3]).repeat_inner(res.dim[4]) * res.repeat_outer(sp.dim[0]);
// 	//                        ^ {s, u, T}             ^ {s, u, T, c2}           ^ {s, u, T, c2, c1}      ^ {s, u, T, c2, c1, y}         ^ {s, u, T, c2, c1, y}

// 	Tensor<double> Sscy = Ssucy.accumulate(0.0, 4, std::plus<double>());	// aggregate along u dim to get {s, T, c2, c1, y}

// 	Tensor<double> Ssc = Sscy.avg_dim(0);
// 	//                        ^ {s, T, c2, c1}
// 	Ssc.transform(3, std::divides<double>(), Ssc.max_dim(0).max_dim(0).max_dim(0).vec);
// 	//			  ^ s                            ^ {s,T,c2} ^ {s,T}    ^ {s}
	
// 	return Ssc.vec;

// }


// std::vector<double> Fishery::stakeholder_satisfaction_t(std::vector<int> dims, std::vector<double> data){
// 	Tensor<double> res(dims);
// 	res.vec = data;		// res is {u, T, c2, c1, t}

// 	// here t is also treated as a control parameter
// 	Tensor<double> res2 = res.max_dim(0).max_dim(0).max_dim(0).max_dim(0);	// max over t, then max over c1, then max over c2, then max over T
// 	res.transform(4, std::divides<double>(), res2.vec); // divide u dimension by u_max vector

// 	Tensor<double> sp({5,4});	// spvec is {s, u}
// 	//        ssb yield emp  profit  
// 	sp.vec = {0.0, 0.3, 0.0, 0.7,	// industrial
// 			  0.3, 0.5, 0.1, 0.1,	// artisanal
// 			  0.3, 0.2, 0.5, 0.0,	// employment-maximizing policymakers
// 			  0.2, 0.2, 0.0, 0.6,	// profit-maximizing policymakers
// 			  0.5, 0.1, 0.2, 0.2	// conservationists
// 			 };

// 	sp.print();

// 	// stakeholder preferences {s,u} repeated to get same dim as res, then multiplied with utilities to get  {s,u}*u
// 	Tensor<double> Ssucy = sp.repeat_inner(res.dim[1]).repeat_inner(res.dim[2]).repeat_inner(res.dim[3]).repeat_inner(res.dim[4]) * res.repeat_outer(sp.dim[0]);
// 	//                        ^ {s, u, T}             ^ {s, u, T, c2}           ^ {s, u, T, c2, c1}      ^ {s, u, T, c2, c1, y}         ^ {s, u, T, c2, c1, y}

// 	// aggregate along u dim to get {s, T, c2, c1, y}
// 	Tensor<double> Sscy = Ssucy.accumulate(0.0, 4, std::plus<double>());	

// 	// no time average, since we need time-explicit JSS
// 	//Tensor<double> Ssc = Sscy.avg_dim(0);
// 	//                        ^ {s, T, c2, c1}
	
// 	Sscy.transform(4, std::divides<double>(), Sscy.max_dim(0).max_dim(0).max_dim(0).max_dim(0).vec);
// 	//			  ^ s                            ^ {s,T,c2,c1} ^ {s,T,c2}  ^ {s,T}   ^ {s}
	
// 	return Sscy.vec;

// }




/*
Questions:
1. Should lmin a fleet-specific parameter?
2. Thus, is fishable biomass of the population only valid from the perspective of a given fleet?
3. Which all socio-economic parameters should move to Fleet?

4. Summary variables are calculated in certain order. What happens when we make life sequence a parameter?
*/

// ************ R stuff *****************
#ifndef NATIVE_CPP

Rcpp::DataFrame Fishery::simulate_r(double lf, double h, int nyears, double tsb0, std::vector<double> temp_t, std::vector<double> rec_noise_t, bool re_init, std::string output_file)
{
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

	if (temp_t.size() < nyears) throw std::runtime_error("temperature vector has "+std::to_string(temp_t.size())+" values, at least "+std::to_string(nyears)+" expected");
	if (rec_noise_t.size() < nyears) throw std::runtime_error("noise vector has "+std::to_string(rec_noise_t.size())+" values, at least "+std::to_string(nyears)+" expected");

	set_harvestProp(h);
	set_minSizeLimit(lf);
	
	double K_fishable = fleets[0].biomassFishable(no_fishing_pop, no_fishing_pop.par.recruitmentAge, false);
	// std::cout << "h/lf = " << h << " / " << lf << " | K = " << K << std::endl;

	if (re_init) pop.init(1000, 0, temp_t[0]);

	std::vector<std::vector<double>> columns(colnames.size());
	for (auto& vec : columns) vec.reserve(nyears);
	
	Rcpp::DataFrame df = Rcpp::DataFrame::create();

	for (int i=0; i<nyears; ++i){
		std::vector<double> state_now = update(temp_t[i], rec_noise_t[i], K_fishable);
		
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

Rcpp::NumericVector Fishery::simulate_multi_r(std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, std::vector<double> rec_noise_t, double tsb0, int niters, bool re_init){
	Tensor<double> res = scan(Tvec, lminvec, hvec, nyears, rec_noise_t, tsb0, niters, re_init);
	return tensor2array(res);
}

// Rcpp::NumericVector Fishery::get_profit_mask(){
//     return tensor2array(profit_mask);
// }


#endif

