#include "population.h"
#include "pn_zero.h"

#include <cmath>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <string>
#include <cassert>
#include "stock.h"

using namespace std;

void StockParams::initFromFile(std::string params_file, bool verbose) {
	io::Initializer I;
	I.parse(params_file, false, verbose);

	#define READ_PAR(x) x = I.get<double>("stock", #x)

	READ_PAR(recruitmentAge);
	READ_PAR(sigmaf);

	#undef READ_PAR
}

void StockParams::print() {
	#define PRINT_PAR(x) std::cout << #x << " = " << x << "\n"

	PRINT_PAR(recruitmentAge);
	PRINT_PAR(sigmaf);

	#undef PRINT_PAR
}



Stock::Stock(Fish f) : proto_fish(f){
}

int Stock::readParams(std::string filename, bool verbose){
	par.initFromFile(filename, verbose);
	return 0;
}


// void Stock::set_traitVariances(vector<double> var){
// 	proto_fish.trait_variances = var;
// }
	
// FIXME: Add testthat
void Stock::init(int n, double t_init, double temp){
	fishes.clear();
	proto_fish.init(0, temp); // initialize prototype fish under 0 tsb conditions
	proto_fish.t_birth = t_init;
	fishes.resize(n, proto_fish);
}


// vector<double> Stock::noFishingEquilibriate(double temp){
// 	// backup params
// 	auto par_back = par;
	
// 	// disable fishing and env stochasticity
// 	set_harvestProp(0);
// 	par.sigmaf = 0;	// no env stochasticity while calculating carrying capacity
// 	par.simulate_bio_only = true;

// 	// start with 1000 age-1 superfish created under the specified temperature
// 	init(1000, temp); 

// 	// run 200 years of population dynamics
// 	int nsteps = 200;
// 	std::vector<double> state_t;
// 	for (int t=0; t<nsteps; ++t){
// 		state_t = this->update(temp);
// 	}

// 	// restore params
// 	par = par_back;
	
// 	return state_t;
// }


double Stock::calcSSB(double min_age){
	return 
	std::accumulate(fishes.begin(), fishes.end(), 0.0, 
		[min_age, this](double sum, const Fish& f) { 
			return sum + ((f.isAlive && f.isMature && f.age >= min_age) ? f.weight * superfish_size : 0); 
		}
	);
}


double Stock::calcTSB(double min_age){
	return 
	std::accumulate(fishes.begin(), fishes.end(), 0.0, 
		[min_age, this](double sum, const Fish& f) { 
			return sum + ((f.isAlive && f.age >= min_age) ? f.weight * superfish_size : 0); 
		}
	);
}


double Stock::calcMaturity(double min_age){
	return 
	std::accumulate(fishes.begin(), fishes.end(), 0.0, 
		[min_age, this](double sum, const Fish& f) { 
			return sum + ((f.isAlive && f.isMature && f.age >= min_age) ? 1 : 0); 
		}
	) / 
	std::accumulate(fishes.begin(), fishes.end(), 0.0, 
		[min_age, this](double sum, const Fish& f) { 
			return sum + ((f.isAlive && f.age >= min_age) ? 1 : 0); 
		}
	);
} 


double Stock::calcAbundanceAtAge(int age){
	return 
	std::accumulate(fishes.begin(), fishes.end(), 0.0, 
		[age, this](double sum, const Fish& f) { return sum + ((f.isAlive && f.age == age) ? superfish_size : 0); }
	);
}

int Stock::nfish(){
	return fishes.size();
}


double Stock::avgOverAges(const std::vector<double> &Qa, int amin, int amax, double missing_value){
	double Q_sum = 0;
	int nQ = 0;
	for (int a = amin; a <= amax; ++a){
		if (Qa[a] != missing_value){
			Q_sum += Qa[a];
			nQ += 1;
		}
	}
	if (nQ == 0) return 0;
	else return Q_sum/nQ;
}


inline double runif(double rmin=0, double rmax=1){
	double r = double(rand())/RAND_MAX; 
	return rmin + (rmax-rmin)*r;
}

inline double rnorm(double mu=0, double sd=1){
	double u = runif(1e-12, 1), v = runif();		// uniform rn's [0,1] for box-muller
	double x = sqrt(-2.0*log(u)) * cos(2*M_PI*v);
	return mu + sd*x;
}


vector<Fish> Stock::spawn(double ssb_now, double tsb_now, double temp, StockSummary &stock_summary) {
	// 3.b.1: Get the number of recruits for each fish 
	// -------------------------------------------------
	std::vector<double> nrecruits_vec(fishes.size(), 0.0);

	double nrecruits_total = 0;
	for (int k=0; k<fishes.size(); ++k) {
		auto &f = fishes[k];
		if (f.isAlive && f.isMature){  // fish survives the spawning-grounds fishery until actual spawning time
			// count this fish as spawner and calculate its potential recruits without density effect (for analysis only)
			stock_summary.nspawners += superfish_size; 
			stock_summary.nrecruits_potential += f.produceRecruits(  0, temp) * superfish_size;

			// Recruit production
			double nrecruits_fish = f.produceRecruits(ssb_now, temp) * superfish_size;
			nrecruits_vec[k]  = nrecruits_fish;
			nrecruits_total  += nrecruits_fish;

			// Mortality due to spawning
			double p_survival_spawning = exp(-f.par.Mspawning);
			f.isAlive = f.isAlive && (runif() <= p_survival_spawning);
		}
	}

	double nrecruits_before_noise = nrecruits_total;
	double noise_multiplier = exp(rnorm(-par.sigmaf*par.sigmaf/2, par.sigmaf));
	noise_multiplier = clamp(noise_multiplier, 1e-3, 10.0);
	nrecruits_total *= noise_multiplier;
	stock_summary.nrecruits_real = std::clamp(nrecruits_total, 1.0, par.rmax);
	//	for (auto& nn : nrecruits_vec) nn = nn*nrecruits_real/(nrecruits_total+1e-20); 

	// ** for analysis
	stock_summary.r0_avg = (ssb_now > 0)? (stock_summary.nrecruits_real * (1 + ssb_now/proto_fish.par.Bhalf) / ssb_now) : -999;
	stock_summary.factor_dr = stock_summary.nrecruits_real / (stock_summary.nrecruits_potential+1e-12);
	stock_summary.nrecruits_per_fish = stock_summary.nrecruits_real / stock_summary.nspawners;

	// 3.b.2: Generate recruits, inheriting traits from parents
	// -------------------------------------------------
	int n_super_recruits = std::ceil(stock_summary.nrecruits_real / superfish_size);

	vector<Fish> recruits;
	try{
		recruits.reserve(n_super_recruits);
	}
	catch(std::exception &e){
		std::cout << "Could not reserve recruits vec - required size: " << n_super_recruits << " / " << e.what() << std::endl;
		std::cout << "nrecruits_before_noise / nrecruits_total = " << nrecruits_before_noise << " / " << nrecruits_total << std::endl;
		throw e;
	}

	++proto_fish.t_birth;
	std::discrete_distribution<size_t> fitness_dist(nrecruits_vec.begin(), nrecruits_vec.end());

	for (int i=0; i<n_super_recruits; ++i){
		vector<double> mother_traits = fishes[fitness_dist(generator)].get_traits();
		vector<double> father_traits = fishes[fitness_dist(generator)].get_traits();
		vector<double> offspring_traits(mother_traits.size());
		for (int k=0; k<mother_traits.size(); ++k){
			offspring_traits[k] = (mother_traits[k] + father_traits[k])/2 + sqrt(proto_fish.trait_variances[k])*proto_fish.trait_scalars[k]*normal_dist(generator);
		}
		proto_fish.set_traits(offspring_traits);
		proto_fish.init(tsb_now/1e6, temp);
		recruits.push_back(proto_fish);
	}

	// FIXME: uncomment these
	// stock_summary.ssb_after_spawning = calcSSB(par.recruitmentAge);
	// stock_summary.ssb_after_spawning_ref = stock_summary.ssb0*exp(-proto_fish.par.Mspawning)*(1-par.f_spf_before*h_spf);

	return recruits;
}



vector<double> Stock::equilibriate_without_fishing(double temp){
	// start with 1000 age-1 superfish created under the specified temperature
	init(1000, 0, temp); 
	StockSummary stock_summary;

	std::vector<double> state_t;
	// run 200 years of population dynamics
	int nsteps = 200;
	for (int t=0; t<nsteps; ++t){
		stock_summary = StockSummary(); // reset stock summary for each year

		double ssb = calcSSB(par.recruitmentAge);
		double tsb = calcTSB(par.recruitmentAge);
		double maturity = calcMaturity(par.recruitmentAge);

		// 1. Maturation
		for (auto& f: fishes) f.updateMaturity(temp);

		// 2. Growth
		for (auto& f: fishes) f.grow(tsb/1e6, temp); // convert tsb to kT

		// 3. Reproduction
		vector<Fish> recruits = spawn(ssb, tsb, temp, stock_summary);

		// 4. Mortality
		for (auto& f: fishes) {
			double mortality_rate = f.naturalMortalityRate(temp);
			double survival_prob = exp(-mortality_rate*1.0);

			f.isAlive = f.isAlive && (runif() <= survival_prob);	// set the fish to die probabilistically, if not dead already.
		}

		// 6. remove dead fish from population
		fishes.erase(std::remove_if(fishes.begin(), fishes.end(), [](Fish &f){return !f.isAlive;}), fishes.end());

		// 7. Increment age and advance to new year
		for (auto& f: fishes)  f.set_age(f.age+1);

		// 8. Finally, add recruits to population 
		fishes.insert(fishes.end(), recruits.begin(), recruits.end());
		
		// 9. Calculate metrics for analysis
		state_t.insert(state_t.end(), {
			ssb, 
			tsb,
			maturity,
			stock_summary.nrecruits_real,
			stock_summary.factor_dr,
			static_cast<double>(fishes.size())
		});
	}

	return state_t;
}



// /// This function simulates the annual dynamics of a fish population, including maturation, growth, reproduction, 
// /// mortality (both natural and fishing-induced), and population metrics.
// ///
// /// Detailed Steps:
// /// 0. Update Fishing and Natural Mortality Rates:
// ///    - Calculate age-specific average fishing mortality rate (Fref_a), including spawning grounds fishery.
// ///    - Calculate age-specific average natural mortality rate (Mort_a), including spawning grounds fishery.
// ///    - Calculate age-specific average maturity (Mat_a).
// ///    - Average these quantities over ages 5-10 to get Fref_5_10, Mort_5_10, and Mat_5_10.
// ///    - Calculate chi, a factor that scales Fref_5_10 to give the actual fishing mortality rate, such that the overall fishing mortality rate adds up to Fc, the control parameter.
// ///      \f[
// ///      \chi = 
// ///      \begin{cases} 
// ///      0 & \text{if } F_\text{ref,5-10} = 0 \\
// ///      \frac{F_c \cdot (1 - \rho \cdot M_\text{5-10})}{F_\text{ref,5-10}} & \text{otherwise}
// ///      \end{cases}
// ///      \f]
// ///
// /// 1. Maturation:
// ///    - Update the maturity status of each fish in the population based on temperature effects.
// ///    - Calculate metrics for analysis: proportion of mature fish (maturity) and number of fish reaching recruitment age (nfish_ra).
// ///
// /// 2. Growth:
// ///    - Calculate the total spawning biomass (TSB) of the population.
// ///    - Update the length (growth) of each fish in the population based on TSB and temperature.
// ///    - Calculate metrics for analysis: average density-inhibition factor (factor_dg), maximum length (lmax), and 90th percentile length (length90).
// ///
// /// 3. Reproduction and Spawning Grounds Fishery:
// ///    - Calculate the initial spawning stock biomass (SSB) of the population.
// ///    - Implement pre-spawning mortality for mature fish exposed to spawning grounds fishery (SPF).
// ///    - Calculate metrics for analysis: actual and reference spawning stock biomass (ssb_spawning and ssb_spawning_ref).
// ///    - Calculate spawning mortality and recruitment of fish, adjusting for survival and potential recruits.
// ///    - Generate recruits (offspring) from spawning events, considering genetic traits and environmental variability.
// ///    - Implement post-spawning mortality for mature fish exposed to SPF.
// ///    - Calculate metrics for analysis: actual and reference SSB after spawning (ssb_after_spawning and ssb_after_spawning_ref).
// ///
// /// 4. Mortality:
// ///    - Calculate fishing effort requirements and actual efforts based on fishable biomass and mortality rates.
// ///    - Implement natural and fishing-induced mortality for all fish in the population.
// ///    - Calculate yield (harvested biomass) and mortality metrics for analysis.
// ///
// /// 5. Age Advancement:
// ///    - Increment the age of all surviving fish in the population.
// ///
// /// 6. Recruitment:
// ///    - Add newly recruited fish (offspring) to the population.
// ///
// /// 7. Calculate Employment and Profit:
// ///    - Calculate employment (sea and shore) and profit (sea and shore) based on yield, costs, and prices.
// ///
// /// 8. Print and Return:
// ///    - Print annual summary statistics and return a vector containing various population metrics and dynamics.
// ///    - Metrics include: SSB, yield, employment, profit, biological metrics (mortality rates, lengths, survival probabilities, etc.).
// ///
// /// @see Fish::updateMaturity, Fish::grow, Fish::produceRecruits, calcTSB, calcSSB, effort1, fishingMortalityRef
// ///
// /// @todo Confirm placement of mortality rate update and spawning stock calculations with domain experts.
// ///
// std::vector<double> Stock::update(double temp){
// 	for (auto& f : fishes) assert(f.isAlive);
// 	for (auto& f : fishes) assert(!f.isCaught);
// 	int nfish_start = fishes.size();

// 	// 0. Update F_5_10 and M_5_10 at start of the year
// 	// FIXME: Confirm the placement of this chunk with Mikko. Should this be done after decision to mature?
// 	// ~~~~~~~~~~~~
// 	// vector<double> Fref_a = fishingMortRefByAge(); // calculate age-specific average fishing mortality rate, including spawning grounds fishery
// 	// vector<double> Mort_a = naturalMortByAge(temp); // calculate age-specific average natural mortality rate, including spawning mortality
// 	// vector<double> Mat_a  = maturityByAge(); // calculate age-specific average natural mortality rate, including spawning mortality

// 	// // average the maturity, ref fishing, and natural mortality rates over ages 5-10
// 	// double Fref_5_10 = avgOverAges(Fref_a, 5, 10, std_missing_value);
// 	// double Mort_5_10 = avgOverAges(Mort_a, 5, 10, std_missing_value);
// 	// double Mat_5_10  = avgOverAges(Mat_a,  5, 10, std_missing_value);

// 	double Fref_ref = fishingMortRefFishable();
// 	double Mort_ref = naturalMortFishable(temp);
// 	double Mat_ref = maturityFishable();

// 	double chi = (Fref_ref == 0)? 0 : par.Fc*(1-par.rho*Mat_ref)/Fref_ref; // FIXME: this implies that chi will be very high for a young population just entering age 5, which is purely a population artefact!

// 	double F_spf = par.rho * par.Fc;
// 	double F_fgf = chi * Fref_ref;
// 	// ~~~~~~~~~~~~

// 	// 1. Maturation
// 	// update maturity 
// 	for (auto& f: fishes){
// 		f.updateMaturity(temp);
// 	}

// 	// ** ~~~~~~~ for analysis ~~~~~~~~
// 	double maturity = 0;
// 	for (auto& f : fishes) if (f.isAlive && f.isMature) {maturity += 1;}
// 	maturity /= fishes.size();

// 	double nfish_ra = 0;
// 	for (auto& f : fishes) if (f.isAlive && f.age == par.recruitmentAge) nfish_ra += superfish_size;
// 	// ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// 	// Calc by-age metrics
// 	pop_summary.n_a = aggregateByAge([this](const Fish &f){
// 			return (f.isAlive)? superfish_size : 0;
// 		});

// 	pop_summary.w_a = aggregateByAge([this](const Fish &f){
// 			return (f.isAlive)? superfish_size*f.weight : 0;
// 		});
// 	for (int i=0; i<pop_summary.w_a.size(); ++i) pop_summary.w_a[i] /= (pop_summary.n_a[i]+1e-20);

// 	pop_summary.mat_a = aggregateByAge([this](const Fish &f){
// 			return (f.isAlive && f.isMature)? superfish_size : 0;
// 		});
// 	for (int i=0; i<pop_summary.mat_a.size(); ++i) pop_summary.mat_a[i] /= (pop_summary.n_a[i]+1e-20);


// 	// 2. Growth
// 	double ssb = calcSSB(par.recruitmentAge);
// 	double tsb = calcTSB(par.recruitmentAge);
// 	for (auto& f: fishes){
// 		f.grow(tsb/1e6, temp); // convert tsb to kT
// 	}
	
// 	// ** ~~~~~~~ for analysis ~~~~~~~~
// 	// calculate metrics to analyse density-inhibition on growth
// 	double factor_dg = 0;
// 	for (auto& f: fishes){
// 		factor_dg += f.dl_real/(f.dl_potential+1e-12); 
// 	}
// 	factor_dg /= fishes.size();
	
// 	// calc 90%ile length
// 	double lmax = 0;
// 	for (auto &f : fishes) lmax = fmax(lmax, f.length);

// 	vector<Fish> ff = fishes;
// 	std::sort(ff.begin(), ff.end(), [](const Fish &f1, const Fish &f2){return f1.length > f2.length;});  // sort fishes descending by length
// 	for (int i=1; i<ff.size(); ++i) assert(ff[i].length <= ff[i-1].length);

// 	double length90 = 0;
// 	double cut = 0.05;
// 	for (int i=0; i < ceil(cut*ff.size()); ++i) length90 += ff[i].length;
// 	length90 /= ceil(cut*ff.size());
// 	//print_summary();
// 	// ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// 	// 3. Reproduction and Spawning grounds fishery
// 	// implement spawning for remaining fish
// 	// FIXME: Check carefully where SSB should include fish below recruitment age and where not...
// 	double ssb0 = ssb;
// 	if (verbose) cout << "ssb0 = " << ssb0 << endl;

// 	// 3a. pre-spawning part of the SPF
// 	double yield_spf = 0;
// 	double ssb_spawning = 0;
// 	double h_spf = 1-exp(-F_spf*1);
// 	double p_survival_spf_before = 1 - par.f_spf_before*h_spf;
// 	for (int k=0; k<fishes.size(); ++k) {
// 		auto &f = fishes[k];
// 		if (f.isAlive && f.isMature){ // only alive and mature fish are exposed to SPF
// 			f.isAlive = f.isAlive && (runif() <= p_survival_spf_before);

// 			if (f.isAlive) ssb_spawning += superfish_size * f.weight; // surviving individuals contribute to SSB
// 			if (!f.isAlive) yield_spf += superfish_size * f.weight;   // dying individuals contribute to yield
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
// 			nspawners += superfish_size; 

// 			// Recruitment
// 			double nrecruits_fish = f.produceRecruits(ssb_spawning, temp) * superfish_size;
// 			nrecruits_vec[k] = nrecruits_fish;
// 			nrecruits_total     += nrecruits_fish; // * (1/(1+ssb/f.par.Bhalf));
// 			nrecruits_potential += f.produceRecruits(  0, temp) * superfish_size;

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
// 	int nr = nrecruits_real/superfish_size;
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

// 			if (!f.isAlive) yield_spf += superfish_size * f.weight;
// 			if (!f.isAlive) f.isCaught = true;               // mark fish as caught

// 		}
// 	}

// 	double ssbn = calcSSB(par.recruitmentAge);

// 	double yield_spf_ref = ssb0*h_spf*(par.f_spf_before + (1-par.f_spf_before)*exp(-proto_fish.par.Mspawning));
// 	double ssbn_ref = ssb0*(1-h_spf)*exp(-proto_fish.par.Mspawning);

// 	// 4. Mortality 
	
// 	// 4.1. Effort Dynamics
// 	double F_req = par.Fc; // total control fishing mortality rate should be used in effort dynamics
// 	double F_real = F_req;
// 	double E_req = 0, E_real = 0;
// 	double D_sea_req = 0, D_sea_real = 0;
// 	double Nrel = 0;

// 	if (!par.simulate_bio_only){
// 		if (par.h > 0){
// 			Nrel = (K_fishableBiomass > 0)? fishableBiomass() / K_fishableBiomass : 1e-20;
			
// 			// Effort covers spawning and feeding grounds, so 
// 			//   we use the total control fishing mortality rate and the total natural mortality rate
// 			E_req = (Nrel < 1e-20)? 0 : effort1(Nrel, F_real, Mort_ref); 
// 			//pow(Nrel, 1-par.b) * F * (exp(-(F+M)*(1-par.b))-1) / (par.q*(F+M)*(par.b-1));
// 			D_sea_req  = par.dsea * E_req;
// 			D_sea_real = D_sea_req / (1 + D_sea_req/par.dmax);
			
// 			E_real = D_sea_real / par.dsea;
// 			// Solve for F_real - ignore for now
// 			// F_real = pn::zero(0, F_req, [E_real, Nrel, temp, this](double F){ return (E_real - effort(Nrel, F, temp));}, 1e-6).root;		
// 			F_real = F_req;
// 		}
// 	}

// 	// implement natural+fishing mortality in feeding grounds over the rest of the year, and calculate yield
// 	double tsb_before_mort = calcTSB(par.recruitmentAge);
// 	double yield = 0, to_sea_bed = 0;
// 	double survival_mean = 0, n_survival_mean = 0;
// 	for (auto& f : fishes){
// 		if (f.isAlive){
// 			double fishing_mort_rate = chi*fishingMortalityRef(f.length); //*(F_real/(F_req+1e-20)); // the factor F_real/F_req is needed if effort limitation is used
// 			double natural_mort_rate = f.naturalMortalityRate(temp); // This does not (should not) include spawning-related mortality
// 			double mortality_rate = natural_mort_rate + fishing_mort_rate; // post-spawning mortality rate is same for mature and immature individuals
// 			double survival_prob = exp(-mortality_rate*1.0);	// mortality in feeding grounds (post-spawning), over full year.
// 			survival_mean += survival_prob;
// 			n_survival_mean += 1;

// 			f.isAlive = f.isAlive && ((rand() / double(RAND_MAX)) <= survival_prob);	// set the fish to die probabilistically, if not dead already.
			
// 			if (!f.isAlive){
// 				f.isCaught = runif() < fishing_mort_rate/mortality_rate; // check if fish is caught or goes to sea bed!
				
// 				if (f.isCaught) yield += superfish_size*f.weight; // if caught, add to yield
// 				else to_sea_bed += superfish_size*f.weight;       // else, goes to sea bed
// 			}
// 		}
// 	} 
// 	survival_mean /= n_survival_mean;

// 	double tsb_after_mort = calcTSB(par.recruitmentAge);

// 	// Calc by-age metrics in catch
// 	pop_summary.nc_a = aggregateByAge([this](const Fish &f){
// 			return (!f.isAlive && f.isCaught)? superfish_size : 0;
// 		});

// 	pop_summary.wc_a = aggregateByAge([this](const Fish &f){
// 			return (!f.isAlive && f.isCaught)? superfish_size*f.weight : 0;
// 		});
// 	for (int i=0; i<pop_summary.wc_a.size(); ++i) pop_summary.wc_a[i] /= (pop_summary.nc_a[i]+1e-20);


// 	// remove dead fish from population
// 	fishes.erase(std::remove_if(fishes.begin(), fishes.end(), [](Fish &f){return !f.isAlive;}), fishes.end());
// 	int nfish_after_mort = fishes.size();

// 	// 5. Increment age and advance to new year
// 	for (auto& f: fishes){
// 		f.set_age(f.age+1);
// 	}

// 	// 6. Finally, add recruits to population 
// 	fishes.insert(fishes.end(), recruits.begin(), recruits.end());

// 	// calculate employment
// 	double emp_sea = D_sea_req;
// 	double emp_shore = par.dshr * yield;

// 	// calculate profit for the year
// 	double profit_sea = 0, profit_shr = 0;
// 	if (!par.simulate_bio_only){
// 	//if (par.h > 0){
// 		profit_sea = yield*par.price_sea*(1-par.fee_ratio) - par.scale_catch*(D_sea_req*par.salary_sea + E_req*par.variable_costs_sea + par.fixed_costs_sea);
// 		profit_shr = yield*(par.price_shore - par.price_sea) - yield*par.dshr * par.salary_shore - par.scale_catch*par.fixed_costs_shore;
// 	//}
// 	}

// 	if (verbose) cout << "year = " << current_year 
// 					  << " | TSB(MT) = " << tsb/1e9 << ", SSB(MT) = " << ssb/1.0e9 
// 					  << ", recruits = " << nrecruits_real << "/" << std::accumulate(nrecruits_vec.begin(), nrecruits_vec.end(), 0.0) 
// 					  << ", maturity = " << maturity 
// 					  << ", survival = " << nfish_after_mort << "/" << nfish_start
// 					  << ", survival_prob = " << survival_mean << " --> " << double(nfish_after_mort)/nfish_start
// 					  << ", N_rel = " << Nrel 
// 					  << ", F_real = " << F_real << "(" << F_real/(F_req+1e-20)*100 
// 					  << "%), r0_avg = " << r0_avg 
// 					  << ", % harvest = " << yield/tsb 
// 					  << ", dg/dr = " << factor_dg << "/" << factor_dr
// 					  << ", yield = " << yield 
// 					  << "\n";
					  
// 	++current_year;
// 	return {ssb, yield, emp_sea+emp_shore, profit_sea+profit_shr, emp_sea, emp_shore, profit_sea, profit_shr, tsb, r0_avg, nrecruits_real, nfish_ra, static_cast<double>(nfish()), factor_dg, factor_dr, lmax, length90, survival_mean, maturity, Nrel, 
// 			ssb_spawning, ssb_spawning_ref, ssb_after_spawning, ssb_after_spawning_ref, ssbn, ssbn_ref, yield_spf, yield_spf_ref,
// 			tsb_before_mort, tsb_after_mort, to_sea_bed, 
// 			chi, Fref_ref, Mort_ref, Mat_ref, F_spf
// 			};	
// }



// // **
// // ** Utilities **
// // **
// void Stock::summarize(){
// 	// make histogram
// 	const int n = proto_fish.par.amax+3;
// 	vage.clear(); vage.resize(n, 0);
// 	vlen = vmat = vfreq = vage;	
// 	for (auto& f: fishes){
// 		vage[f.age] += f.age;
// 		vlen[f.age] += f.length;
// 		vmat[f.age] += f.isMature;
// 		vfreq[f.age] += 1;
// 	}
// 	for (int i=0; i<n; ++i){
// 		vage[i] /= vfreq[i]+1e-12;
// 		vlen[i] /= vfreq[i]+1e-12;
// 		vmat[i] /= vfreq[i]+1e-12;
// 	}

// }


// void Stock::print_summary(){
// 	cout << "------ overall summary ----\n";
// 	cout << "Current year = " << current_year << '\n';
// 	cout << "K_fishable   = " << K_fishableBiomass << '\n';
// 	cout << "K_ssb        = " << K_ssb << '\n';
// 	cout << "nfish        = " << fishes.size() << '\n';
// 	cout << "SSB          = " << calcSSB(par.recruitmentAge) << '\n';
// 	cout << "TSB          = " << calcTSB(par.recruitmentAge) << '\n';
// 	cout << "B_fishable   = " << fishableBiomass() << '\n';

// 	summarize();
// 	cout << "----classwise summary ----\n";
// 	int n = vage.size();
// 	cout << "age: "; for (int i=0; i<n; ++i) cout << vage[i] << "\t"; cout << "\n";
// 	cout << "len: "; for (int i=0; i<n; ++i) cout << vlen[i] << "\t"; cout << "\n";
// 	cout << "mat: "; for (int i=0; i<n; ++i) cout << vmat[i] << "\t"; cout << "\n";
// 	cout << "num: "; for (int i=0; i<n; ++i) cout << vfreq[i] << "\t"; cout << "\n";
// 	cout << "--------------------------\n";
// }


#ifndef NATIVE_CPP

Rcpp::DataFrame Stock::get_state(){
	vector<double> col(fishes.size());
	Rcpp::DataFrame df = Rcpp::DataFrame::create();
	
	vector<double> x;
	x.clear(); x.reserve(fishes.size());
	for (auto &f : fishes){
		x.push_back(f.t_birth);	
	}
	df.push_back(x, "t.birth");
	
	//vector<double> x;
	x.clear(); x.reserve(fishes.size());
	for (auto &f : fishes){
		x.push_back(f.age);	
	}
	df.push_back(x, "age");

	vector<bool> bx;
	bx.clear(); bx.reserve(fishes.size());
	for (auto &f : fishes){
		bx.push_back(f.isMature);	
	}
	df.push_back(bx, "isMature");

	//vector<bool> bx;
	bx.clear(); bx.reserve(fishes.size());
	for (auto &f : fishes){
		bx.push_back(f.isAlive);	
	}
	df.push_back(bx, "isAlive");

	//vector<double> x;
	x.clear(); x.reserve(fishes.size());
	for (auto &f : fishes){
		x.push_back(f.length);	
	}
	df.push_back(x, "length");

	//vector<double> x;
	x.clear(); x.reserve(fishes.size());
	for (auto &f : fishes){
		x.push_back(f.weight);	
	}
	df.push_back(x, "weight");

	// add flag from parameters for debug
	//vector<double> x;
	x.clear(); x.reserve(fishes.size());
	for (auto &f : fishes){
		x.push_back(f.par.flag);	
	}
	df.push_back(x, "flag");
	return df;	
}


Rcpp::DataFrame Stock::get_traits(){
	vector<double> traits = proto_fish.get_traits();
	vector<vector<double>> all_traits(traits.size());
	for (auto& f: fishes){
		traits = f.get_traits();
		for (int i=0; i<traits.size(); ++i){
			all_traits[i].push_back(traits[i]);
		}
	}

	Rcpp::DataFrame df = Rcpp::DataFrame::create();
	for (int i=0; i<proto_fish.trait_names.size(); ++i){
		df.push_back(all_traits[i], proto_fish.trait_names[i]);
	}
	// df.push_back(nrecruits_vec, "nrecruits");
	return df;
}

#endif

