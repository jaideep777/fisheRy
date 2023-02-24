#include "population.h"
#include "pn_zero.h"

#include <cmath>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <string>
#include <cassert>

using namespace std;

int Population::readEnvironmentFile(std::string filename){
	
	ifstream fin(filename.c_str());
	if (!fin) throw std::runtime_error("Could not open file: " + filename);
	
	// skip header
	std::string line;
	getline(fin, line);
	
	// read CO2 file
	while (fin.peek() != EOF){
		std::getline(fin, line);
	
		std::stringstream lineStream(line);

		std::string cell;
		
		std::getline(lineStream, cell, ',');
		int year = stoi(cell);

		SeaEnvironment env;

		std::getline(lineStream, cell, ',');
		env.temperature = stod(cell);
		
		std::getline(lineStream, cell, ',');
		env.recruitment_noise_multiplier = stod(cell);
		
		t_env.push_back(year);
		v_env.push_back(env);
	}
	
	//for (int i=0; i<t_env.size(); ++i) cout << "env: " << t_env[i] << " " << v_env[i].temperature << "\n";
		
	return 0;

}

void Population::updateEnv(double t){
	if (par.update_env){
		int id = (t - *t_env.begin());
		int D = t_env.size(); // NEVER USE UNSIGNED INTs in modulo operations. Hence store in int.
		id = id % D;
		if (id < 0) id += D;
	
		//cout << "update: t = " << t << " " << id << " " << t_env[id] << " " << v_env[id].temperature << " (" << *t_env.begin() << ", " << t_env.size() << ")\n";	
		env = v_env[id];
	}
}



Population::Population(Fish f) : proto_fish(f){
//	proto_fish = f;
//	calc_athresh();
}

void Population::set_superFishSize(double _n){
	par.n = _n;
}

void Population::set_harvestProp(double _h){
	par.h = _h;
	par.mort_fishing_mature = -log(1-_h);
	par.mort_fishing_immature = -log(1-_h);
}


//// USED IN OLD FORMULATION ONLY
//void Population::calc_athresh(double tsb0, double temp){
//	// set a_thresh
//	Fish f = proto_fish;
//	assert(f.par.growth_model == Model::Dankel22); // this calculation is only valid for old growth model
//	par.a_thresh = 99999;
//	for (int i=1; i <= f.par.amax; ++i){
//		f.set_age(i);
//		if (selectivity(f.length) > 0.5){
//			par.a_thresh = i;
//			break;
//		}
//	}
//	cout << "a_0.5 = " << par.a_thresh << "\n";
//}


void Population::set_minSizeLimit(double _lf50){
	par.lf50 = _lf50;
//	calc_athresh();
}

void Population::set_traitVariances(vector<double> var){
	proto_fish.trait_variances = var;
}

void Population::init(int n, double tsb, double temp){
	current_year = 1;
	fishes.clear();
	proto_fish.init(tsb, temp);
	proto_fish.t_birth = current_year;
	fishes.resize(n, proto_fish);
//	if (par.use_old_model_effort) calc_athresh(tsb, temp);
}


vector<double> Population::noFishingEquilibriate(double tsb0, double temp){
	// backup params
	auto par_back = par;
	set_harvestProp(0);
	par.sigmaf = 0;	// no env stochasticity while calculating carrying capacity

	init(1000, tsb0, temp);
	int nsteps = 200;

	std::vector<double> state_t;
	for (int t=0; t<nsteps; ++t){
		state_t = this->update();
	}

	// restore params
	par = par_back;
	
	return state_t;
}


double Population::calcSSB(){
	double ssb = 0;
	for (auto& f : fishes) if (f.isAlive && f.isMature &&  f.age >= par.recruitmentAge) ssb += par.n * f.weight;
	return ssb;
}

double Population::calcTSB(){
	double tsb = 0;
	for (auto& f : fishes) if (f.isAlive && f.age >= par.recruitmentAge) tsb += par.n * f.weight;
	return tsb;
}

vector<double> Population::calcSB(){
	double tsb = 0, ssb = 0;
	for (auto& f : fishes){
		if (f.isAlive && f.age >= f.age >= par.recruitmentAge){
			tsb += par.n * f.weight;
			if (f.isMature) ssb += par.n * f.weight;
		} 
	} 
	return {ssb, tsb};
}


int Population::nfish(){
	return fishes.size();
}

double Population::selectivity(double len){
	return 1/(1+exp(-par.sf*(len-par.lf50))); 
}


//double Population::calcRealizedFishingMortality(){
//	
//}


double Population::fishableBiomass(){
	double B_fishable = 0;
	for (auto& f : fishes) if (f.age > 1) B_fishable += par.n * f.weight * selectivity(f.length);
	return B_fishable;
}


double Population::effort(double Nr, double F, double temp){
//	double M = proto_fish.par.mam[proto_fish.par.amax];
	double sum_wimi = 0, sum_wi = 0;
	for (auto& f : fishes){
		if (f.age <= f.par.amax){
			sum_wimi += f.weight * selectivity(f.length) * f.naturalMortalityRate(temp);
			sum_wi   += f.weight * selectivity(f.length);
		}
	} 
	double M = sum_wimi / sum_wi;  // Mass-weighted average mortality of fishable population
	//cout << ": Nrel/F/M = " << Nr << " / " << F << " / " << M << "\n";
	return pow(Nr, 1-par.b) * F * (exp(-(F+M)*(1-par.b))-1) / (par.q*(F+M)*(par.b-1)); 
}


inline double runif(double rmin=0, double rmax=1){
	double r = double(rand())/RAND_MAX; 
	return rmin + (rmax-rmin)*r;
}

inline double rnorm(double mu=0, double sd=1){
	double u = runif(), v = runif();		// uniform rn's [0,1] for box-muller
	double x = sqrt(-2.0*log(u)) * cos(2*M_PI*v);
	return mu + sd*x;
}


std::vector<double> Population::update(double temp){
	for (auto& f : fishes) assert(f.isAlive);
	
	// 1. Maturation
	// update maturity 
	for (auto& f: fishes){
		f.updateMaturity(temp);
	}

	// ** for analysis
	double maturity = 0, nspawners = 0;
	for (auto& f : fishes) if (f.isAlive && f.isMature) {maturity += 1; nspawners += par.n;}
	maturity /= fishes.size();
	double nfish_ra = 0;
	for (auto& f : fishes) if (f.isAlive && f.age == par.recruitmentAge) nfish_ra += par.n;
	// **

	// 2. Growth
	double tsb = calcTSB();
	for (auto& f: fishes){
		f.grow(tsb/1e6, temp); // convert tsb to kT
	}
	
	// ** for analysis
	// calculate metrics to analyse density-inhibition on growth
	double factor_dg = 0;
	for (auto& f: fishes){
		factor_dg += f.dl_real/(f.dl_potential+1e-12); 
	}
	factor_dg /= fishes.size();
	
	// calc 90%ile length
	double lmax = 0;
	for (auto &f : fishes) lmax = fmax(lmax, f.length);

	vector<Fish> ff = fishes;
	std::sort(ff.begin(), ff.end(), [](const Fish &f1, const Fish &f2){return f1.length > f2.length;});  // sort fishes descending by length
	for (int i=1; i<ff.size(); ++i) assert(ff[i].length <= ff[i-1].length);

	double length90 = 0;
	double cut = 0.05;
	for (int i=0; i < ceil(cut*ff.size()); ++i) length90 += ff[i].length;
	length90 /= ceil(cut*ff.size());
	//print_summary();
	// **

	// 3. Reproduction 
	// implement spawning for remaining fish
	double ssb = calcSSB();
//	double nrecruits = par.r0*ssb / (1 + ssb/par.Bhalf); // * exp(rnorm(-par.sigmaf*par.sigmaf/2, par.sigmaf));
	nrecruits_vec.resize(fishes.size());
	std::fill(nrecruits_vec.begin(), nrecruits_vec.end(), 0.0);
	double nrecruits_total = 0;
	double nrecruits_potential = 0;
	for (int k=0; k<fishes.size(); ++k) {
		auto &f = fishes[k];
		// nrecruits += par.r0*n*f.weight/(1+ssb/par.Bhalf);
		if (f.isAlive && f.isMature){
			double nrecruits_fish = f.produceRecruits(ssb, temp) * par.n;
			nrecruits_vec[k] = nrecruits_fish;
			nrecruits_total     += nrecruits_fish; // * (1/(1+ssb/f.par.Bhalf));
			nrecruits_potential += f.produceRecruits(  0, temp) * par.n;
		}
	}
	//nrecruits *= exp(rnorm(-par.sigmaf*par.sigmaf/2, par.sigmaf));
	double nrecruits_real = std::min(nrecruits_total, par.rmax);
//	for (auto& nn : nrecruits_vec) nn = nn*nrecruits_real/(nrecruits_total+1e-20); 

	// ** for analysis
	double r0_avg = nrecruits_real * (1 + ssb/proto_fish.par.Bhalf) / ssb;
	double factor_dr = nrecruits_real / (nrecruits_potential+1e-12);
	double nrecruits_per_fish = nrecruits_real/nspawners;
	// **

	// Generate recruits (in a separate vector)
	// cout << "nrecruits_vec: ";
	// for (auto nn : nrecruits_vec) cout << nn << " ";
	// cout << "\n";
	std::discrete_distribution<size_t> fitness_dist(nrecruits_vec.begin(), nrecruits_vec.end());
	int nr = nrecruits_real/par.n;
	if (nr <= 0) nr = 1;
	++proto_fish.t_birth;

	vector<Fish> recruits;
	recruits.reserve(nr);
	for (int i=0; i<nr; ++i){
		vector<double> mother_traits = fishes[fitness_dist(generator)].get_traits();
		vector<double> father_traits = fishes[fitness_dist(generator)].get_traits();
		vector<double> offspring_traits(mother_traits.size());
		for (int k=0; k<mother_traits.size(); ++k){
			offspring_traits[k] = (mother_traits[k] + father_traits[k])/2 + sqrt(proto_fish.trait_variances[k])*proto_fish.trait_scalars[k]*normal_dist(generator);
		}
		proto_fish.set_traits(offspring_traits);
		proto_fish.init(tsb/1e6, temp);
		recruits.push_back(proto_fish);
	}


	// 4. Mortality 
//	if (par.use_old_model_effort) summarize(); // population summary for calculation of Nrel
	
	// Calculate mortality rate in spawning-grounds (h1) and open sea (h2)
	double h1, h2;
	double B = fishableBiomass();
	h1 = par.f_harvest_spg * par.h * B / (ssb+1);
	h1 = fmin(fmax(h1, 0), 0.4);
	h2 = (par.h * B - h1 * ssb) / B;
	h2 = fmin(fmax(h2, 0), 1);
	if (h2 == 0) h1 = fmin(par.h * B / (ssb+1), 1);
	if (verbose) cout << "h1/h2 = " << h1 << " / " << h2 << endl;

//	// calculate realized mortality rate
	// double F_req = par.mort_fishing_mature; //, M = proto_fish.par.mam[proto_fish.par.amax];
	// double F_real = F_req;
	double F_req_sea = -log(1-h2);
	double F_req_spg = -log(1-h1);

	double F_real_sea = F_req_sea;
	double F_real_spg = F_req_spg;

	double E_req_sea = 0, E_real_sea = 0;
	double E_req_spg = 0, E_real_spg = 0;
	double E_req = 0, E_real = 0;

	double D_sea_req = 0, D_sea_real = 0;

	double Nrel_sea = 0, Nrel_spg = 0;

	if (!par.simulate_bio_only){
		if (par.h > 0){
			// Ignore effort limitation in the following calcs
			Nrel_sea = (K_fishableBiomass > 0)? B / K_fishableBiomass : 1e-20;
			E_req_sea = (Nrel_sea < 1e-10)? 0 : effort(Nrel_sea, F_req_sea, temp); 

			Nrel_spg = (K_ssb > 0)? ssb / K_fishableBiomass : 1e-20;
			E_req_spg = (Nrel_spg < 1e-10)? 0 : effort(Nrel_spg, F_req_spg, temp); 

			E_req = E_req_sea + E_req_spg;
			E_real = E_req;
			
			D_sea_req  = par.dsea * E_req;
			
			// Nrel = (K_fishableBiomass > 0)? fishableBiomass() / K_fishableBiomass : 1e-20;
			
			// E_req = (Nrel < 1e-10)? 0 : effort(Nrel, F_req, temp); //pow(Nrel, 1-par.b) * F * (exp(-(F+M)*(1-par.b))-1) / (par.q*(F+M)*(par.b-1));
			// D_sea_req  = par.dsea * E_req;
			// D_sea_real = D_sea_req / (1 + D_sea_req/par.dmax);
			
			// E_real = D_sea_real / par.dsea;
			// // Solve for F_real
			// F_real = pn::zero(0, F_req, [E_real, Nrel, temp, this](double F){ return (E_real - effort(Nrel, F, temp));}, 1e-6).root;
		}
	}

	// implement mortality over the year and calculate yield
	double yield = 0;
	double survival_mean = 0;
	for (auto& f : fishes){
		double fishing_mort_rate = selectivity(f.length)*F_real_sea + double(f.isMature)*F_real_spg; //(f.isMature)? par.mort_fishing_mature : par.mort_fishing_immature;
		double mortality_rate = f.naturalMortalityRate(temp) + fishing_mort_rate; // post-spawning mortality rate is same for mature and immature individuals
		double survival_prob = exp(-mortality_rate*1.0);	// mortality during post-spawining, over full year.
		survival_mean += survival_prob;

		f.isAlive = f.isAlive && ((rand() / double(RAND_MAX)) <= survival_prob);	// set the fish to die probabilistically, if not dead already.
		
		if (!f.isAlive && f.age >= par.recruitmentAge) yield += fishing_mort_rate/mortality_rate * par.n*f.weight;
	} 
	survival_mean /= fishes.size();
	
	// remove dead fish from population
	fishes.erase(std::remove_if(fishes.begin(), fishes.end(), [](Fish &f){return !f.isAlive;}), fishes.end());


	// 5. Increment age and advance to new year
	for (auto& f: fishes){
		f.set_age(f.age+1);
	}

	// 6. Finally, add recruits to population 
	fishes.insert(fishes.end(), recruits.begin(), recruits.end());

	// calculate employment
	double emp_sea = D_sea_req;
	double emp_shore = par.dshr * yield;

	// calculate profit for the year
	double profit_sea = 0, profit_shr = 0;
	if (!par.simulate_bio_only){
	//if (par.h > 0){
		profit_sea = yield*par.price_sea - par.scale_catch*(D_sea_req*par.salary_sea + E_req*par.variable_costs_sea + par.fixed_costs_sea);
		profit_shr = yield*(par.price_shore - par.price_sea) - yield*par.dshr * par.salary_shore - par.scale_catch*par.fixed_costs_shore;
	//}
	}

	if (verbose) cout << "year = " << current_year << " | TSB = " << tsb/1e9 << ", SSB = " << ssb/1.0e9 << ", recruits = " << nrecruits_real << "/" << std::accumulate(nrecruits_vec.begin(), nrecruits_vec.end(), 0.0) << ", N_rel_sea/spg = " << Nrel_sea << " / " << Nrel_spg << ", F_real = " << F_real_sea << "(" << F_real_sea/F_req_sea*100 << "%), r0_avg = " << r0_avg << ", % harvest = " << yield/tsb << "\n";
	++current_year;
	return {ssb, yield, emp_sea+emp_shore, profit_sea+profit_shr, emp_sea, emp_shore, profit_sea, profit_shr, tsb, r0_avg, nrecruits_real, nfish_ra, static_cast<double>(nfish()), factor_dg, factor_dr, lmax, length90, survival_mean, maturity, Nrel_sea, Nrel_spg};	
}



// **
// ** Utilities **
// **
void Population::summarize(){
	// make histogram
	const int n = proto_fish.par.amax+3;
	vage.clear(); vage.resize(n, 0);
	vlen = vmat = vfreq = vage;	
	for (auto& f: fishes){
		vage[f.age] += f.age;
		vlen[f.age] += f.length;
		vmat[f.age] += f.isMature;
		vfreq[f.age] += 1;
	}
	for (int i=0; i<n; ++i){
		vage[i] /= vfreq[i]+1e-12;
		vlen[i] /= vfreq[i]+1e-12;
		vmat[i] /= vfreq[i]+1e-12;
	}

}


void Population::print_summary(){
	summarize();
	int n = vage.size();
	cout << "---------------\n";
	cout << "K = " << K_fishableBiomass << "\n";
	cout << "age: "; for (int i=0; i<n; ++i) cout << vage[i] << "\t"; cout << "\n";
	cout << "len: "; for (int i=0; i<n; ++i) cout << vlen[i] << "\t"; cout << "\n";
	cout << "mat: "; for (int i=0; i<n; ++i) cout << vmat[i] << "\t"; cout << "\n";
	cout << "num: "; for (int i=0; i<n; ++i) cout << vfreq[i] << "\t"; cout << "\n";
	cout << "---------------\n";
}


#ifndef NATIVE_CPP

Rcpp::DataFrame Population::get_state(){
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


Rcpp::DataFrame Population::get_traits(){
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