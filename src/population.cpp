#include "population.h"
#include "pn_zero.h"

#include <cmath>
#include <algorithm>
#include <iostream>

using namespace std;

Population::Population(Fish f){
	proto_fish = f;
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


//void Population::calc_athresh(){
//	// set a_thresh
//	Fish f = proto_fish;
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


void Population::init(int n){
	current_year = 1;
	fishes.clear();
	fishes.resize(n, proto_fish);
}


vector<double> Population::noFishingEquilibriate(){
	// backup params
	auto par_back = par;
	set_harvestProp(0);
	par.sigmaf = 0;	// no env stochasticity while calculating carrying capacity

	init(1000);
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
	for (auto& f : fishes) if (f.isAlive && f.isMature) ssb += par.n * f.weight;
	return ssb;
}


int Population::nfish(){
	return fishes.size();
}

double Population::selectivity(double len){
	return 1/(1+exp(-par.sf*(len-par.lf50))); 
}


double Population::calcRealizedFishingMortality(){
	
}


double Population::fishableBiomass(){
	double B_fishable = 0;
	for (auto& f : fishes) B_fishable += par.n * f.weight * selectivity(f.length);
	return B_fishable;
}


double Population::effort(double Nr, double F){
//	double M = proto_fish.par.mam[proto_fish.par.amax];
	double sum_wimi = 0, sum_wi = 0;
	for (auto& f : fishes){
		if (f.age <= f.par.amax){
			sum_wimi += f.weight * selectivity(f.length) * f.naturalMortalityRate();
			sum_wi   += f.weight * selectivity(f.length);
		}
	} 
	double M = sum_wimi / sum_wi;  // Mass-weighted average mortality of fishable population
	//cout << ": F/M = " << F << " / " << M << "\n";
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


std::vector<double> Population::update(){
	// 1. Maturation
	// update maturity 
	for (auto& f: fishes){
		f.updateMaturity();
	}

	double tsb = 0;
	for (auto& f : fishes) if (f.isAlive) tsb += par.n * f.weight;
	// 2. Growth
	for (auto& f: fishes){
		f.grow(tsb);
	}
	//print_summary();

	// 3. Reproduction 
	// implement spawning for remaining fish
	double ssb = calcSSB();
//	double nrecruits = par.r0*ssb / (1 + ssb/par.Bhalf); // * exp(rnorm(-par.sigmaf*par.sigmaf/2, par.sigmaf));
	double nrecruits = 0;
	for (auto &f: fishes) {
		// nrecruits += par.r0*n*f.weight/(1+ssb/par.Bhalf);
		if (f.isAlive && f.isMature){
			nrecruits += f.produceRecruits() * par.n * (1/(1+ssb/par.Bhalf));
		}
	}
	//nrecruits *= exp(rnorm(-par.sigmaf*par.sigmaf/2, par.sigmaf));
	nrecruits = std::min(nrecruits, par.rmax);
	double r0_avg = nrecruits * (1 + ssb/par.Bhalf) / ssb;

	// 4. Mortality 
//	summarize();
	
//	// calculate realized mortality rate
	double F_req = par.mort_fishing_mature; //, M = proto_fish.par.mam[proto_fish.par.amax];
	double F_real = F_req; 
	double E_req, E_real;
	double D_sea_req, D_sea_real;
	double Nrel = 0;
	if (par.h > 0){
//		double Nrel = 0; 
//		for (int i=par.a_thresh; i<vfreq.size(); ++i) Nrel += vfreq[i] / (carrying_capacity[i]+1e-20);	// FIXME: This has potential to blow up Nrel
//		Nrel /= (proto_fish.par.amax - par.a_thresh + 1);
		Nrel = (K_fishableBiomass > 0)? fishableBiomass() / K_fishableBiomass : 1e-10;
			
		E_req = effort(Nrel, F_req); //pow(Nrel, 1-par.b) * F * (exp(-(F+M)*(1-par.b))-1) / (par.q*(F+M)*(par.b-1));
		D_sea_req  = par.dsea * E_req;
		D_sea_real = D_sea_req / (1 + D_sea_req/par.dmax);
		
		E_real = D_sea_real / par.dsea;
		// Solve for F_real
		F_real = pn::zero(0, F_req, [E_real, Nrel, this](double F){ return (E_real - effort(Nrel, F));}, 1e-6).root;
	}

	// implement mortality over the year and calculate yield
	double yield = 0;
	for (auto& f : fishes){
		double fishing_mort_rate = selectivity(f.length)*F_real; //(f.isMature)? par.mort_fishing_mature : par.mort_fishing_immature;
		double mortality_rate = f.naturalMortalityRate() + fishing_mort_rate; // post-spawning mortality rate is same for mature and immature individuals
		double survival_prob = exp(-mortality_rate*1.0);	// mortality during post-spawining, over full year.
		
		f.isAlive = f.isAlive && ((rand() / double(RAND_MAX)) <= survival_prob);	// set the fish to die probabilistically, if not dead already.
		
		if (!f.isAlive) yield += fishing_mort_rate/mortality_rate * par.n*f.weight;
	} 

	// remove dead fish from population
	fishes.erase(std::remove_if(fishes.begin(), fishes.end(), [](Fish &f){return !f.isAlive;}), fishes.end());


	// 5. Increment age and advance to new year
	for (auto& f: fishes){
		f.set_age(f.age+1);
	}
	
	// add recruits at age 1
	int nr = nrecruits/par.n;
	if (nr == 0) nr = 1;
	fishes.resize(fishes.size()+nr, proto_fish);

	// calculate employment
	double emp_sea = D_sea_req;
	double emp_shore = par.dshr * yield;

	// calculate profit for the year
	double profit_sea = 0, profit_shr = 0;
	//if (par.h > 0){
		profit_sea = yield*par.price_sea - par.scale_catch*(D_sea_req*par.salary_sea + E_req*par.variable_costs_sea + par.fixed_costs_sea);
		profit_shr = yield*(par.price_shore - par.price_sea) - yield*par.dshr * par.salary_shore - par.scale_catch*par.fixed_costs_shore;
	//}

	if (verbose) cout << "year = " << current_year << " | TSB = " << tsb/1e9 << ", SSB = " << ssb/1e9 << ", recruits = " << nrecruits/1e9 << ", N_rel = " << Nrel << ", F_real = " << F_real << "(" << F_real/F_req*100 << "%), r0_avg = " << r0_avg << "\n";
	++current_year;
	return {ssb, yield, emp_sea, emp_shore, profit_sea, profit_shr, tsb};	
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

