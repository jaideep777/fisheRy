#include "fleet.h"
#include <cmath>
#include <algorithm>
#include <stdexcept>
#include <cassert>
#include "stock.h"

inline double runif(double rmin=0, double rmax=1){
	double r = double(rand())/RAND_MAX; 
	return rmin + (rmax-rmin)*r;
}

struct linregresult{
	double slope = 0;
	double intercept = 0;
};

inline linregresult linreg(const std::vector<double> &x, const std::vector<double> &y){
	double xMean = std::accumulate(x.begin(), x.end(), 0.0) / x.size();
	double yMean = std::accumulate(y.begin(), y.end(), 0.0) / y.size();

	// Calculate the numerator and denominator for the slope (m) using transform and accumulate
	double numerator = std::inner_product(
		x.begin(), x.end(), y.begin(), 0.0,
		std::plus<>(),
		[xMean, yMean](double xi, double yi) { return (xi - xMean) * (yi - yMean); }
	);

	double denominator = std::accumulate(
		x.begin(), x.end(), 0.0,
		[xMean](double acc, double xi) { return acc + (xi - xMean) * (xi - xMean); }
	);

	linregresult res;
	res.slope = numerator / denominator;
	res.intercept = yMean - res.slope * xMean;
	
	return res;
}

inline linregresult linreg0(const std::vector<double>& x, const std::vector<double>& y, bool debug = false) {
	// Calculate the numerator and denominator for the slope (m)
	double numerator = std::inner_product(x.begin(), x.end(), y.begin(), 0.0);
	double denominator = std::accumulate(
		x.begin(), x.end(), 0.0,
		[](double acc, double xi) { return acc + (xi * xi); }
	);

	linregresult res;
	res.slope = numerator / denominator;
	res.intercept = 0;
	
	if (debug){
		std::cout << "linreg0: \n"; 
		std::cout << "  x = "; for (auto xx : x) std::cout << xx << " "; std::cout << '\n';
		std::cout << "  y = "; for (auto yy : y) std::cout << yy << " "; std::cout << '\n';
		std::cout << "  res: slope/int = " << res.slope << " / " << res.intercept << '\n';
	}

	return res;
}

inline double linreg_predict(double x_new, const linregresult& res){
	return res.intercept + res.slope * x_new;
}

inline double linreg_predict_inverse(double y_new, const linregresult& res){
	return (y_new - res.intercept)/res.slope;
}


void FleetParams::initFromFile(std::string params_file, bool verbose){
	io::Initializer I;
	I.parse(params_file, false, verbose);

	#define READ_PAR(x) x = I.get<double>("fleet", #x)

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

	// effort dynamics and employment
	READ_PAR(q);
	READ_PAR(dsea);
	READ_PAR(dmax);
	READ_PAR(dshr);
	READ_PAR(b);

	// revenue and profit 
	READ_PAR(price_sea);
	READ_PAR(price_shore);
	READ_PAR(fee_ratio);

	READ_PAR(salary_sea);
	READ_PAR(salary_shore);
	READ_PAR(fixed_costs_sea);
	READ_PAR(fixed_costs_shore);
	READ_PAR(variable_costs_sea);
	READ_PAR(scale_catch);

	// Fractional quota assigned to this fleet
	READ_PAR(quota);

	READ_PAR(max_chi);
	READ_PAR(chi0_scalar_slope);
	READ_PAR(window_dt);
	control_model = I.get<std::string>("fleet", "control_model");

	#undef READ_PAR

}

void FleetParams::print(){
	#define PRINT_PAR(x) std::cout << #x << " = " << x << "\n"

	// status quo lmin
	PRINT_PAR(lmin);
	PRINT_PAR(lmin_sq);

	// management / fishing selectivity
	PRINT_PAR(F1);
	PRINT_PAR(F2);
	PRINT_PAR(F3);
	PRINT_PAR(F4);
	PRINT_PAR(F5);
	PRINT_PAR(F6);

	PRINT_PAR(F3_sq);
	PRINT_PAR(F5_sq);

	// effort dynamics and employment
	PRINT_PAR(q);
	PRINT_PAR(dsea);
	PRINT_PAR(dmax);
	PRINT_PAR(dshr);
	PRINT_PAR(b);

	// revenue and profit 
	PRINT_PAR(price_sea);
	PRINT_PAR(price_shore);
	PRINT_PAR(fee_ratio);

	PRINT_PAR(salary_sea);
	PRINT_PAR(salary_shore);
	PRINT_PAR(fixed_costs_sea);
	PRINT_PAR(fixed_costs_shore);
	PRINT_PAR(variable_costs_sea);
	PRINT_PAR(scale_catch);

	PRINT_PAR(quota);

	PRINT_PAR(max_chi);
	PRINT_PAR(control_model);
	PRINT_PAR(chi0_scalar_slope);
	PRINT_PAR(window_dt);

	#undef PRINT_PAR
}

std::random_device rd;

Fleet::Fleet() : g(rd()){
}

void Fleet::readParams(std::string params_file, bool verbose){
	par.initFromFile(params_file, verbose);
}


// void Fleet::set_harvestProportion(double _h){
// 	h = _h;
// 	Fc = -log(1-_h);
// }

void Fleet::set_minSizeLimit(double _lf50){
	double dl = _lf50 - par.lmin_sq;

	par.lmin = par.lmin_sq + dl;
	par.F3   = par.F3_sq + dl;
	par.F5   = par.F5_sq + dl;
}

/// Dry run simply takes population by value, so that original one is not altered
std::vector<double> Fleet::harvest_dry_run(Stock pop, double quota, double temp){
	return harvest(pop, quota, temp, true); // harvest a copy population and return progress
}


/// Formula:
/// \f[
/// F_\text{ref} = \frac{F_1}{1 + \exp(-F_2 \cdot (l - F_3))} - \frac{F_6}{1 + \exp(-F_4 \cdot (l - F_5))}
/// \f]
double Fleet::fishingMortalityRef(double len){
	// return par.F1/(1+exp(-par.F2*(len-par.F3))); 
	return 
		  par.F1/(1+exp(-par.F2*(len-par.F3))) 
		- par.F6/(1+exp(-par.F4*(len-par.F5)));
}

double Fleet::fishingMortality(double len){
	double scalar = (len < par.lmin)? fmin(1, chi) : chi;
	return scalar * fishingMortalityRef(len);
}


double Fleet::FishingMortalityRef_avgl(double lmax = 200, int n = 100){
	double Fref_avgl = 0;
	for (int i = 0; i < n; ++i){
		double l = par.lmin + (lmax - par.lmin) * double(i)/(n-1);
		Fref_avgl += fishingMortalityRef(l);
	}
	Fref_avgl /= n;
	return Fref_avgl;
}



// bool Fleet::isFishable(const Fish &f){
// 	return f.length >= par.lmin;
// }


double Fleet::fishability(double length){
	// Above minimum size limit, fishability = 1; below min size limit, fishability = probability of death
	return (length > par.lmin)? 1.0 : 1-exp(-fishingMortalityRef(length)); 
}


std::vector<double> Fleet::cummulativeFishingMortalityRef(const Stock &stock, double min_age){
	double wF_below_lmin = 0, wF_above_lmin = 0, w_sum = 0;
	for (auto& f : stock.fishes){
		if (f.age < min_age || !f.isAlive) continue; // skip fish below min age or dead

		double w = fishability(f.length);
		w_sum += w;
		if (f.length < par.lmin) wF_below_lmin += w * fishingMortalityRef(f.length);
		else wF_above_lmin += w * fishingMortalityRef(f.length);
	}
	return {wF_below_lmin, wF_above_lmin, w_sum, (wF_below_lmin+wF_above_lmin)/(w_sum+1e-20)};
	//      ^ sum(wF)      ^ sum(wF)      ^ sum(w)   ^ average F weighted by w
}


double Fleet::biomassFishable(const Stock &stock, double min_age){
	return 
	std::accumulate(stock.fishes.begin(), stock.fishes.end(), 0.0, 
		[min_age, this, &stock](double sum, const Fish& f) { 
			if (f.age < min_age || !f.isAlive) return sum + 0;
			else return sum + (fishability(f.length) * f.weight * stock.superfish_size);
		}
	);
}


void Fleet::init_chi(Stock &pop, double F_fgf, double temp){
	std::vector<double> w_Fref = cummulativeFishingMortalityRef(pop, 0);

	double Fref_below_lmin = w_Fref[0];
	double Fref_above_lmin = w_Fref[1];
	double wsum = w_Fref[2];
	double Fref_avg = w_Fref[3];

	if (Fref_avg > F_fgf) chi = F_fgf/Fref_avg; // case when chi < 1
	else chi = (wsum*F_fgf - Fref_below_lmin)/Fref_above_lmin;

	double h = 1-exp(-F_fgf);
	chi *= exp(par.chi0_scalar_slope*(h-0.5));
}


void Fleet::update_chi(const std::vector<double>& chi_in_windows, 
					   const std::vector<double>& yield_in_windows, 
					   const std::vector<double>& bs_in_windows,
					   double yield_remainder, double bs_remainder){
	// 3a. calibrate yield model (y = Bs * f(X))
	linregresult res;
	std::vector<double> y(yield_in_windows.size());
	if (par.control_model == "exp"){
		// exponential model: y = Bs (1-e^-kX) --> -log(1-y/Bs) = kX
		std::transform(yield_in_windows.begin(), yield_in_windows.end(),
						bs_in_windows.begin(), y.begin(),
						[](double yield, double bs) {
							if (bs == 0) return 0.0;		
							if (yield >= bs) return 25.0;
							return -log(1 - (yield / bs));
						});

		res = linreg0(chi_in_windows, y, debug);
	}
	else if (par.control_model == "linear"){
		// linear model: y = Bs (k X) --> y/Bs = kX
		std::transform(yield_in_windows.begin(), yield_in_windows.end(),
						bs_in_windows.begin(), y.begin(),
						[](double yield, double bs) {
							return (bs == 0)? 0 : yield / bs;
						});

		res = linreg0(chi_in_windows, y, debug);
	}
	else {
		throw std::runtime_error("Unsopported control model");
	}

	// 3b. predict new chi
	if (par.control_model == "exp"){
		// exponential model: y = Bs (1-e^-kX)
		// std::cout << "using exp model" << std::endl;
		if (yield_remainder >= bs_remainder) chi = 25;
		else chi = linreg_predict_inverse(-log(1 - (yield_remainder/bs_remainder)), res);
	}
	else if (par.control_model == "linear"){
		// linear model: y = Bs k X
		// std::cout << "using linear model" << std::endl;
		chi = linreg_predict_inverse((yield_remainder/bs_remainder), res);
	}

	// if (std::isinf(chi) || std::isnan(chi) || chi > 1e20) throw std::runtime_error("Regressed chi is Inf or NA or extremely large");

	chi = std::clamp(chi, 1e-6, par.max_chi);

}


/// Note: this function takes pop by reference so it IS altered
/// Some computations are doubled in the function below, but that's ok for now as it serves to
/// cross-check those calcs. These can be removed after sufficient testing
std::vector<double> Fleet::harvest(Stock& pop, double quota, double temp, bool return_progress){
	double yield = 0, to_sea_bed = 0;
	double survival_mean = 0, n_survival_mean = 0;
	window_props_vec.clear(); // clear old data in windows 

	// count number of alive fish to calculate per-window samples
	double n_alive = 0;
	for (auto& f : pop.fishes) n_alive += f.isAlive? 1:0;
	int window_n = std::ceil(par.window_dt*n_alive);
	
	// shuffle fish so that all windows are statistically similar
	shuffle(pop.fishes.begin(), pop.fishes.end(), g);

	double B = biomassFishable(pop, 0);
	// double quota = B*h; // Should this be fishable biomass at start of season or after SPF?
	double B_sampled = 0;
	double yield_expected;

	// double Bsampled_debug = 0, n_fishable_debug = 0, n_debug = 0; 
	// for (auto& f : pop.fishes){
	// 	if (f.isAlive){
	// 		double fishability_f = fishability(f.length);

	// 		Bsampled_debug += fishability_f * f.weight * pop.superfish_size;
	// 		n_fishable_debug += fishability_f;
	// 		n_debug += 1;
	// 	}
	// }
	// std::cout << "Check consistency: B_sampled = " << Bsampled_debug << ", B = " << B << ", TSB = " << pop.calcTSB(0) << ", fishability = " << n_fishable_debug/n_debug << '\n';
	// return {0};

	std::vector<double> progress;
	std::vector<double> chi_in_windows(1, 0), yield_in_windows(1, 0), bs_in_windows(1, 0);
	double yield_prev = 0, bs_prev = 0;
	int windows_sampled = 0;
	WindowProps window_props;
	int count = 0;
	for (auto& f : pop.fishes){
		if (f.isAlive){
			double fishability_f = fishability(f.length);

			B_sampled += fishability_f * f.weight * pop.superfish_size;
			if (B_sampled > B) throw std::runtime_error("Sampled fishable biomass exceeds total fishable biomass");

			yield_expected = (B_sampled/B) * quota;

			double fishing_mort_rate = fishingMortality(f.length); 
			double natural_mort_rate = f.naturalMortalityRate(temp); // This does not (should not) include spawning-related mortality
			double mortality_rate = natural_mort_rate + fishing_mort_rate; // post-spawning mortality rate is same for mature and immature individuals
			double survival_prob = exp(-mortality_rate*1.0);	// mortality in feeding grounds (post-spawning), over full year. Note that survival prob must be annualized because this fish will be iterated over only once
			survival_mean += survival_prob;
			n_survival_mean += 1;

			window_props.M_fishable += fishability_f * natural_mort_rate;
			window_props.F_fishable += fishability_f * fishing_mort_rate;
			window_props.B_sampled  += fishability_f * f.weight*pop.superfish_size;
			window_props.n_fishable += fishability_f * 1;

			f.isAlive = f.isAlive && (runif() <= survival_prob);	// set the fish to die probabilistically, if not dead already.
			
			if (!f.isAlive){
				f.isCaught = runif() < fishing_mort_rate/mortality_rate; // check if fish is caught or goes to sea bed!
				
				if (f.isCaught){
					yield += pop.superfish_size*f.weight; // if caught, add to yield
					window_props.yield += pop.superfish_size*f.weight;
				}
				else to_sea_bed += pop.superfish_size*f.weight;       // else, goes to sea bed
			}

			++count;

			if (count >= window_n){ // update chi, but not if most fish have already been sampled
				count = 0;
				++windows_sampled;

				// get parameters and outcomes realized during this window
				double chi_window = chi;
				double yield_window = yield - yield_prev;
				double bs_window = B_sampled - bs_prev;

				window_props.chi = chi;
				window_props.B_start = B - yield - to_sea_bed;
				window_props.C_rate = window_props.yield/par.window_dt; // catch rate = annualized yield = yield per year
				window_props.M_fishable /= window_props.n_fishable; 
				window_props.F_fishable /= window_props.n_fishable; 

				if (debug){
					std::cout << "Yield window: " << yield_window << " " << window_props.yield << '\n';
					std::cout << "Bs window: " << bs_window << " " << window_props.B_sampled << std::endl;
				}

				if (fabs(1-yield_window/(window_props.yield+1e-20)) > 1e-5 && fabs(yield_window-window_props.yield) > 1e-5) 
					throw std::runtime_error("Yield in window_props does not match yield_window");
				if (fabs(1-bs_window/(window_props.B_sampled+1e-20)) > 1e-5 && fabs(bs_window-window_props.B_sampled) > 1e-5) 
					throw std::runtime_error("Yield or B_sampled in window_props does not match yield_window or bs_window");
				
				// push them into history
				window_props_vec.push_back(window_props);
				yield_in_windows.push_back(window_props.yield);
				chi_in_windows.push_back(window_props.chi);
				bs_in_windows.push_back(window_props.B_sampled);
				// yield_in_windows.push_back(yield_window);
				// chi_in_windows.push_back(chi_window);
				// bs_in_windows.push_back(bs_window);

				// update cumulative yield and bs 
				yield_prev = yield;
				bs_prev = B_sampled;

				// remainder biomass and yield (new values to update chi)
				double bs_remainder = fmax(B - B_sampled, 0);
				double yield_remainder = fmax(quota - yield, 0);

				// update chi once yield goes above 0. This condition is to prevent degenerate points in regression
				if (yield > 0 && window_props.n_fishable > 1){
					update_chi(chi_in_windows, yield_in_windows, bs_in_windows, yield_remainder, bs_remainder);
				}

				// reset window_props
				window_props = WindowProps();
			}

			if (return_progress){
				progress.insert(progress.end(), 
					{
						static_cast<double>(f.age),
						B,
						B_sampled,
						yield,
						yield_expected,
						chi,
						window_props.chi,
						window_props.B_sampled,
						window_props.B_start,
						window_props.yield
					});
			}

		}

		if (debug){
			std::cout 
				<< B/1e9 << " "
				<< B_sampled/1e9 << " "
				<< yield/1e9 << " "
				<< yield_expected/1e9 << " "
				<< chi << " "
				<< window_props.chi << " "
				<< window_props.B_sampled/1e9 << " "
				<< window_props.B_start/1e9 << " "
				<< window_props.yield/1e9 << " "
				<< std::endl;
		}
	} 
	survival_mean /= n_survival_mean;

	if (return_progress) return progress;
	else return {
		yield,
		to_sea_bed,
		survival_mean
	};
}


double Fleet::effort_constantC(double q, double b, double K){
	double effort = 0;
	for (auto& w : window_props_vec){
		double N0 = w.B_start / K;
		double C  = w.C_rate / K;
		double M  = w.M_fishable;
		double t = par.window_dt/2;
		double effort_t = C/q/(pow( (N0+C/M)*exp(-M*t) - C/M, b));
		effort += effort_t*par.window_dt;
	}
	return effort;
}

double Fleet::effort_constantF(double q, double b, double K){
	double effort = 0;
	for (auto& w : window_props_vec){
		double N0 = w.B_start / K;
		double F  = w.F_fishable;
		double M  = w.M_fishable;
		double t = par.window_dt/2;
		double effort_t = F/q/(pow( N0*exp(-(F+M)*t), b-1));
		effort += effort_t*par.window_dt;
	}
	return effort;
}

