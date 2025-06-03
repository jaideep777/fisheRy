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

inline linregresult linreg0(const std::vector<double>& x, const std::vector<double>& y) {
	// Calculate the numerator and denominator for the slope (m)
	double numerator = std::inner_product(x.begin(), x.end(), y.begin(), 0.0);
	double denominator = std::accumulate(
		x.begin(), x.end(), 0.0,
		[](double acc, double xi) { return acc + (xi * xi); }
	);

	linregresult res;
	res.slope = numerator / denominator;
	res.intercept = 0;
	
	// std::cout << "linreg0: \n"; 
	// std::cout << "  x = "; for (auto xx : x) std::cout << xx << " "; std::cout << '\n';
	// std::cout << "  y = "; for (auto yy : y) std::cout << yy << " "; std::cout << '\n';
	// std::cout << "  res: slope/int = " << res.slope << " / " << res.intercept << '\n';

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
	READ_PAR(lmin_sq);

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

	#undef READ_PAR

}

void FleetParams::print(){
	#define PRINT_PAR(x) std::cout << #x << " = " << x << "\n"

	// status quo lmin
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

	#undef PRINT_PAR
}



Fleet::Fleet() : g(rd()){
}

void Fleet::readParams(std::string params_file, bool verbose){
	par.initFromFile(params_file, verbose);
}


void Fleet::set_harvestProportion(double _h){
	h = _h;
	Fc = -log(1-_h);
}

void Fleet::set_minSizeLimit(double _lf50){
	double dl = _lf50 - par.lmin_sq;
	par.F3 = par.F3_sq + dl;
	par.F5 = par.F5_sq + dl;
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


bool Fleet::isFishable(const Fish &f){
	return f.length >= par.F3;
}


double Fleet::biomassFishable(const Stock &stock, double min_age){
	return 
	std::accumulate(stock.fishes.begin(), stock.fishes.end(), 0.0, 
		[min_age, this, &stock](double sum, const Fish& f) { 
			return sum + ((f.isAlive && isFishable(f) && f.age >= min_age) ? f.weight * stock.superfish_size : 0);
		}
	);
}

/// This function computes the average per capita natural mortality rate over the fishable population.
///
/// \f[
///   \mu = \frac{1}{N} \sum_{i} \left( \mu_i(T) + \mathbb{1}[\text{Mature}] \cdot M_\text{spawning} \right) \mathbb{1}[\text{fishbale}]
/// \f]
///
/// If no fishable fish are present, the average is set to 0.
///
/// @see Fish::naturalMortalityRate, fishes
double Fleet::naturalMortFishable(const Stock& stock, double temp){
	return 
	avgOverFishable(
		[temp](const Fish& f){
			return f.naturalMortalityRate(temp) + double(f.isMature)*f.par.Mspawning; // FIXME: Is it correct to include spawning mortality here?
		},
		stock
	);
}


/// This function computes the average per capita reference fishing mortality rate over the fishable population.
/// 
/// \f[
///   \mu = \frac{1}{N} \sum_{i} F_\text{ref}(l_a) \mathbb{1}[\text{fishable}]
/// \f]
/// 
/// If no fishable fish are present, the average is set to 0.
/// 
/// @see Fish::naturalMortalityRate, fishes
double Fleet::fishingMortRefFishable(const Stock& stock){
	return 
	avgOverFishable(
		[this](const Fish& f){
			return fishingMortalityRef(f.length);
		},
		stock
	);
}


/// This function computes the average maturity rate the fishable population.
/// 
/// \f[
///   \mu = \frac{1}{N} \sum_{i} M(l_a) \mathbb{1}[\text{fishable}]
/// \f]
/// 
/// If no fishable fish are present, the average is set to 0.
/// 
/// @see Fish::maturity, fishes
double Fleet::maturityFishable(const Stock& stock){
	return 
	avgOverFishable(
		[](const Fish& f){
			return (f.isMature)? 1:0;
		},
		stock
	);
}


void Fleet::init_chi(Stock &pop, double F_fgf, double temp){
	double Fref_fishable = fishingMortRefFishable(pop);
	double Mort_fishable = naturalMortFishable(pop, temp);

	chi = (Fref_fishable == 0)? 0 : F_fgf/Fref_fishable;

	double h = 1-exp(-F_fgf);
	chi *= exp(chi0_scalar_slope*(h-0.5));
}


void Fleet::update_chi(const std::vector<double>& chi_in_windows, 
					   const std::vector<double>& yield_in_windows, 
					   const std::vector<double>& bs_in_windows,
					   double yield_remainder, double bs_remainder){
	// 3a. calibrate yield model (y = Bs * f(X))
	linregresult res;
	std::vector<double> y(yield_in_windows.size());
	if (control_model == "exp"){
		// exponential model: y = Bs (1-e^-kX) --> -log(1-y/Bs) = kX
		std::transform(yield_in_windows.begin(), yield_in_windows.end(),
						bs_in_windows.begin(), y.begin(),
						[](double yield, double bs) {
							return (bs == 0)? 0 : -log(1 - (yield / bs));
						});

		res = linreg0(chi_in_windows, y);
	}
	else if (control_model == "linear"){
		// linear model: y = Bs (k X) --> y/Bs = kX
		std::transform(yield_in_windows.begin(), yield_in_windows.end(),
						bs_in_windows.begin(), y.begin(),
						[](double yield, double bs) {
							return (bs == 0)? 0 : yield / bs;
						});

		res = linreg0(chi_in_windows, y);
	}
	else {
		// throw std::runtime_error("Unsopported control model");
	}

	// 3b. predict new chi
	if (control_model == "exp"){
		// exponential model: y = Bs (1-e^-kX)
		// std::cout << "using exp model" << std::endl;
		if (yield_remainder >= bs_remainder) chi = 1e20;
		else chi = linreg_predict_inverse(-log(1 - (yield_remainder/bs_remainder)), res);
	}
	else if (control_model == "linear"){
		// linear model: y = Bs k X
		// std::cout << "using linear model" << std::endl;
		chi = linreg_predict_inverse((yield_remainder/bs_remainder), res);
	}

	if (isinf(chi) || isnan(chi) || chi > 1e20) throw std::runtime_error("Regressed chi is Inf or NA or extremely large");

	chi = std::clamp(chi, 1e-6, 1e20);

}


/// Note: this function takes pop by reference so it IS altered
/// Some computations are doubled in the function below, but that's ok for now as it serves to
/// cross-check those calcs. These can be removed after sufficient testing
std::vector<double> Fleet::harvest(Stock& pop, double quota, double temp, bool return_progress){
	double yield = 0, to_sea_bed = 0;
	double survival_mean = 0, n_survival_mean = 0;
	int count = 0, n_alive = 0;
	window_props_vec.clear(); // clear old data in windows 

	for (auto& f : pop.fishes) n_alive += f.isAlive? 1:0;
	
	shuffle(pop.fishes.begin(), pop.fishes.end(), g);

	double B = biomassFishable(pop, 0);
	// double quota = B*h; // Should this be fishable biomass at start of season or after SPF?
	double B_sampled = 0;
	double yield_expected;

	std::vector<double> progress;
	std::vector<double> chi_in_windows(1, 0), yield_in_windows(1, 0), bs_in_windows(1, 0);
	double yield_prev = 0, bs_prev = 0;
	int window_n = std::ceil(window_dt*n_alive);
	int windows_sampled = 0;
	WindowProps window_props;
	for (auto& f : pop.fishes){
		if (f.isAlive){
			bool f_is_fishable = isFishable(f);

			B_sampled += (f.isAlive && f_is_fishable)? f.weight*pop.superfish_size : 0;
			yield_expected = (B_sampled/B) * quota;
			if (yield_expected > quota) throw std::runtime_error("Expected yield exceeds quota");

			double fishing_mort_rate = chi*fishingMortalityRef(f.length); 
			double natural_mort_rate = f.naturalMortalityRate(temp); // This does not (should not) include spawning-related mortality
			double mortality_rate = natural_mort_rate + fishing_mort_rate; // post-spawning mortality rate is same for mature and immature individuals
			double survival_prob = exp(-mortality_rate*1.0);	// mortality in feeding grounds (post-spawning), over full year. Note that survival prob must be annualized because this fish will be iterated over only once
			survival_mean += survival_prob;
			n_survival_mean += 1;

			window_props.M_fishable += f_is_fishable? natural_mort_rate : 0;
			window_props.F_fishable += f_is_fishable? fishing_mort_rate : 0;
			window_props.B_sampled  += f_is_fishable? f.weight*pop.superfish_size : 0;
			window_props.n_fishable += f_is_fishable? 1 : 0;

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

			if (count >= window_n){ 
				count = 0;
				++windows_sampled;

				// get parameters and outcomes realized during this window
				double chi_window = chi;
				double yield_window = yield - yield_prev;
				double bs_window = B_sampled - bs_prev;

				window_props.chi = chi;
				window_props.B_start = B - yield - to_sea_bed;
				window_props.C_rate = window_props.yield/window_dt; // catch rate = annualized yield = yield per year
				window_props.M_fishable /= window_props.n_fishable; 
				window_props.F_fishable /= window_props.n_fishable; 

				// std::cout << "Yield window: " << yield_window << " " << window_props.yield << '\n';
				// std::cout << "Bs window: " << bs_window << " " << window_props.B_sampled << '\n';

				assert(fabs(yield_window - window_props.yield) < 1e-5);
				assert(fabs(bs_window - window_props.B_sampled) < 1e-5);

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
				if (yield > 0){
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

		// std::cout << B << " "
		// 		<< B_sampled << " "
		// 		<< yield << " "
		// 		<< yield_expected << " "
		// 		<< chi << " "
		// 		<< window_props.chi << " "
		// 		<< window_props.B_sampled << " "
		// 		<< window_props.B_start << " "
		// 		<< window_props.yield << " "
		// 		<< '\n';

	} 
	survival_mean /= n_survival_mean;
	return progress;
}


double Fleet::effort_constantC(double q, double b, double K){
	double effort = 0;
	for (auto& w : window_props_vec){
		double N0 = w.B_start / K;
		double C  = w.C_rate / K;
		double M  = w.M_fishable;
		double t = window_dt/2;
		double effort_t = C/q/(pow( (N0+C/M)*exp(-M*t) - C/M, b));
		effort += effort_t*window_dt;
	}
	return effort;
}

double Fleet::effort_constantF(double q, double b, double K){
	double effort = 0;
	for (auto& w : window_props_vec){
		double N0 = w.B_start / K;
		double F  = w.F_fishable;
		double M  = w.M_fishable;
		double t = window_dt/2;
		double effort_t = F/q/(pow( N0*exp(-(F+M)*t), b-1));
		effort += effort_t*window_dt;
	}
	return effort;
}

