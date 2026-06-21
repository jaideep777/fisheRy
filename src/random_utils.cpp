#include "random_utils.h"

static constexpr std::mt19937::result_type DEFAULT_RANDOM_SEED = 5489u;
std::mt19937 rng(DEFAULT_RANDOM_SEED);
std::uniform_real_distribution<double> uniform_dist(0.0, 1.0);
std::normal_distribution<double> normal_dist(0.0, 1.0);

void set_rng_seed(unsigned int seed){
    rng.seed(seed);
}

double runif(double rmin, double rmax){
	double r = uniform_dist(rng);
	return rmin + (rmax-rmin)*r;
}

double rnorm(double mu, double sd){
	double x = normal_dist(rng);
	return mu + sd*x;
}

double rlognorm(double mu, double sd){
	double x = normal_dist(rng);
	return exp(mu + sd*x);
}
