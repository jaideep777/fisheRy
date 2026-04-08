#include "random_utils.h"

std::random_device rd;
std::mt19937 rng(rd());
std::uniform_real_distribution<double> uniform_dist;
std::normal_distribution<double> normal_dist;

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
