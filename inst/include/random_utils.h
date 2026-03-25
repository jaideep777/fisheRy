#ifndef FISHERY_RANDOM_UTILS
#define FISHERY_RANDOM_UTILS

#include <random>

extern std::random_device rd;
extern std::mt19937 rng;
extern std::uniform_real_distribution<double> uniform_dist;
extern std::normal_distribution<double> normal_dist;


double runif(double rmin=0, double rmax=1);
double rnorm(double mu=0, double sd=1);
double rlognorm(double mu=0, double sd=1);


#endif
