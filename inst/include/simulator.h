#ifndef FISHERY_SIMULATOR_H
#define FISHERY_SIMULATOR_H

#include <vector>
#include "population.h"
#include "tensor.h"

#ifndef NATIVE_CPP
#include <Rcpp.h>
#endif

// FIXME: In calculation of normalized utilities, exclude -ve profit regions
// FIXME: See why harmonic mean blows up

class Simulator{
	public:
	Population noFishingPop;
	
	public:
	bool verbose = false;

	public:
	Simulator(Fish f);

	void setNaturalPopulation(const Population& pop); 
	std::vector<double> equilibriateNaturalPopulation(std::string params_file, double temp, double _n = 2e6);

	Tensor<double> simulate_multi_2d(Population pop, std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, double tsb0, bool re_init);
	std::vector<double> max_avg_utils_2d(std::vector<int> dims, std::vector<double> data);
	std::vector<double> stakeholder_satisfaction_2d(std::vector<int> dims, std::vector<double> data);

	std::vector<double> stakeholder_satisfaction_2d_t(std::vector<int> dims, std::vector<double> data);

#ifndef NATIVE_CPP
	Rcpp::DataFrame simulate_r(Population &pop, double lf, double h, int nyears, double tsb0, double temp, bool re_init, std::string output_file);
	Rcpp::NumericVector simulate_multi_2d_r(Population pop, std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, double tsb0, bool re_init);
#endif

};

#endif

