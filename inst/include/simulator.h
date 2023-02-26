#ifndef FISHERY_SIMULATOR_H
#define FISHERY_SIMULATOR_H

#include <vector>
#include "population.h"
#include "tensor.h"

#include <Rcpp.h>

// FIXME: In calculation of normalized utilities, exclude -ve profit regions
// FIXME: See why harmonic mean blows up

class Simulator{
	public:
	Population noFishingPop;
	
	public:
	bool verbose = false;

	public:
	Simulator(Fish f);

	void setNaturalPopulation(Population & pop); 
	std::vector<double> equilibriateNaturalPopulation(double tsb0, double temp, double _n = 2e6);

	Tensor<double> simulate_multi(Population &pop, std::vector<double> hvec, int nyears, double tsb0, double temp, bool re_init);
	std::vector<double> max_avg_utils(std::vector<int> dims, std::vector<double> data);
	std::vector<double> stakeholder_satisfaction(std::vector<int> dims, std::vector<double> data);
	
//	double calcK(Population &pop, double lmin, double h);
//	std::vector<double> calcK_2d(Population &pop, std::vector<double> lminvec, std::vector<double> hvec);

	Tensor<double> simulate_multi_2d(Population pop, std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, double tsb0, bool re_init);
	std::vector<double> max_avg_utils_2d(std::vector<int> dims, std::vector<double> data);
	std::vector<double> stakeholder_satisfaction_2d(std::vector<int> dims, std::vector<double> data);
	
	Rcpp::DataFrame simulate_r(Population &pop, double lf, double h, int nyears, double tsb0, double temp, bool re_init);

	Rcpp::NumericVector simulate_multi_r(Population &pop, std::vector<double> hvec, int nyears, double tsb0, double temp, bool re_init);
	Rcpp::NumericVector simulate_multi_2d_r(Population pop, std::vector<double> Tvec, std::vector<double> lminvec, std::vector<double> hvec, int nyears, double tsb0, bool re_init);

};

#endif

