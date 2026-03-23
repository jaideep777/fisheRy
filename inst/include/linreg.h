#ifndef FISHERY_LINREG_H
#define FISHERY_LINREG_H

#include <vector>
#include <iostream>
#include <cmath>
#include <algorithm>
#include <numeric>

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
	if (res.slope == 0) return 1e-12;
	else return (y_new - res.intercept)/res.slope;
}

#endif
