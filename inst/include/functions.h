#ifndef FISHERY_FUNCTIONS_H
#define FISHERY_FUNCTIONS_H

#include <cmath>
#include <algorithm>
#include <iostream>

// #include <Rcpp.h>

namespace fish {

//// [[Rcpp::export]]
//inline double init_length(double gamma_1, double gamma_2, double alpha_1, double alpha_2){
////	double s = gamma_1 * alpha_1 / alpha_2;
////	return pow(s, 1 / gamma_2);
//	return 19.18;
//}



// [[Rcpp::export]]
/// @brief Compute the nonlinear growth increment \f$\Delta l_p = l_a^{\gamma_1 \gamma_2} - l_{a-1}^{\gamma_1 \gamma_2}\f$, as follows
/// \f[
///   \Delta l_p = \gamma_{1}\alpha_{1}\alpha_{2}^{- \gamma_{1}} e^{(\beta_1\Delta B +\beta_2 \Delta T)} 
/// \f]
/// @param tsb_ano Total stock biomass anomaly (kilo Tons, i.e. 1e6 kg)
/// @param temp_ano Temperature anomaly (deg C)
inline double dl_power(double tsb_ano, double temp_ano, double gamma_1, double gamma_2, double alpha_1, double alpha_2, double beta_1, double beta_2){
	double dl0 = gamma_1 * alpha_1 * pow(alpha_2, -gamma_1);
	double dl1 = dl0 * exp(beta_1*(tsb_ano) + beta_2*(temp_ano));
	return dl1;
}


// [[Rcpp::export]]
/// @brief Compute the new length after growth assuming no allocation to reproduction
/// \f[l'_{a} = \left(l_{a - 1}^{\gamma_{1}\gamma_{2}} + \Delta l_p\right)^{\frac{1}{{\gamma_{1}\gamma_{2}}}}\f]
/// @param pow_dl The generalized length increment \f$\Delta l_p\f$ as calculated by dl_power()
inline double length_juvenile(double init_length, double pow_dl, double gamma_1, double gamma_2){
	double lp0 = pow(init_length, gamma_1 * gamma_2);
	double lp1 = lp0 + pow_dl;
	return pow(lp1, 1 / (gamma_1 * gamma_2));
}


// [[Rcpp::export]]
/// @brief Compute the new length after growth for adults, assuming allocation to reproduction
/// \f[\text{max}\left\{\frac{l'_{a}}{(1 + \gamma_{1}g)^{\frac{1}{{\gamma_{1}\gamma_{2}}}}},\ l_{a-1}\right\}\f]
/// @param pow_dl The generalized length increment \f$\Delta l_p\f$ as calculated by dl_power()
inline double length_adult(double init_length, double pow_dl, double gamma_1, double gamma_2, double gsi){
	double l = length_juvenile(init_length, pow_dl, gamma_1, gamma_2) / pow(1 + gamma_1 * gsi, 1 / (gamma_1 * gamma_2));
	return std::max(l, init_length); 
}


// [[Rcpp::export]]
/// @brief Calculate the steepness of the probabilistic maturation reaction norm (PMRN)
/// \f[d_{m} = \frac{\Delta l_{50}}{\ln{\left( \frac{1 - p}{p} \right) - \ln\left( \frac{p}{1 - p} \right)}}\f]
inline double maturation_steepness(double pmrn_width, double pmrn_envelope){
	return pmrn_width / (log((1- pmrn_envelope) / pmrn_envelope) - log(pmrn_envelope / (1 - pmrn_envelope)));
}


// [[Rcpp::export]]
/// @brief Calculate the maturation probability given age \f$a\f$ and body length \f$l_a\f$,
/// \f[m(a,l_a) = \frac{1}{1 + e^{-\left(\frac{l_{a} - (s_{m}a + i_{m})}{d_{m}} + \beta_3(T-T_\text{ref})\right)}}\f]
/// @param steepness Steepness \f$d_m\f$ of the probabilistic maturation reaction norm (PMRN) as calculated by maturation_steepness()
/// @param pmrn_slope Slope \f$s_m\f$ of the PMRN 
/// @param pmrn_intercept Intercept \f$i_m\f$ of the PMRN
/// @param temp Temperature (deg C) 
inline double maturation_probability(double age, double body_length, double temp, double Tref, double steepness, double pmrn_slope, double pmrn_intercept, double beta_3){
	double pmrn_midpoint = pmrn_slope * age + pmrn_intercept;
	double arg = (body_length - pmrn_midpoint)/steepness + beta_3*(temp - Tref);
	double p = 1.0 / (1.0 + exp(-arg));
	//std::cout << "p_mat: age = " << age << ", length = " << body_length << ", slope = " << pmrn_slope << ", int = " << pmrn_intercept << ", mid = " << pmrn_midpoint << ", steepness = " << steepness << ", p = " << p << "\n";
	return p;  
}


// [[Rcpp::export]]
/// @brief Compute weight of fish (grams) given length (cm), 
/// \f[w = \alpha_2 l^{\gamma_2}\f]
inline double weight_fish(double body_length, double alpha_2, double gamma_2){
	return alpha_2 * pow(body_length, gamma_2);
}


// [[Rcpp::export]]
/// @brief Calculate the number of eggs produced per year by a fish of given weight,
/// \f[f(a,l_a) = \delta g_{a}^{'} \cdot w(l_a)\f]
/// @param gsi Effective GSI as calculated by gsi()
inline double fecundity(double weight, double delta, double gsi) {
	return delta * gsi * weight;
}


// effective GSI given initial and final length
// [[Rcpp::export]]
/// @brief Calculate effective GSI, given the initial and final body lengths.
/// \f[g_{a}^{'} = \frac{ \Delta l_p - (l_{a}^{\gamma_{1}\gamma_{2}} - l_{a - 1}^{\gamma_{1}\gamma_{2}})}{\gamma_{1}l_{a}^{\gamma_{1}\gamma_{2}}}\f]
/// @param pow_dl The generalized length increment \f$\Delta l_p\f$ as calculated by dl_power()
inline double gsi(double body_length, double init_body_length, double pow_dl, double gamma_1, double gamma_2, double alpha_1, double alpha_2){
	double lp0 = pow(init_body_length, gamma_1 * gamma_2);
	double lp1 = pow(body_length, gamma_1 * gamma_2);
//	double dl = gamma_1 * alpha_1 * pow(alpha_2, -gamma_1);
//	double d  = gamma_1 * pow(body_length, gamma_1 * gamma_2);
	
	return fmax(0, (lp0 - lp1 + pow_dl)/(lp1*gamma_1)); // ensure that effective GSI is not negative due to numerical errors
}


// [[Rcpp::export]]
/// @brief Calculate the instantaneous natural mortality
/// \f[M(a,l_a) = \left( \mu_0 + \alpha_{3}\left( \frac{l_{a}}{l_{\text{ref}}} \right)^{\gamma_{3}} \right)\left(\frac{T}{T_\text{ref}}\right)^{c_T}\f] 
/// @param temp Temperature (deg C)
/// @param M0 baseline (size-independent) mortality rate
/// @param body_length_ref Reference body length at unit size-dependent mortality rate
/// @param Tref Reference temperature at which mortality rate is measured
/// @param cT Exponent of the temperature dependence of mortality
inline double natural_mortality(double body_length, double temp, double M0, double gamma_3, double alpha_3, double body_length_ref, double Tref, double cT){
	return (M0 + alpha_3 * pow(body_length / body_length_ref, gamma_3)) * pow(temp/Tref, cT);
}

// [[Rcpp::export]]
/// @brief Calculate fishing selectivity
/// @param len   Length og fish
/// @param sf    Slope of the selectivity curve
/// @param lf50  Midpoint of the selectivity curve
/// @return      Selectivity
inline double fishing_selectivity(double len, double sf, double lf50){
	return 1/(1+exp(-sf*(len-lf50)));
}


// [[Rcpp::export]]
/// @brief Calculate survival probability over a time interval dt (years) 
/// \f[s(a,l_a) = e^{- (M(a,l_a) + F)}\f]
/// where \f$M(a, l_a)\f$ is the instantaneous natural_mortality() rate 
///
/// For parameter definitions see also natural_mortality()
/// @param fishing_mortality Instantaneous fishing mortality rate \f$F\f$
/// @param dt Interval over which mortality is applied (years)
inline double survival_probability(double body_length, double temp, double M0, double gamma_3, double alpha_3, double body_length_ref, double Tref, double cT, double fishing_mortality, double dt = 1){
	double total_mortality = natural_mortality(body_length, temp, M0, gamma_3, alpha_3, body_length_ref, Tref, cT) + fishing_mortality;
	return exp(-total_mortality * dt);
}


} // namespace fish

#endif





