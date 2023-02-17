#ifndef FISHERY_FISH_H_
#define FISHERY_FISH_H_

#include <vector>
#include <map>
#include "functions.h"
#include "initializer_v2.h"

enum class GrowthModel       {Dankel22, Bioenergetic};
enum class MaturationModel   {Dankel22, Bioenergetic};
enum class MortalityModel    {Dankel22, Bioenergetic};
enum class RecruitmentModel  {BevertonHoltDirect, BevertonHoltBioenergetic, RickerDirect, RickerBioenergetic};

class FishParams {
	private:
	std::map<std::string, GrowthModel> growth_names_map{
		{"Dankel22",     GrowthModel::Dankel22}, 
	    {"Bioenergetic", GrowthModel::Bioenergetic}};

	std::map<std::string, MaturationModel> maturation_names_map{
		{"Dankel22",     MaturationModel::Dankel22}, 
	    {"Bioenergetic", MaturationModel::Bioenergetic}};

	std::map<std::string, MortalityModel> mortality_names_map{
		{"Dankel22",     MortalityModel::Dankel22}, 
	    {"Bioenergetic", MortalityModel::Bioenergetic}};

	std::map<std::string, RecruitmentModel> recruitment_names_map{
		{"BevertonHoltDirect",       RecruitmentModel::BevertonHoltDirect}, 
	    {"BevertonHoltBioenergetic", RecruitmentModel::BevertonHoltBioenergetic},
	    {"RickerDirect",             RecruitmentModel::RickerDirect}, 
	    {"RickerBioenergetic",       RecruitmentModel::RickerBioenergetic}};
	
	public:
	// Original parameters (from file) North Arctic cod
	//double amax = 20;
	double beta; // = 0.655793; // 0.648728;   
	double r; // = 0.090367; // 0.077281;
	double c; // = 6.519584; // 6.318308; //6.51559;
	double q; // = 1;
	double k; // = 0.00674;
	double alpha; // = 3.056863227;
	double E1; // = 2500;
	double pmrn_lp50; // = 148.6918; //118.122779;
	double pmrn_width; // = 47.532614;
	double pmrn_slope; // = -6.609008;
	double pmrn_envelope; // = 0.25;
	double Lref; // = 70.48712; //80;
	
//	// Pure power law
//	double Mref = 0.20775; // ////0.1421; //<--old value from file
//	double b    =  1.58127; //////1.8131;
//	double M0   = 0; //
	
	// Power law + offset
	double Mref; // = 0.062994; // 0.20775; // ////0.1421; //<--old value from file
	double b; //    = 2.455715; // 1.58127; //////1.8131;
	double M0; //   = 0.162126; //  0; //

	// Juvenile length and survival probability 
	double L0; // = 9.1;
	double s0; // = 0.08094733; // 0.02; // 0.09637
	double Bhalf; // = 187837572; //3.65e8;  // Bhalf for recruitment 

	// temperature and density dependence of growth
	double beta1; // = -7.07e-5;
	double beta2; // = 0.178;
	double Tmean; // = 5.61;
	double tsbmean; // = 1.93e9/1e6; // convert kg to kT
	
	// temperature dependence of mortality
	double cT; // = 0.196;
	double Tref; // = 5.61;
		
	// growth 
	double gamma1; // = 0.33333;
	double gamma2; // = 3.519072971;
	double alpha1; // = 3.464594;
	double alpha2; // = 0.00156;
	double gsi; // = 0.464097;

	// maturation
	double pmrn_intercept; // = 18.399575;
	double steepness;  // calculated by constructor
	double beta3;

	// reproducttion
	double delta; // = 1820;
	double beta4;
	
	// mortality
	double gamma3; // = -1.20565;
	double alpha3; // = 0.57792;
//	double lref   = 18.25037;
	double alpha4;
	double alpha1_ref;
	double alpha5;
	double gsi_ref;
	
	// *********** OLD MODEL *****************
	// biology
	double theta = 8.10e-6; // kg/cm 
	double zeta  = 3.01; 
	
	double amax = 30;

	std::vector<double> ma = {0,  0.00000000, 0.00000000, 0.00020000, 0.00270054, 0.05375589, 0.28351881, 0.46168639, 0.57433361, 0.65978050, 0.69259962, 0.65432099, 0.35714286, 1.00000000,
								  1.00000000, 1.00000000, 1.00000000, 1.00000000, 1.00000000, 1.00000000, 1.00000000, 1.00000000, 1.00000000, 1.00000000, 1.00000000, 1.00000000, 1.00000000,
								  1.00000000, 1.00000000, 1.00000000, 1.00000000};   // maturation probability with age
	std::vector<double> mai = {0, 1.48936, 0.61272, 0.36018, 0.26108, 0.22300, 0.20447, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000,
								  0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000}; // mortality rate of immature indivuduals
	std::vector<double> mam = {0, 1.48936, 0.61272, 0.36018, 0.26108, 0.22300, 0.20447, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000,
								  0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000, 0.20000}; // mortality rate of mature indivuduals

	double l8 = 196;
	double kappa = 0.0555;
	double a0 = -0.937;
	
	double r0 = 21.77072;
	// ***************************************

	// debug
	int flag = 0;
	
	std::string growth_model_name;
	std::string maturation_model_name;
	std::string mortality_model_name;
	std::string recruitment_model_name;
	
	GrowthModel growth_model;
	MaturationModel maturation_model;
	MortalityModel mortality_model;
	RecruitmentModel recruitment_model;

	// bool use_old_model_mat = false;
	// bool use_old_model_fec = false;
	// bool use_old_model_mor = false;
	
	bool verbose = false;

	void init();
	void initFromFile(std::string params_file);
	
	void print();

	FishParams(){
		// init();
	}
};


class Fish{
	//private:
	//static int nfish = 0;

	public:
	FishParams par;   ///< Parameters object that holds all necessary fish parameters
	
	// state variables
	int age = 1;		       ///< Current age in years
	double length;             ///< Current length in cm
	double gsi_effective = 0;  ///< Effective GSI (saved from previous growth calculation)

	double dl_real,            ///< Linear length increment (\f$l_a-l_{a-1}\f$) calculated with actual density. 
	       dl_potential;       ///< Linear length increment (\f$l_a-l_{a-1}\f$) calculated with 0 density (tsb = 0). 

	// physiological variables
	double weight;	           ///< weight in kg

	bool isMature = false;     ///< Flag indicating whether the fish is mature
	bool isAlive = true;       ///< Flag indicating whether the fish is alive

	double t_birth;            ///< Year of birth (not used)

	std::vector<double> trait_scalars = {6.5, 0.09, 150, 6.6, 50, 0.06};
	std::vector<double> trait_variances = {0, 0, 0, 0, 0, 0};
	std::vector<std::string> trait_names = {"alpha1", "gsi", "pmrn_intercept", "pmrn_slope", "pmrn_width", "s0"};

	Fish(double tb = 0);
	
	/// Construct a fish and initialize parameters using a parameters file
	Fish(std::string params_file); 

	/// @brief Set fish age and other variables that scale directly with age
	void set_age(int _a);      
	/// @brief Set fish length and other variables that scale directly with length
	void set_length(double s); 

	void set_traits(std::vector<double> traits);
	std::vector<double> get_traits();

	/// @brief Initializes the fish, i.e., initializes parameters and sets the initial state (age = 1, length)
	/// @param tsb Total stock biomass at birth (kT i.e. 10^6 kg)
	/// @param temp Temperature at birth (deg C)
	void init(double tsb, double temp);
	
	/// @brief Implement growth, i.e., set new length and effective GSI after 1 year of growth.
	/// @param tsb Total stock biomass during the growing season (kT i.e. 10^6 kg)
	/// @param temp Temperature during the growing season (deg C)
	void grow(double tsb, double temp);

	/// @brief Calculate the probability of maturation
	double maturationProb(double temp);
	// @brief Check if the fish should mature in the current year based on maturationProb()
	//bool matureNow();
	/// @brief Implement maturation is matureNow() returns true.
	void updateMaturity(double temp);	
	
	/// @brief Calculate the instantaneous matural mortality rate
	double naturalMortalityRate(double temp);

	/// @brief Calculate the number of surviving recruits produced based on egg production and offspring survival until recruitment.
	/// @param ssb Total spawning stock biomass \f$S\f$ of the population (kg)
	/// @param temp Temperature (deg C)
	double produceRecruits(double ssb, double temp);

	void print();
	void print_line();
	void print_header();
	
	/// Get the state of the fish 
	std::vector<double> get_state();
};

#endif
