#include "fish.h"
#include <iostream>
#include <cmath>
#include <stdexcept>
using namespace std;


// ************************ Fish ***************************

// Fish::Fish(double tb){
// 	t_birth = tb;
// }

/// Reads parameters from the file and initializes them
/// @param params_file Parameters in .ini format
Fish::Fish(string params_file){
	t_birth = 0;
	par.initFromFile(params_file);
	//par.print();
}

//Fish::Fish(double xb, double tb){
	//set_age(
	//set_length(xb);
	//t_birth = tb;
	//age = 0;
//}


void Fish::init(double tsb, double temp){
	par.init();
	
	/// - Initialization sets age to 1. In Dankel et al model, this will also set length.
	set_age(1);
	
	/// - In Joshi et al model, length at age 1 is explicitly calculated using length, temperature, and TSB, at birth.
	if (par.growth_model == GrowthModel::Bioenergetic){
		// calc length at age 1
		double tsb_ano = tsb - par.tsbmean;
		double temp_ano = temp - par.Tmean;
		double dl = fish::dl_power(tsb_ano, temp_ano, par.gamma1, par.gamma2, par.alpha1, par.alpha2, par.beta1, par.beta2);
		double l1 = fish::length_juvenile(par.L0, dl, par.gamma1, par.gamma2);

		set_length(l1);
	}
}

void Fish::set_age(int _a){
	age = _a;

	/// In Dankel model, length is a direct function of age.
	/// Hence, setting age automatically sets length in the Dankel et al model.
	if (par.growth_model == GrowthModel::Dankel22){
		length = par.l8*(1-exp(-par.kappa*(age-par.a0)));
		set_length(length);
	}
}

void Fish::set_length(double s){
	length = s;
	
	if (par.growth_model == GrowthModel::Dankel22){ 
		weight = par.theta*pow(length, par.zeta);  // Eq. 2
	}
	else{
		/// weight_fish() returns weight in grams, whereas the standard unit used throughout the simulations is kg. 
		/// Hence division by 1000.
		weight = fish::weight_fish(length, par.alpha2, par.gamma2)/1000;  
	}
	
}

void Fish::set_traits(vector<double> traits){
	vector<double>::iterator it = traits.begin();
	par.alpha1         = fmax(1e-3, *it++);
	par.gsi            = fmax(1e-6, *it++);
	par.pmrn_intercept = fmax(1e-3, *it++);
	par.pmrn_slope     = *it++;
	par.pmrn_width     = fmax(1e-3, *it++);
	par.s0             = fmax(1e-6, *it++);
}

vector<double> Fish::get_traits(){
	return {
		  par.alpha1        
		, par.gsi
		, par.pmrn_intercept
		, par.pmrn_slope
		, par.pmrn_width
		, par.s0
		};
}


double Fish::naturalMortalityRate(double temp){
	double rate;
	if (age > par.amax) return 1e20; // FIXME: use inf
	else {
		if (par.mortality_model == MortalityModel::Dankel22){
		   	if (isMature) rate = par.mam[age]; 
			else          rate = par.mai[age]; 
			return rate;
		}
		else if (par.mortality_model == MortalityModel::Bioenergetic){
			//return fish::natural_mortality(length, temp, par.M0, par.gamma3, par.alpha3, par.Lref, par.Tref, par.cT);
			return (par.Mspawning*double(isMature)*(par.L0/length) + par.M0 + par.alpha3 * pow(length / par.Lref, par.gamma3) + par.alpha4*(par.alpha1*par.alpha1 - par.alpha1_ref*par.alpha1_ref) + par.alpha5*(par.gsi - par.gsi_ref)) * pow(temp/par.Tref, par.cT);
		}
		else{
			throw std::runtime_error("Invalid mortality model specified");
		}
	}
}


double Fish::maturationProb(double temp){
	if (par.maturation_model == MaturationModel::Dankel22) 
		return (age > par.amax)? 1 : par.ma[age];
	else if (par.maturation_model == MaturationModel::Bioenergetic) 
		return fish::maturation_probability(age, length, temp, par.Tref, par.steepness, par.pmrn_slope, par.pmrn_intercept, par.beta3);
	else 
		throw std::runtime_error("Invalid maturation model specified");
}


// bool Fish::matureNow(){
	// double runif = rand() / double(RAND_MAX);
	// 
	// // double maturation_prob;
	// // if (par.use_old_model_mat) maturation_prob = (age > par.amax)? 1 : par.ma[age];
	// // else                       maturation_prob = fish::maturation_probability(age, length, par.steepness, par.pmrn_slope, par.pmrn_intercept);
	// // //cout << "matureNow(): " << age << " " << length << " " << runif << " " << maturation_prob << "\n";
	// 
	// return runif <= maturationProb();
// }
// 

void Fish::updateMaturity(double temp){
	isMature = isMature || ((rand()/double(RAND_MAX)) <= maturationProb(temp));	
}


/// In Dankel et al model, this function does nothing. New length is set by set_age().
/// 
/// In Joshi et al, we first calculate \f$\Delta l_p\f$, and then increment length based on whether the 
/// individual is a juvenile or adult. We also calculate the effective GSI and set the new length.
///
/// Here, two length increments are calculated based on tsb: potential increment is when tsb = 0, 
/// i.e., there is not density constraint on growth. real increment is calculated using the actual tsb.
/// Actual length increment is based on the real increment. Potential increment is for analysis purposes.
void Fish::grow(double tsb, double temp){
	if (par.growth_model == GrowthModel::Dankel22){
		// do nothing. age is incremented by population update
		gsi_effective = par.gsi; // required if new fecundity model is used in combination with old growth model
	}
	else if (par.growth_model == GrowthModel::Bioenergetic){
		double tsb_ano = tsb - par.tsbmean;
		double temp_ano = temp - par.Tmean;
		
		// This is generalized increment l2^y1y2 - l1^y1y2
		double dl     = fish::dl_power(tsb_ano,      temp_ano, par.gamma1, par.gamma2, par.alpha1, par.alpha2, par.beta1, par.beta2);
		double dl_pot = fish::dl_power(-par.tsbmean, temp_ano, par.gamma1, par.gamma2, par.alpha1, par.alpha2, par.beta1, par.beta2);
		
		double lnew, lnew_pot;
		if (isMature){
			lnew     = fish::length_adult(length, dl,     par.gamma1, par.gamma2, par.gsi);
			lnew_pot = fish::length_adult(length, dl_pot, par.gamma1, par.gamma2, par.gsi);
		}
		else{
			lnew     = fish::length_juvenile(length, dl,     par.gamma1, par.gamma2);
			lnew_pot = fish::length_juvenile(length, dl_pot, par.gamma1, par.gamma2);
		}

		// ------ This is linear increment, just for analysis --------
		dl_real      = lnew     - length;
		dl_potential = lnew_pot - length;
		//cout << "tsb_ano = " << tsb << " / " << par.tsbmean << ", fac = " << dl_real << " / " << dl_potential << endl; 
		// -----------------------------------------------------------

		gsi_effective = fish::gsi(lnew, length, dl, par.gamma1, par.gamma2, par.alpha1, par.alpha2);
		set_length(lnew);
	}
	else{
		throw std::runtime_error("Invalid growth model specified");
	}
	/// This function does not increment age as growth can happen during the beginning of the year. 
	/// Age is incremented separately in the population update at the end of the year.
}


/// In Dankel et al model, the number of recruits is calculated directly as \f$r_0 w\f$.
/// 
/// In Joshi et al, the number of eggs produced is first calculated based on the effective GSI.
/// The number of surviving eggs is then calculated by applying two survival probabilities:
/// - \f$s_0\f$ is the survival probability of offspring until recruitment
/// - \f$1/(1+S/B_{1/2})\f$ is the probability of survival during recruitment. This is modelled as a Beverton-Holt function.
double Fish::produceRecruits(double ssb, double temp){
	double temp_ano = temp - par.Tref;
	if (par.recruitment_model == RecruitmentModel::BevertonHoltDirect){
		double recruits = par.r0 * weight * 1 / (1 + ssb/par.Bhalf);
		return recruits;
	}
	else if (par.recruitment_model == RecruitmentModel::RickerDirect){
		double recruits = par.r0 * weight * exp(par.beta4*temp_ano) * pow(2, -ssb/par.Bhalf);
		return recruits;
	}
	else if (par.recruitment_model == RecruitmentModel::BevertonHoltBioenergetic){
		double eggs = fish::fecundity(weight, par.delta, gsi_effective);  // Total eggs produced
		double recruits = eggs * par.s0 * 1 / (1 + ssb/par.Bhalf);   // offspring surviving density-dependent recruitment
		return recruits; 
	}
	else if (par.recruitment_model == RecruitmentModel::RickerBioenergetic){
		double eggs = fish::fecundity(weight, par.delta, gsi_effective);  // Total eggs produced
		double recruits = eggs * par.s0 * exp(par.beta4*temp_ano) * pow(2, -ssb/par.Bhalf);   // offspring surviving density-dependent recruitment
		return recruits; 
	}
	else{
		throw std::runtime_error("Invalid recruitment model specified");
	}
}


void Fish::print(){
	cout << "Fish: \n";
	cout << "  age = " << age << "\n";
	cout << "  length = " << length << "\n";
	cout << "  weight = " << weight << "\n";
	cout << "  t_birth = " << t_birth << "\n";
	cout << "  isMature = " << isMature << "\n";
	cout << "----\n";
}

void Fish::print_line(){
	cout << t_birth << "\t" << age << "\t" << isMature << "\t" << isAlive << "\t" << length << "\t" << weight << "\t";
}	

void Fish::print_header(){
	cout << "t_birth" << "\t" << "age" << "\t" << "isMature" << "\t" << "isAlive" << "\t" << "length" << "\t" << "weight" << "\t";
}	

std::vector<double> Fish::get_state(){
	return {t_birth, static_cast<double>(age), static_cast<double>(isMature), static_cast<double>(isAlive), length, weight};
}


// *******************************************************
// ******************** Fish Params **********************
// *******************************************************


void FishParams::init(){
	steepness = fish::maturation_steepness(pmrn_width, pmrn_envelope);
	if (steepness <= 0) throw std::runtime_error("Calculated steepness values is <= 0");

	growth_model = growth_names_map.at(growth_model_name);
	maturation_model = maturation_names_map.at(maturation_model_name);
	mortality_model = mortality_names_map.at(mortality_model_name);
	recruitment_model = recruitment_names_map.at(recruitment_model_name);
	
//		s0 = (growth_model == Joshi23)? 0.02 : 0.09637;
}

void FishParams::initFromFile(std::string params_file){
	io::Initializer I;
	I.parse(params_file, false, verbose);

	#define READ_PAR(x) x = I.get<double>("fish", #x)

	READ_PAR(beta); // = 0.655793; // 0.648728;
	READ_PAR(r); // = 0.090367; // 0.077281;
	READ_PAR(c); // = 6.519584; // 6.318308; //6.51559;
	READ_PAR(q); // = 1;
	READ_PAR(k); // = 0.00674;
	READ_PAR(alpha); // = 3.056863227;
	READ_PAR(E1); // = 2500;
	READ_PAR(pmrn_lp50); // = 148.6918; //118.122779;
	READ_PAR(pmrn_width); // = 47.532614;
	READ_PAR(pmrn_slope); // = -6.609008;
	READ_PAR(pmrn_envelope); // = 0.25;
	READ_PAR(Lref); // = 70.48712; //80;
	
	// Power law + offset
	READ_PAR(Mref); // = 0.062994; // 0.20775; // ////0.1421; //<--old value from file
	READ_PAR(b); //    = 2.455715; // 1.58127; //////1.8131;
	READ_PAR(M0); //   = 0.162126; //  0; //

	// Juvenile length and survival probability 
	READ_PAR(L0); // = 9.1;
	READ_PAR(s0); // = 0.08094733; // 0.02; // 0.09637
	READ_PAR(Bhalf); // = 187837572; //3.65e8;  // Bhalf for recruitment 

	// temperature and density dependence of growth
	READ_PAR(beta1); // = -7.07e-5;
	READ_PAR(beta2); // = 0.178;
	READ_PAR(Tmean); // = 5.61;
	READ_PAR(tsbmean); // = 1.93e9/1e6; // convert kg to kT
	
	// temperature dependence of mortality
	READ_PAR(cT); // = 0.196;
	READ_PAR(Tref); // = 5.6;

	// temperature dependence of maturation and recruitment
	READ_PAR(beta3); // = 0.196;
	READ_PAR(beta4); // = 0.196;

	// Trait dependence of mortality
	READ_PAR(alpha4);
	READ_PAR(alpha1_ref);
	READ_PAR(alpha5);
	READ_PAR(gsi_ref);
	READ_PAR(Mspawning);


	#undef READ_PAR

	gamma1 = 1-beta;
	gamma2 = alpha;
	alpha1 = c;
	alpha2 = k;
	gsi = r/q;
	pmrn_intercept = pmrn_lp50;  // The variable name in params file provided by Mikko is lp50, but the description says "Intercept of the PMRN midpoint curve". So I'm treating this as the intercept - anyway, lp50 is age-dependent
	delta = E1;
	alpha3 = Mref;
	gamma3 = -b;
	

	growth_model_name = I.get<string>("fish", "growth_model_name");
	recruitment_model_name = I.get<string>("fish", "recruitment_model_name");
	mortality_model_name = I.get<string>("fish", "mortality_model_name");
	maturation_model_name = I.get<string>("fish", "maturation_model_name");

	init();	
}


void FishParams::print(){

	#define PRINT_PAR(x) std::cout << #x << " = " << x << "\n"

	PRINT_PAR(beta); // = 0.655793; // 0.648728;
	PRINT_PAR(r); // = 0.090367; // 0.077281;
	PRINT_PAR(c); // = 6.519584; // 6.318308; //6.51559;
	PRINT_PAR(q); // = 1;
	PRINT_PAR(k); // = 0.00674;
	PRINT_PAR(alpha); // = 3.056863227;
	PRINT_PAR(E1); // = 2500;
	PRINT_PAR(pmrn_lp50); // = 148.6918; //118.122779;
	PRINT_PAR(pmrn_width); // = 47.532614;
	PRINT_PAR(pmrn_slope); // = -6.609008;
	PRINT_PAR(pmrn_envelope); // = 0.25;
	PRINT_PAR(Lref); // = 70.48712; //80;
	
	// Power law + offset
	PRINT_PAR(Mref); // = 0.062994; // 0.20775; // ////0.1421; //<--old value from file
	PRINT_PAR(b); //    = 2.455715; // 1.58127; //////1.8131;
	PRINT_PAR(M0); //   = 0.162126; //  0; //

	// Juvenile length and survival probability 
	PRINT_PAR(L0); // = 9.1;
	PRINT_PAR(s0); // = 0.08094733; // 0.02; // 0.09637
	PRINT_PAR(Bhalf); // = 187837572; //3.65e8;  // Bhalf for recruitment 

	// temperature and density dependence of growth
	PRINT_PAR(beta1); // = -7.07e-5;
	PRINT_PAR(beta2); // = 0.178;
	PRINT_PAR(Tmean); // = 5.61;
	PRINT_PAR(tsbmean); // = 1.93e9/1e6; // convert kg to kT
	
	// temperature dependence of mortality
	PRINT_PAR(cT); // = 0.196;
	PRINT_PAR(Tref); // = 5.6;

	PRINT_PAR(gamma1); // = 0.33333;
	PRINT_PAR(gamma2); // = 3.519072971;
	PRINT_PAR(alpha1); // = 3.464594;
	PRINT_PAR(alpha2); // = 0.00156;
	PRINT_PAR(gsi); // = 0.464097;

	// maturation
	PRINT_PAR(pmrn_intercept); // = 18.399575;
	PRINT_PAR(steepness);  // calculated by constructor
	PRINT_PAR(beta3);  // calculated by constructor

	// reproducttion
	PRINT_PAR(delta); // = 1820;
	
	// mortality
	PRINT_PAR(gamma3); // = -1.20565;
	PRINT_PAR(alpha3); // = 0.57792;

	PRINT_PAR(alpha4);
	PRINT_PAR(alpha1_ref);
	PRINT_PAR(alpha5);
	PRINT_PAR(gsi_ref);
	PRINT_PAR(Mspawning);

	PRINT_PAR(beta4);  // calculated by constructor
	PRINT_PAR(verbose);  // calculated by constructor

	cout << "growth_model = " << growth_model_name << " (" << int(growth_model) << ")\n";
	cout << "recruitment_model = " << recruitment_model_name << " (" << int(recruitment_model) << ")\n";
	cout << "mortality_model = " << mortality_model_name << " (" << int(mortality_model) << ")\n";
	cout << "maturation_model = " << maturation_model_name << " (" << int(maturation_model) << ")\n";


	#undef PRINT_PAR	
}



