#include <Rcpp.h>
using namespace Rcpp;

#include "fish.h"
#include "functions.h"

RCPP_EXPOSED_CLASS(Fish);
RCPP_EXPOSED_CLASS(FishParams);

RCPP_EXPOSED_ENUM_NODECL(GrowthModel)
RCPP_EXPOSED_ENUM_NODECL(MaturationModel)
RCPP_EXPOSED_ENUM_NODECL(MortalityModel)
RCPP_EXPOSED_ENUM_NODECL(RecruitmentModel)

RCPP_MODULE(fish_module) {
	
//	function("init_length", &fish::init_length);
	function("length_juvenile", &fish::length_juvenile);
	function("length_adult", &fish::length_adult);
	function("maturation_steepness", &fish::maturation_steepness);
	function("maturation_probability", &fish::maturation_probability);
	function("weight_fish", &fish::weight_fish);
	function("fecundity", &fish::fecundity);
	function("gsi", &fish::gsi);
	function("natural_mortality", &fish::natural_mortality);
	function("survival_probability", &fish::survival_probability);
	function("fishing_selectivity", &fish::fishing_selectivity);
	
	class_ <FishParams>("FishParams")
		.constructor()
		.field("flag", &FishParams::flag)
//		.field("Bhalf_growth", &FishParams::Bhalf_growth)
		.field("c", &FishParams::c)
		.field("beta1", &FishParams::beta1)
		.field("beta2", &FishParams::beta2)
		.field("s0", &FishParams::s0)
		.field("Bhalf", &FishParams::Bhalf)
		.field("pmrn_lp50", &FishParams::pmrn_lp50)
		.field("growth_model_name", &FishParams::growth_model_name)
		.field("maturation_model_name", &FishParams::maturation_model_name)
		.field("mortality_model_name", &FishParams::mortality_model_name)
		.field("recruitment_model_name", &FishParams::recruitment_model_name)
		.field("r0", &FishParams::r0)
		.field("M0", &FishParams::M0)

		.field("alpha1", &FishParams::alpha1)
		.field("gsi", &FishParams::gsi)
		.field("pmrn_intercept", &FishParams::pmrn_intercept)
		.field("pmrn_slope", &FishParams::pmrn_slope)
		.field("pmrn_width", &FishParams::pmrn_width)
		
		.method("print", &FishParams::print)
		.method("initFromFile", &FishParams::initFromFile)	
	;

	class_ <Fish>("Fish")
		.constructor()
		.constructor<std::string>()
		
		.field("age", &Fish::age)
		.field("length", &Fish::length)
		.field("weight", &Fish::weight)
		.field_readonly("t_birth", &Fish::t_birth)
		.field("par", &Fish::par)
		.field("trait_variances", &Fish::trait_variances)

		.method("print", &Fish::print)
		.method("print_line", &Fish::print_line)
		.method("print_header", &Fish::print_header)

		.method("set_age", &Fish::set_age)
		.method("set_length", &Fish::set_length)

		.method("init", &Fish::init)
		//.method("matureNow", &Fish::matureNow)
		.method("maturationProb", &Fish::maturationProb)
		.method("updateMaturity", &Fish::updateMaturity)
		.method("grow", &Fish::grow)
		.method("produceRecruits", &Fish::produceRecruits)
		
		.method("naturalMortalityRate", &Fish::naturalMortalityRate)
		//.method("survivalProbability", &Fish::survivalProbability)

		.method("get_state", &Fish::get_state)


	;
}	
	
#include "population.h"

RCPP_EXPOSED_CLASS(PopulationParams);
RCPP_EXPOSED_CLASS(SeaEnvironment);


////RCPP_EXPOSED_AS(Population);
RCPP_MODULE(population_module){
	class_ <SeaEnvironment>("SeaEnvironment")
		.field("temperature", &SeaEnvironment::temperature)
		.field("recruitment_noise_multiplier", &SeaEnvironment::recruitment_noise_multiplier)
	;

	class_ <PopulationParams>("PopulationParams")
		.constructor()
		.field("n", &PopulationParams::n)
		.field_readonly("h", &PopulationParams::h)
		.field_readonly("lf50", &PopulationParams::lf50)
//		.field("mort_fishing_mature", &PopulationParams::mort_fishing_mature) 
//		.field("mort_fishing_immature", &PopulationParams::mort_fishing_immature) 
		.field("dsea", &PopulationParams::dsea)
		.field("dmax", &PopulationParams::dmax)
		.field("recruitmentAge", &PopulationParams::recruitmentAge)
		.field("update_env", &PopulationParams::update_env)
		.field("simulate_bio_only", &PopulationParams::simulate_bio_only)
	;
	
	class_ <Population>("Population")
		.constructor<Fish>()
		.field("par", &Population::par)
		.field("env", &Population::env)
		.field("verbose", &Population::verbose)
		.field("K", &Population::K_fishableBiomass)
		.field("colnames", &Population::colnames)

		.method("set_superFishSize", &Population::set_superFishSize) 
		
		.method("set_harvestProp", &Population::set_harvestProp) 
		.method("set_minSizeLimit", &Population::set_minSizeLimit) 

		.method("selectivity", &Population::selectivity) 
		.method("init", &Population::init) 
		.method("update", &Population::update)
		.method("calcSSB", &Population::calcSSB)
		.method("fishableBiomass", &Population::fishableBiomass)

		.method("noFishingEquilibriate", &Population::noFishingEquilibriate)

		.method("get_state", &Population::get_state)
		.method("get_traits", &Population::get_traits)
		.method("summarize", &Population::summarize)
		.method("print_summary", &Population::print_summary)
		.method("nfish", &Population::nfish)

		.method("readEnvironmentFile", &Population::readEnvironmentFile)
		.method("updateEnv", &Population::updateEnv)
	;
}


#include "simulator.h"
RCPP_EXPOSED_CLASS(Population);

RCPP_MODULE(simulator_module){
	class_ <Simulator>("Simulator")
	.constructor<Fish>()

	//.field("noFishingPop", &Simulator::noFishingPop)

	.method("setNaturalPopulation", &Simulator::setNaturalPopulation)
	.method("equilibriateNaturalPopulation", &Simulator::equilibriateNaturalPopulation)
	
	.method("simulate", &Simulator::simulate_r)
    
	.method("simulate_multi", &Simulator::simulate_multi_r)
    .method("max_avg_utils", &Simulator::max_avg_utils)
    .method("stakeholder_satisfaction", &Simulator::stakeholder_satisfaction)
	
	.method("simulate_multi_2d", &Simulator::simulate_multi_2d_r)
    .method("max_avg_utils_2d", &Simulator::max_avg_utils_2d)
    .method("stakeholder_satisfaction_2d", &Simulator::stakeholder_satisfaction_2d)
	;
}




