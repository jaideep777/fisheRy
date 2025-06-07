//   ====== RCPP BLACKLISTED METHOD NAMES ======
// 
//   Avoid using these names for any method exposed via RCPP_MODULE:
// 
//   - initialize
//   - finalize
//   - show
//   - call
//   - dim
//   - length
//   - names
//   - levels
//   - plot
//   - print
//   - summary
//   - mean
//   - median
//   - quantile
//   - any other common S4 or base generic
// 
//   Why?
//   These names are intercepted by R's S4 / S3 method dispatch system.
//   Your methods will either:
//     - not show up at all
//     - be silently ignored
//     - be replaced by base R behavior
//     - give mysterious errors when calling from R
//     
//   Recommendation:
//     - Always use custom or non-conflicting names.
//     - E.g., use "init" instead of "initialize", "print_line" instead of "print".
//    
//
//   Source:
//     Rcpp FAQ + StackOverflow + blood, sweat, and tears.

//   ====== R OBJECT COPYING GOTCHAS ======
//
//   When accessing nested objects in R (e.g., fishery$population$method()), 
//   R creates temporary copies. Any modifications made through such calls
//   will be lost because they modify the temporary copy, not the original object.
//
//   Example of the problem:
//     fishery$population$modifier()  # Modifies a temporary copy
//     print(fishery$population)      # Original remains unchanged
//
//   To avoid this:
//   1. Don't expose methods that modify state through nested objects
//   2. Expose modification methods at the top level (e.g., fishery$modifyPopulation())
//   3. If you must modify nested objects in R, use intermediate assignment:
//      pop <- fishery$population
//      pop$modifier()
//      fishery$population <- pop
//
//   However, this behaviour appears to be limited to functions. Directly setting nested 
//   object members appears to work. E.g.,
//
//     fishery$pop$par$n <- new_n  # works
// 

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
	// function("natural_mortality", &fish::natural_mortality);
	// function("survival_probability", &fish::survival_probability);
	// function("fishing_selectivity", &fish::fishing_selectivity);
	
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
		.field("Mspawning", &FishParams::Mspawning)

		.field("alpha1", &FishParams::alpha1)
		.field("gsi", &FishParams::gsi)
		.field("pmrn_intercept", &FishParams::pmrn_intercept)
		.field("pmrn_slope", &FishParams::pmrn_slope)
		.field("pmrn_width", &FishParams::pmrn_width)
		
		.method("print", &FishParams::print)
		.method("initFromFile", &FishParams::initFromFile)	
	;

	class_ <Fish>("Fish")
		.constructor<std::string>()
		
		.field("age", &Fish::age)
		.field("length", &Fish::length)
		.field("weight", &Fish::weight)
		.field_readonly("t_birth", &Fish::t_birth)
		.field("par", &Fish::par)  // THIS WORKS, even though par is a different copy every time!
		.field("trait_variances", &Fish::trait_variances)

		.method("setMortalityParams", &Fish::setMortalityParams)

		.method("print", &Fish::print)
		.method("print_line", &Fish::print_line)
		.method("print_header", &Fish::print_header)

		.method("set_age", &Fish::set_age)           // consider unexposing: modifies state
		.method("set_length", &Fish::set_length)     // consider unexposing: modifies state
		.method("set_traits", &Fish::set_traits)     // consider unexposing: modifies state

		.method("init", &Fish::init)                 // consider unexposing: modifies state
		//.method("matureNow", &Fish::matureNow)
		.method("maturationProb", &Fish::maturationProb)
		.method("updateMaturity", &Fish::updateMaturity)  // consider unexposing: modifies state
		.method("grow", &Fish::grow)                 // consider unexposing: modifies state
		.method("produceRecruits", &Fish::produceRecruits)
		
		.method("naturalMortalityRate", &Fish::naturalMortalityRate)
		//.method("survivalProbability", &Fish::survivalProbability)

		.method("get_state", &Fish::get_state)


	;
}	


#include "population.h"
#include "stock.h"

RCPP_EXPOSED_CLASS(PopulationParams);
RCPP_EXPOSED_CLASS(StockParams);
RCPP_EXPOSED_CLASS(SeaEnvironment);

////RCPP_EXPOSED_AS(Population);
RCPP_MODULE(population_module){
	class_ <SeaEnvironment>("SeaEnvironment")
		.constructor()
		.field("temperature", &SeaEnvironment::temperature)
		.field("recruitment_noise_multiplier", &SeaEnvironment::recruitment_noise_multiplier)
	;

	class_ <PopulationParams>("PopulationParams")
		.constructor()
		.field("n", &PopulationParams::n)
		.field("rmax", &PopulationParams::rmax)
		.field_readonly("h", &PopulationParams::h)
		.field_readonly("Fc", &PopulationParams::Fc)
		.field_readonly("lf50", &PopulationParams::F3)
		.field_readonly("lmin", &PopulationParams::lmin)
		.field_readonly("lmin_sq", &PopulationParams::lmin_sq)
		.field_readonly("F1", &PopulationParams::F1)
		.field_readonly("F2", &PopulationParams::F2)
		.field_readonly("F3", &PopulationParams::F3)
		.field_readonly("F4", &PopulationParams::F4)
		.field_readonly("F5", &PopulationParams::F5)
		.field_readonly("F6", &PopulationParams::F6)
		.field_readonly("F5_sq", &PopulationParams::F5_sq)
//		.field("mort_fishing_mature", &PopulationParams::mort_fishing_mature) 
//		.field("mort_fishing_immature", &PopulationParams::mort_fishing_immature) 
		// .field("F_spf", &PopulationParams::F_spf)
		.field("f_spf_before", &PopulationParams::f_spf_before)
		.field("dsea", &PopulationParams::dsea)
		.field("dmax", &PopulationParams::dmax)
		.field("recruitmentAge", &PopulationParams::recruitmentAge)
		.field("rho", &PopulationParams::rho)

		.field("update_env", &PopulationParams::update_env)
		.field("simulate_bio_only", &PopulationParams::simulate_bio_only)

		.method("initFromFile", &PopulationParams::initFromFile)
		.method("print", &PopulationParams::print)
	;
	
	class_ <Population>("Population")
		.constructor<Fish>()
		.field("par", &Population::par)
		.field("env", &Population::env)
		.field("verbose", &Population::verbose)
		.field("K_fishableBiomass", &Population::K_fishableBiomass)
		.field("K_ssb", &Population::K_ssb)
		.field("colnames", &Population::colnames)
		.field("current_year", &Population::current_year)

		// ALL FUNCTIONS THAT MODIFTY POPULATION ARE NOW EXPOSED VIA FISHERY CLASS
		// ------------------------------------------------
		// These are retained for transitioning/debugging purposes (DO NOT USE in production code)
		.method("readParams", &Population::readParams) 
		.method("set_superFishSize", &Population::set_superFishSize) 
		.method("set_traitVariances", &Population::set_traitVariances) 
		.method("set_harvestProp", &Population::set_harvestProp) 
		.method("set_minSizeLimit", &Population::set_minSizeLimit) 
		.method("init", &Population::init) 
		.method("update", &Population::update)
		.method("noFishingEquilibriate", &Population::noFishingEquilibriate)
		.method("summarize", &Population::summarize)
		.method("readEnvironmentFile", &Population::readEnvironmentFile)
		.method("updateEnv", &Population::updateEnv)
		// -----------------------------------------------

		.method("calcSSB", &Population::calcSSB)
		.method("fishableBiomass", &Population::fishableBiomass)
		.method("fishingMortalityRef", &Population::fishingMortalityRef)
		.method("fishingMortRefByAge", &Population::fishingMortRefByAge)
		.method("maturityByAge", &Population::maturityByAge)
		.method("naturalMortByAge", &Population::naturalMortByAge)
		.method("avgOverAges", &Population::avgOverAges)

		.method("fishingMortRefByAge", &Population::fishingMortRefByAge)
		.method("maturityByAge", &Population::maturityByAge)
		.method("naturalMortByAge", &Population::naturalMortByAge)
		.method("avgOverAges", &Population::avgOverAges)

		.method("get_state", &Population::get_state)
		.method("get_traits", &Population::get_traits)
		.method("print_summary", &Population::print_summary)
		.method("nfish", &Population::nfish)
	;
	
	class_ <StockParams>("StockParams")
		.constructor()
		.field("recruitmentAge", &StockParams::recruitmentAge)
		.field("rmax", &StockParams::rmax)
	;

	class_ <Stock>("Stock")
		.constructor<Fish>()
		.field("par", &Stock::par)
		.field("superfish_size", &Stock::superfish_size)
		
		.method("readParams", &Stock::readParams)
		.method("init", &Stock::init)
		.method("calcSSB", &Stock::calcSSB)
		.method("calcTSB", &Stock::calcTSB)
		
		.method("nfish", &Stock::nfish)
		.method("get_state", &Stock::get_state)
		.method("get_traits", &Stock::get_traits)

		.method("equilibriate_without_fishing", &Stock::equilibriate_without_fishing)
		;
}

#include "fleet.h"

RCPP_EXPOSED_CLASS(FleetParams);
RCPP_EXPOSED_CLASS(Fleet);

RCPP_MODULE(fleet_module){
	class_ <FleetParams>("FleetParams")
		.constructor()
		.method("initFromFile", &FleetParams::initFromFile) // Expose initFromFile
		.method("print", &FleetParams::print)              // Expose print

		.field("chi0_scalar_slope", &FleetParams::chi0_scalar_slope)
		.field("control_model", &FleetParams::control_model)
		.field("dsea", &FleetParams::dsea)
	;

	class_ <Fleet>("Fleet")
		.constructor()
		.field("chi", &Fleet::chi)
		.field("par", &Fleet::par)

		.method("readParams", &Fleet::readParams) 

		.method("init_chi", &Fleet::init_chi) // modifies state: consider unexposing

		.method("set_minSizeLimit", &Fleet::set_minSizeLimit)
		// .method("set_harvestProportion", &Fleet::set_harvestProportion)

		.method("fishability", &Fleet::fishability)
		.method("fishingMortalityRef", &Fleet::fishingMortalityRef)
		.method("fishingMortality", &Fleet::fishingMortality)
		.method("update_chi", &Fleet::update_chi)
		.method("harvest_dry_run", &Fleet::harvest_dry_run)
		.method("harvest", &Fleet::harvest)
		.method("effort_constantC", &Fleet::effort_constantC)
		.method("effort_constantF", &Fleet::effort_constantF)
	;
}


// #include "fishery_system.h"
#include "simulator.h"

RCPP_EXPOSED_CLASS(Population);
RCPP_EXPOSED_CLASS(Stock);
// RCPP_EXPOSED_CLASS(FisheryParams);

RCPP_MODULE(simulator_module){
    // class_ <FisheryParams>("FisheryParams")
    //     .constructor()
    //     .field("rho", &FisheryParams::rho)
    //     .field("f_spf_before", &FisheryParams::f_spf_before)
    //     .method("print", &FisheryParams::print)
    // ;

    // class_ <Fishery>("Fishery")
    //     .constructor<std::string, Fish>()
    //     .field("par", &Fishery::par)
    //     .field("pop", &Fishery::pop) // Use updated getter and setter

    //     // Wrappers for population functions exposed from Fishery because they modify population state
    //     .method("equilibriateNaturalPopulation", &Fishery::equilibriateNaturalPopulation)
    //     .method("init", &Fishery::init)
    //     .method("readParams", &Fishery::readParams)
    //     .method("set_superFishSize", &Fishery::set_superFishSize)
    //     .method("readEnvironmentFile", &Fishery::readEnvironmentFile)
    //     .method("updateEnv", &Fishery::updateEnv)
    //     .method("set_harvestProp", &Fishery::set_harvestProp)
    //     .method("set_minSizeLimit", &Fishery::set_minSizeLimit)
    //     .method("set_traitVariances", &Fishery::set_traitVariances)
    //     .method("noFishingEquilibriate", &Fishery::noFishingEquilibriate)

    //     .method("update", &Fishery::update)
    // ;

    class_ <Simulator>("Simulator")
        .constructor<Fish>()

        .field_readonly("noFishingPop", &Simulator::noFishingPop)

        .method("setNaturalPopulation", &Simulator::setNaturalPopulation)
        .method("equilibriateNaturalPopulation", &Simulator::equilibriateNaturalPopulation)
        
        .method("simulate", &Simulator::simulate_r)
        
        // .method("simulate_multi", &Simulator::simulate_multi_r)
        // .method("max_avg_utils", &Simulator::max_avg_utils)
        // .method("stakeholder_satisfaction", &Simulator::stakeholder_satisfaction)
        
        .method("simulate_multi_2d", &Simulator::simulate_multi_2d_r)
        .method("max_avg_utils_2d", &Simulator::max_avg_utils_2d)
        .method("stakeholder_satisfaction_2d", &Simulator::stakeholder_satisfaction_2d)

        .method("stakeholder_satisfaction_2d_t", &Simulator::stakeholder_satisfaction_2d_t)
    ;
}




