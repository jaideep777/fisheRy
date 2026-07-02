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
#include "random_utils.h"

RCPP_EXPOSED_CLASS(Fish);
RCPP_EXPOSED_CLASS(FishParams);

RCPP_EXPOSED_ENUM_NODECL(GrowthModel)
RCPP_EXPOSED_ENUM_NODECL(MaturationModel)
RCPP_EXPOSED_ENUM_NODECL(MortalityModel)
RCPP_EXPOSED_ENUM_NODECL(RecruitmentModel)

RCPP_MODULE(fish_module) {
	function("set_rng_seed", &set_rng_seed);
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
		.field("growth_noise_sd", &FishParams::growth_noise_sd)
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
		.field("delta_weight", &Fish::delta_weight)
		.field_readonly("gsi_effective", &Fish::gsi_effective)
		.field_readonly("dl_real", &Fish::dl_real)
		.field_readonly("dl_real_stochastic", &Fish::dl_real_stochastic)
		.field_readonly("t_birth", &Fish::t_birth)
		.field("par", &Fish::par)  // THIS WORKS, even though par is a different copy every time!
		.field("trait_variances", &Fish::trait_variances)
		.field("natural_mort_scalar", &Fish::natural_mort_scalar)

		.method("setMortalityParams", &Fish::setMortalityParams)
		.method("setMortalityCurveEmpirical", &Fish::setMortalityCurveEmpirical)

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


#include "stock.h"
#include "fleet.h"

RCPP_EXPOSED_CLASS(StockParams);
RCPP_EXPOSED_CLASS(SeaEnvironment);

RCPP_EXPOSED_CLASS(FleetParams);
RCPP_EXPOSED_CLASS(Fleet);

// [[Rcpp::export]]
std::vector<double> get_fished_dry_run_wrapper(
    SEXP stock_ptr,          // external pointer to Stock
    Rcpp::List fleets_list,  // list of Fleet external pointers
    const std::vector<double>& x,
    double y,
    bool a,
    bool b
) {
    // Extract module objects properly
    Stock* stock = Rcpp::as<Stock*>(stock_ptr);

    std::vector<Fleet> fleets;
    fleets.reserve(fleets_list.size());

    for (int i = 0; i < fleets_list.size(); ++i) {
        Fleet* fptr = Rcpp::as<Fleet*>(fleets_list[i]);
        fleets.push_back(*fptr);  // copy
    }

    return stock->get_fished_dry_run(fleets, x, y, a, b);
}

////RCPP_EXPOSED_AS(Population);
RCPP_MODULE(population_module){
	// class_ <SeaEnvironment>("SeaEnvironment")
	// 	.constructor()
	// 	.field("temperature", &SeaEnvironment::temperature)
	// 	.field("recruitment_noise_multiplier", &SeaEnvironment::recruitment_noise_multiplier)
	// ;

	class_ <StockParams>("StockParams")
		.constructor()
		.field("recruitmentAge", &StockParams::recruitmentAge)
		.field("rmax", &StockParams::rmax)
	;

	// FIXME: Makse sure ALL FUNCTIONS THAT MODIFTY POPULATION ARE NOW EXPOSED VIA FISHERY CLASS
	// ------------------------------------------------
	class_ <Stock>("Stock")
		.constructor<Fish>()
		.field("par", &Stock::par)
		.field("superfish_size", &Stock::superfish_size)
		.field("debug", &Stock::debug)
		
		.method("readParams", &Stock::readParams)
		.method("init", &Stock::init)
		.method("calcSSB", &Stock::calcSSB)
		.method("calcTSB", &Stock::calcTSB)
		
		.method("nfish", &Stock::nfish)
		.method("get_state", &Stock::get_state)
		.method("get_traits", &Stock::get_traits)

		.method("equilibriate_without_fishing", &Stock::equilibriate_without_fishing)

		// .method("get_fished_dry_run", &Stock::get_fished_dry_run)
		// .method("get_fished", &Stock::get_fished)
	;

	function("get_fished_dry_run_wrapper", &get_fished_dry_run_wrapper);
}


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
		.field("debug", &Fleet::debug)

		.method("readParams", &Fleet::readParams) 

		.method("init_chi", &Fleet::init_chi) // modifies state: consider unexposing

		.method("set_referenceFishingMortalityCurveLogistic", &Fleet::set_referenceFishingMortalityCurveLogistic)
		.method("set_referenceFishingMortalityCurveEmpirical", &Fleet::set_referenceFishingMortalityCurveEmpirical)
		// .method("set_minSizeLimit", &Fleet::set_minSizeLimit)
		// .method("set_harvestProportion", &Fleet::set_harvestProportion)

		.method("fishability", &Fleet::fishability)
		.method("fishingMortalityRef", &Fleet::fishingMortalityRef)
		.method("biomassFishable", &Fleet::biomassFishable)
		.method("fishingMortality", &Fleet::fishingMortality)
		.method("update_chi", &Fleet::update_chi)
		// .method("harvest_dry_run", &Fleet::harvest_dry_run)
		// .method("harvest", &Fleet::harvest)
		.method("effort_constantC", &Fleet::effort_constantC)
		.method("effort_constantF", &Fleet::effort_constantF)
	;
}


#include "fishery_system.h"
// #include "simulator.h"

RCPP_EXPOSED_CLASS(Stock);
RCPP_EXPOSED_CLASS(FisheryParams);

RCPP_MODULE(simulator_module){
	class_ <FisheryParams>("FisheryParams")
		.constructor()
		.field("rho", &FisheryParams::rho)
		.field("f_spf_before", &FisheryParams::f_spf_before)
		.method("print", &FisheryParams::print)
	;

	class_ <Fishery>("Fishery")
		.constructor<std::string, Fish>()
		.field("par", &Fishery::par)
		.field("pop", &Fishery::pop) // Use updated getter and setter
		.field("debug", &Fishery::debug)
		.field("colnames", &Fishery::colnames)

		// Wrappers for population functions exposed from Fishery because they modify population state
		.method("set_superFishSize", &Fishery::set_superFishSize)
		.method("readEnvironmentFile", &Fishery::readEnvironmentFile)
		.method("updateEnv", &Fishery::updateEnv)

		// core Fishery functions
		.method("addFleet", &Fishery::addFleet)
		.method("addSpawnerFleet", &Fishery::addSpawnerFleet)
		.method("calc_quota", &Fishery::calc_quota)
		.method("equilibriateNaturalPopulation", &Fishery::equilibriateNaturalPopulation)
		.method("init", &Fishery::init)
		.method("equilibriateWithoutFishing", &Fishery::equilibriateWithoutFishing)
		.method("readParams", &Fishery::readParams)
		.method("get_fref", &Fishery::get_fref)

		.method("set_debug", &Fishery::set_debug)
		
		.method("set_harvestProp", &Fishery::set_harvestProp)
		.method("set_minSizeLimit", &Fishery::set_minSizeLimit)
		// .method("set_traitVariances", &Fishery::set_traitVariances)

		.method("update", &Fishery::update)

		.method("simulate", &Fishery::simulate_r)
		.method("simulate_multi", &Fishery::simulate_multi_r)

		// .method("max_avg_utils", &Fishery::max_avg_utils)
		// .method("stakeholder_satisfaction", &Fishery::stakeholder_satisfaction)
		// .method("stakeholder_satisfaction_t", &Fishery::stakeholder_satisfaction_t)
		// .method("get_profit_mask", &Fishery::get_profit_mask)
		
	;

	// class_ <Simulator>("Simulator")
	// 	.constructor<Fish>()

	// 	.field_readonly("noFishingPop", &Simulator::noFishingPop)

	// 	.method("setNaturalPopulation", &Simulator::setNaturalPopulation)
	// 	.method("equilibriateNaturalPopulation", &Simulator::equilibriateNaturalPopulation)
		
	// 	.method("simulate", &Simulator::simulate_r)
		
	// 	// .method("simulate_multi", &Simulator::simulate_multi_r)
	// 	// .method("max_avg_utils", &Simulator::max_avg_utils)
	// 	// .method("stakeholder_satisfaction", &Simulator::stakeholder_satisfaction)
		
	// 	.method("simulate_multi_2d", &Simulator::simulate_multi_2d_r)
	// 	.method("max_avg_utils_2d", &Simulator::max_avg_utils_2d)
	// 	.method("stakeholder_satisfaction_2d", &Simulator::stakeholder_satisfaction_2d)

	// 	.method("stakeholder_satisfaction_2d_t", &Simulator::stakeholder_satisfaction_2d_t)
	// ;
}




