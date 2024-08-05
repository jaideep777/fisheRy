#include "simulator.h"
#include <algorithm>
#include <chrono> // microseconds
#include <thread> // sleep_for
using namespace std;

Simulator::Simulator(Fish f) : noFishingPop(f) {
}

void Simulator::setNaturalPopulation(const Population &pop){
	noFishingPop = pop;
}

vector<double> Simulator::equilibriateNaturalPopulation(std::string params_file, double temp, double _n){
	noFishingPop.readParams(params_file);
	noFishingPop.set_superFishSize(_n);
	noFishingPop.set_traitVariances({0,0,0,0,0,0});
	return noFishingPop.noFishingEquilibriate(temp);
}


Tensor<double> Simulator::simulate_multi_2d(Population pop, vector<double> Tvec, vector<double> lminvec, vector<double> hvec, int nyears, double tsb0, bool re_init){
	int niters = 1;
	Tensor<double> res({niters, static_cast<int>(pop.colnames.size()), static_cast<int>(Tvec.size()), static_cast<int>(lminvec.size()), static_cast<int>(hvec.size()), nyears});
	Population pop_ref = pop;

	for (int iter = 0; iter < niters; ++iter){  // loop over iterations
	for (int it=0; it<Tvec.size(); ++it){       // loop over temperature
	for (int il=0; il<lminvec.size(); ++il){    // loop over control parameter 2
	for (int ih=0; ih<hvec.size(); ++ih){       // loop over control parameter 1
		pop = pop_ref;
		// if (hvec[ih] > 0.5) pop.set_superFishSize(1e0);
		
		noFishingPop.set_harvestProp(hvec[ih]);
		noFishingPop.set_minSizeLimit(lminvec[il]);
		double K_fishable = noFishingPop.fishableBiomass();
		double K_ssb      = noFishingPop.calcSSB();
		cout << "h = " << hvec[ih] << ", L50 = " << noFishingPop.par.F3 << ", T = " << Tvec[it] << ", n = " << pop.par.n << " | K_fishable = " << K_fishable << ", K_ssb = " << K_ssb << endl;

		pop.K_fishableBiomass = K_fishable;
		pop.K_ssb = K_ssb;

		pop.set_harvestProp(hvec[ih]);
		pop.set_minSizeLimit(lminvec[il]);

		if (re_init) pop.init(1000, Tvec[it]);
		// pop.print_summary();
	
		for (int t=0; t<nyears; ++t){
			double Tnow;
			if (pop.par.update_env){
				// cout << "t = " << t << "pop.current_year = " << pop.current_year;
				pop.updateEnv(pop.current_year);
				Tnow = pop.env.temperature;
				// cout << " | env.t = " << pop.env.year << ", T = " << pop.env.temperature << "\n";
			}
			else{
				Tnow = Tvec[it];
			}

			std::vector<double> state_now = pop.update(Tnow);
			
			for (int col=0; col<state_now.size(); ++col){
				res({iter, col, it, il, ih, t}) = state_now[col];
			}
			// res({iter, 0, il, ih, t}) = state_now[0];  // ssb
			// res({iter, 1, il, ih, t}) = state_now[1];  // yield
			// res({iter, 2, il, ih, t}) = state_now[2];  // employment sea
			// res({iter, 3, il, ih, t}) = state_now[3];  // employment shore
			// res({iter, 4, il, ih, t}) = state_now[4];  // profit sea
			// res({iter, 5, il, ih, t}) = state_now[5];  // profit shore
		}
		}
		}
		}
	}
	//res.print();
	
	return res.avg_dim(5);	// average over iterations

}


vector<double> Simulator::max_avg_utils_2d(vector<int> dims, vector<double> data){
	Tensor<double> res(dims);
	res.vec = data;		// res is {u, T, c2, c1, t}

	Tensor<double> res2 = res.avg_dim(0).max_dim(0).max_dim(0).max_dim(0);	// avg over t, then max over c1, then max over c2, then max over T
	res.transform(4, std::divides<double>(), res2.vec); // divide u dimension by res2

	return res.avg_dim(0).vec; // average over t
}


vector<double> Simulator::stakeholder_satisfaction_2d(vector<int> dims, vector<double> data){
	Tensor<double> res(dims);
	res.vec = data;		// res is {u, T, c2, c1, t}

	Tensor<double> res2 = res.avg_dim(0).max_dim(0).max_dim(0).max_dim(0);	// avg over t, then max over c1, then max over c2, then max over T
	res.transform(4, std::divides<double>(), res2.vec); // divide u dimension by res2

	Tensor<double> sp({5,4});	// spvec is {s, u}
	//        ssb yield emp  profit  
	sp.vec = {0.0, 0.3, 0.0, 0.7,	// industrial
			  0.3, 0.5, 0.1, 0.1,	// artisanal
			  0.3, 0.2, 0.5, 0.0,	// employment-maximizing policymakers
			  0.2, 0.2, 0.0, 0.6,	// profit-maximizing policymakers
			  0.5, 0.1, 0.2, 0.2	// conservationists
			 };

	sp.print();

	Tensor<double> Ssucy = sp.repeat_inner(res.dim[1]).repeat_inner(res.dim[2]).repeat_inner(res.dim[3]).repeat_inner(res.dim[4]) * res.repeat_outer(sp.dim[0]);
	//                        ^ {s, u, T}             ^ {s, u, T, c2}           ^ {s, u, T, c2, c1}      ^ {s, u, T, c2, c1, y}         ^ {s, u, T, c2, c1, y}

	Tensor<double> Sscy = Ssucy.accumulate(0.0, 4, std::plus<double>());	// aggregate along u dim to get {s, T, c2, c1, y}

	Tensor<double> Ssc = Sscy.avg_dim(0);
	//                        ^ {s, T, c2, c1}
	Ssc.transform(3, std::divides<double>(), Ssc.max_dim(0).max_dim(0).max_dim(0).vec);
	//			  ^ s                            ^ {s,T,c2} ^ {s,T}    ^ {s}
	
	return Ssc.vec;

}


vector<double> Simulator::stakeholder_satisfaction_2d_t(vector<int> dims, vector<double> data){
	Tensor<double> res(dims);
	res.vec = data;		// res is {u, T, c2, c1, t}

	// here t is also treated as a control parameter
	Tensor<double> res2 = res.max_dim(0).max_dim(0).max_dim(0).max_dim(0);	// max over t, then max over c1, then max over c2, then max over T
	res.transform(4, std::divides<double>(), res2.vec); // divide u dimension by u_max vector

	Tensor<double> sp({5,4});	// spvec is {s, u}
	//        ssb yield emp  profit  
	sp.vec = {0.0, 0.3, 0.0, 0.7,	// industrial
			  0.3, 0.5, 0.1, 0.1,	// artisanal
			  0.3, 0.2, 0.5, 0.0,	// employment-maximizing policymakers
			  0.2, 0.2, 0.0, 0.6,	// profit-maximizing policymakers
			  0.5, 0.1, 0.2, 0.2	// conservationists
			 };

	sp.print();

	// stakeholder preferences {s,u} repeated to get same dim as res, then multiplied with utilities to get  {s,u}*u
	Tensor<double> Ssucy = sp.repeat_inner(res.dim[1]).repeat_inner(res.dim[2]).repeat_inner(res.dim[3]).repeat_inner(res.dim[4]) * res.repeat_outer(sp.dim[0]);
	//                        ^ {s, u, T}             ^ {s, u, T, c2}           ^ {s, u, T, c2, c1}      ^ {s, u, T, c2, c1, y}         ^ {s, u, T, c2, c1, y}

	// aggregate along u dim to get {s, T, c2, c1, y}
	Tensor<double> Sscy = Ssucy.accumulate(0.0, 4, std::plus<double>());	

	// no time average, since we need time-explicit JSS
	//Tensor<double> Ssc = Sscy.avg_dim(0);
	//                        ^ {s, T, c2, c1}
	
	Sscy.transform(4, std::divides<double>(), Sscy.max_dim(0).max_dim(0).max_dim(0).max_dim(0).vec);
	//			  ^ s                            ^ {s,T,c2,c1} ^ {s,T,c2}  ^ {s,T}   ^ {s}
	
	return Sscy.vec;

}


// ************ R stuff *****************
#ifndef NATIVE_CPP

Rcpp::DataFrame Simulator::simulate_r(Population &pop, double lf, double h, int nyears, double tsb0, double temp, bool re_init, std::string output_file){
	bool writestate = (output_file != "");

	ofstream fout;
	if (writestate){
		fout.open(output_file.c_str());
		fout << "Year" << ',' 
		     << "age" << ',' 
			 << "N" << ',' 
			 << "weight" << ',' 
			 << "mat" << ',' 
			 << "catch_N" << ',' 
			 << "catch_weight"
			 << '\n';

	}

	noFishingPop.set_harvestProp(h);
	noFishingPop.set_minSizeLimit(lf);
	double K = noFishingPop.fishableBiomass();
	cout << "h/lf = " << h << " / " << lf << " | K = " << K << endl;

	pop.K_fishableBiomass = K;
	pop.set_harvestProp(h);
	pop.set_minSizeLimit(lf);
	if (re_init) pop.init(1000, temp);

	vector<string> colnames = pop.colnames;

	vector<vector<double>> columns(colnames.size());
	for (auto& vec : columns) vec.reserve(nyears);
	
	Rcpp::DataFrame df = Rcpp::DataFrame::create();

	for (int i=0; i<nyears; ++i){
		std::vector<double> state_now = pop.update(temp);
		
		for (int col=0; col<state_now.size(); ++col){
			columns[col].push_back(state_now[col]);
		}

		// write age-wise summaries to file
		if (writestate){
			for (int a=0; a < pop.pop_summary.n_a.size(); ++a){
				fout << i << ',' 
					 << a << ',' 
					 << pop.pop_summary.n_a[a] << ',' 
					 << pop.pop_summary.w_a[a] << ',' 
					 << pop.pop_summary.mat_a[a] << ',' 
					 << pop.pop_summary.nc_a[a] << ',' 
					 << pop.pop_summary.wc_a[a]
					 << '\n';
			}
		}

	}

	// put summarized population state in dataframe
	for (int i=0; i<columns.size(); ++i){	
		if (verbose) cout << "Adding columns[" << i << "] = " << colnames[i] << endl; 
		df.push_back(columns[i], colnames[i]);
	}

	if (writestate) fout.close();

	return df;
}


Rcpp::NumericVector tensor2array(Tensor<double>& v){
	Rcpp::NumericVector out(v.vec.begin(), v.vec.end()); 
	vector<int> dims = v.dim;
	std::reverse(dims.begin(), dims.end());
	out.attr("dim") = dims;
	return out;
}

// Rcpp::NumericVector Simulator::simulate_multi_r(Population &pop, vector<double> hvec, int nyears, double tsb0, double temp, bool re_init){
// 	Tensor<double> res = simulate_multi(pop, hvec, nyears, tsb0, temp, re_init);
// 	return tensor2array(res);
// }

Rcpp::NumericVector Simulator::simulate_multi_2d_r(Population pop, vector<double> Tvec, vector<double> lminvec, vector<double> hvec, int nyears, double tsb0, bool re_init){
	Tensor<double> res = simulate_multi_2d(pop, Tvec, lminvec, hvec, nyears, tsb0, re_init);
	return tensor2array(res);
}

#endif
