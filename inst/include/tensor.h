#ifndef FISHERY_TENSOR_H_
#define FISHERY_TENSOR_H_

#include <iostream>
#include <cassert>
#include <vector>
#include <functional>
#include <algorithm>

#include <numeric>

// Multidimensional vector with
// dimensions [..., N2, N1, N0 ] 
// indexed as [..., i2, i1, i0 ]
// i0 is the lowest dimension (elements along i0 are stored consequtively in memory.
// 
// Example, an array with dimensions [2,3,5] is stored as:
//
// Tensor:
//   axis   2 1 0
//   dims = 2 3 5 
//   vals = 
//	  +-----+--+--+--+--+------> axis 0
//    |+    0  1  2  3  4 
//    | \   5  6  7  8  9 
//    |  \ 10 11 12 13 14 
//    v   \ 
//  axis 1 +     15 16 17 18 19 
//          \    20 21 22 23 24 
//           \   25 26 27 28 29 
//     axis 2 V
//
// i.e.. the indices of each element are      
// loc: index
//	 0: 0 0 0
//	 1: 0 0 1 
//	 2: 0 0 2 
//	 3: 0 0 3 
//	 4: 0 0 4 
//	 5: 0 1 0 
//	 6: 0 1 1 
//	 7: 0 1 2 
//	 8: 0 1 3 
//	 9: 0 1 4 
//	10: 0 2 0 
//	11: 0 2 1 
//	12: 0 2 2 
//	13: 0 2 3 
//	14: 0 2 4 
//	15: 1 0 0 
//	16: 1 0 1 
//	17: 1 0 2 
//	18: 1 0 3 
//	19: 1 0 4 
//	20: 1 1 0 
//	21: 1 1 1 
//	22: 1 1 2 
//	23: 1 1 3 
//	24: 1 1 4 
//	25: 1 2 0 
//	26: 1 2 1 
//	27: 1 2 2 
//	28: 1 2 3 
//	29: 1 2 4  

template <class T>
class Tensor{
	private:
	std::vector<int> offsets;
	int nelem;
	
	public:
	std::vector<int> dim;
	std::vector<T> vec;

	Tensor(std::vector<int> _dim){
		dim = _dim;
		nelem = std::accumulate(dim.begin(), dim.end(), 1, std::multiplies<int>());
		vec.resize(nelem);

		int ndim = dim.size();
		offsets.resize(ndim,0);
		int p = 1;
		for (int i=ndim-1; i>=0; --i){
			offsets[i] = p;
			p *= dim[i];
		}
		
	}


	void print(bool vals = true){
	    std::cout << "Tensor:\n";
	    std::cout << "   dims = "; for (auto d : dim) std::cout << d << " "; std::cout << "\n";
	    std::cout << "   offs = "; for (auto d : offsets) std::cout << d << " "; std::cout << "\n";
		if (vals){
			std::cout << "   vals = \n      "; std::cout.flush();
			for (int i=0; i<nelem; ++i){
				std::cout << vec[i] << " "; 
				bool flag = true;
				for (int axis=dim.size()-1; axis>0; --axis){
					flag = flag && (index(i)[axis] == dim[axis]-1);
					if (flag) std::cout << "\n      ";
				}
			}
		}
		std::cout << "\n";
	}
		
	int location(std::vector <int> ix){
		int loc = 0;
		int ndim = dim.size();
		for (int i=ndim-1; i>=0; --i){
			loc += offsets[i]*ix[i];
		}
		return loc;
	}

	template<class... ARGS>
	int location(ARGS... ids){
		return location({ids...});
	}

	double& operator() (std::vector<int> ix){
		return vec[location(ix)];
	}

	std::vector<int> index(int loc){
		int ndim = dim.size();
		std::vector<int> id(ndim);
		for (int i=ndim-1; i>=0; --i){
			int ix = loc % dim[i];
			loc = (loc-ix)/dim[i];
			id[i]=ix;
		}
		return id;
	}

	// for debug only
	void fill_sequence(){
		for(int i=0; i<vec.size(); ++i) vec[i]=i;
	}

	
	// generate indices on the plane perpendicular to 'axis' at index 'k' on the axis 
	std::vector<int> plane(int axis, int k = 0){
		axis = dim.size()-1-axis;
		std::vector<int> locs;
		locs.reserve(nelem);
		for (int i=0; i<nelem; ++i){
			if (index(i)[axis] == 0) locs.push_back(i + k*offsets[axis]);
		}
		return locs;
	}

	// axis is counted from the right
	// [..., 2, 1, 0]
	//          ^
	//           axis
	template <class BinOp>
	void transform_dim(int loc, int axis, BinOp binary_op, std::vector<double> w){
		assert(w.size() == dim[dim.size()-1-axis]);
		
		axis = dim.size()-1-axis;
		int off = offsets[axis];
		
		for (int i=loc, count=0; count<dim[axis]; i+= off, ++count){
			vec[i] = binary_op(vec[i], w[count]);	// this order is important, because the operator may not be commutative
		}
		
	}

	// axis is counted from the right
	// [..., 2, 1, 0]
	//          ^
	//           axis
	template <class BinOp>
	void transform(int axis, BinOp binary_op, std::vector<double> w){
		std::vector<int> locs = plane(axis);
		for (int i=0; i<locs.size(); ++i){
			transform_dim(locs[i], axis, binary_op, w);
		}
	}
	
	
	// axis is counted from the right
	// [..., 2, 1, 0]
	//          ^
	//           axis
	template <class BinOp>
	double accumulate_dim(double v0, int loc, int axis, BinOp binary_op, std::vector<double> weights={}){
		assert(weights.size() == 0 || weights.size() == dim[dim.size()-1-axis]);
		
		axis = dim.size()-1-axis;
		int off = offsets[axis];
		
		double v = 0;
		for (int i=loc, count=0; count<dim[axis]; i+= off, ++count){
			double w = (weights.size()>0)? weights[count] : 1;
			v = binary_op(v, w*vec[i]);
		}
		
		return v;
	}


	// axis is counted from the right
	// [..., 2, 1, 0]
	//          ^
	//           axis
	template <class BinOp>
	Tensor<T> accumulate(T v0, int axis, BinOp binary_op, std::vector<double> weights={}){
		std::vector<int> dim_new = dim;
		dim_new.erase(dim_new.begin()+dim_new.size()-1-axis);
		Tensor<T> tens(dim_new);
		
		std::vector<int> locs = plane(axis);
		
		for (int i=0; i<locs.size(); ++i){
			tens.vec[i] = accumulate_dim(v0, locs[i], axis, binary_op, weights);
		}
		
		return tens;
	}


	Tensor<T> max_dim(int axis){
		T v0 = vec[1];
		return accumulate(v0, axis, [](T a, T b){return std::max(a,b);});
	}

	Tensor<T> avg_dim(int axis, std::vector<double> weights={}){
		Tensor tens = accumulate(0, axis, std::plus<T>(), weights);
		tens /= double(dim[dim.size()-1-axis]);
		return tens;
	}


	Tensor<T> repeat_inner(int n) const {
		std::vector<int> dim_new = dim;
		dim_new.push_back(n);
		
		Tensor<T> tout(dim_new);
		int count = 0;
		for (int i=0; i<nelem; ++i){
			for (int j=0; j<n; ++j){
				tout.vec[count++] = vec[i];
			}
		}

		return tout;
	}

	Tensor<T> repeat_outer(int n) const {
		std::vector<int> dim_new = dim;
		dim_new.insert(dim_new.begin(), n);
		
		Tensor<T> tout(dim_new);
		int count = 0;
		for (int j=0; j<n; ++j){
			for (int i=0; i<nelem; ++i){
				tout.vec[count++] = vec[i];
			}
		}

		return tout;
	}


	// operators
	public: 	
	// see https://stackoverflow.com/questions/4421706/what-are-the-basic-rules-and-idioms-for-operator-overloading/4421719#4421719
	template <class S>
	Tensor<T>& operator += (const Tensor<S>& rhs){
		assert(dim == rhs.dim);
		std::transform(vec.begin(), vec.end(), rhs.vec.begin(), vec.begin(), std::plus<T>());
		return *this;
	}
	
	template <class S>
	Tensor<T>& operator -= (const Tensor<S>& rhs){
		assert(dim == rhs.dim);
		std::transform(vec.begin(), vec.end(), rhs.vec.begin(), vec.begin(), std::minus<double>());
		return *this;
	}

	template <class S>
	Tensor<T>& operator *= (const Tensor<S>& rhs){
		assert(dim == rhs.dim);
		std::transform(vec.begin(), vec.end(), rhs.vec.begin(), vec.begin(), std::multiplies<T>());
		return *this;
	}

	template<class S>	
	Tensor<T>& operator += (S s){
		std::transform(vec.begin(), vec.end(), vec.begin(), [&s](const T& x){return x+s;});
		return *this;
	}

	template <class S>
	Tensor<T>& operator -= (S s){
		std::transform(vec.begin(), vec.end(), vec.begin(), [&s](const T& x){return x-s;});
		return *this;
	}

	template <class S>
	Tensor<T>& operator *= (S s){
		std::transform(vec.begin(), vec.end(), vec.begin(), [&s](const T& x){return x*s;});
		return *this;
	}

	template<class S>
	Tensor<T>& operator /= (S s){
		std::transform(vec.begin(), vec.end(), vec.begin(), [&s](const T& x){return x/s;});
		return *this;
	}

};

template<class T>
Tensor<T> operator + (Tensor<T> lhs, const Tensor<T>& rhs){
	assert(lhs.dim == rhs.dim);
	lhs += rhs;
	return lhs;
}

template<class T>
Tensor<T> operator - (Tensor<T> lhs, const Tensor<T>& rhs){
	assert(lhs.dim == rhs.dim);
	lhs -= rhs;
	return lhs;
}

template<class T>
Tensor<T> operator * (Tensor<T> lhs, const Tensor<T>& rhs){
	assert(lhs.dim == rhs.dim);
	lhs *= rhs;
	return lhs;
}

template<class T, class S>
Tensor<T> operator + (Tensor<T> lhs, S s){
	lhs += s;
	return lhs;
}

template<class T, class S>
Tensor<T> operator - (Tensor<T> lhs, S s){
	lhs -= s;
	return lhs;
}

template<class T, class S>
Tensor<T> operator / (Tensor<T> lhs, S s){
	lhs /= s;
	return lhs;
}

template<class T, class S>
Tensor<T> operator * (Tensor<T> lhs, S s){
	lhs *= s;
	return lhs;
}

template<class T, class S>
Tensor<T> operator + (S s, Tensor<T> t){	// TODO: can passing be ref be used for these?
	return t+s;
}

template<class T, class S>
Tensor<T> operator - (S s, Tensor<T> t){
	return t-s;
}

template<class T, class S>
Tensor<T> operator * (S s, Tensor<T> t){
	return t*s;
}





//using namespace std;

//bool equals(double x1, double x2, double tol=1e-6){
//	return (abs(x1-x2) < tol);
//}

//template <class T>
//bool equals(vector<T> x1, vector<T> x2, double tol=1e-6){
//	if (x1.size()!= x2.size()) return false;
//	for (int i=0; i<x1.size(); ++i) if (abs(x1[i]-x2[i]) > tol) return false;
//	return true;
//}


//int main(){

//	Tensor u({2,3,5});
//	u.fill_sequence();
//	u.print();

//	for (int i=0; i<30; ++i){
//		auto v = u.index(i);
//		cout << i << ": ";
//		for (auto x : v) cout << x << " ";
//		cout << "\n";
//	}


//	double d;
//	d = u.accumulate_dim(0, u.location({0,0,0}), 0, std::plus<double>());
//	cout << "accum0 (0,0,0) = " << d << "\n"; // expected 10
//	if (!equals(d,10)) return 1;

//	d= u.accumulate_dim(0, u.location(1,1,0),   0, std::plus<double>());
//	cout << "accum0 (1,1,0) = " << d << "\n"; // expected 110	// template version
//	if (!equals(d,110)) return 1;
//	
//	d= u.accumulate_dim(0, u.location(1,0,3),   1, std::plus<double>());
//	cout << "accum1 (1,0,3) = " << d << "\n"; // expected 69 	// template version
//	if (!equals(d,69)) return 1;
//	
//	d= u.accumulate_dim(0, u.location({0,1,4}), 2, std::plus<double>());
//	cout << "accum1 (0,1,4) = " << d << "\n"; // expected 33
//	if (!equals(d,33)) return 1;


//	vector<int> x = u.plane(0);
//	vector<int> expected;
//	expected = {0,5,10, 15,20,25};
//	cout << "starts dim 0: "; for (auto xx : x) cout << xx << " "; cout << "\n";  
//	if (!equals(x, expected)) return 1;

//	x = u.plane(1);
//	expected = {0,1,2,3,4,15,16,17,18,19};
//	cout << "starts dim 1: "; for (auto xx : x) cout << xx << " "; cout << "\n";	
//	if (!equals(x, expected)) return 1;

//	x = u.plane(1,1);
//	expected = {5,6,7,8,9,20,21,22,23,24};
//	cout << "starts dim 1, off 1: "; for (auto xx : x) cout << xx << " "; cout << "\n";	
//	if (!equals(x, expected)) return 1;

//	x = u.plane(2);
//	expected = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
//	cout << "starts dim 2: "; for (auto xx : x) cout << xx << " "; cout << "\n";	
//	if (!equals(x, expected)) return 1;

//	x = u.plane(2,1);
//	expected = {15,16,17,18,19,20,21,22,23,24,25,26,27,28,29};
//	cout << "starts dim 2, off 1: "; for (auto xx : x) cout << xx << " "; cout << "\n";	
//	if (!equals(x, expected)) return 1;

//	
//	Tensor v = u.accumulate(0, 0, std::plus<double>());
//	expected = {10,35,60,  
//	            85,110,135};
//	v.print();
//	
//	Tensor v1 = u.accumulate(0, 1, std::plus<double>());
//	expected = {15, 18, 21, 24, 27, 
//                60, 63, 66, 69, 72};
//	v1.print();

//	Tensor v2 = u.accumulate(0, 2, std::plus<double>());
//	expected = {15, 17, 19, 21, 23, 
//                25, 27, 29, 31, 33, 
//                35, 37, 39, 41, 43};
//	v2.print();

//	u += 0.1;
//	u.print();

//	return 0;

//}

#endif
