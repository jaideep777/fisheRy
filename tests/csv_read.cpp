#include <iostream>
#include <string>
#include "read_csv.h"
using namespace std;

int main(){
    string file = "data/selection.spline.csv";

    auto v = read_csv_numeric(file);

    for (int i = 0; i<v[0].size(); ++i) {
        for (int j = 0; j<v.size(); ++j) {
            cout << v[j][i] << "\t";
        }
        cout << endl;
    }
    cout << endl;

}