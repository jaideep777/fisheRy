#ifndef READ_CSV_H
#define READ_CSV_H

#include "csvrow.h"
#include <fstream>
#include <vector>
#include <iostream>

inline std::vector<std::vector<double>> read_csv_numeric(std::string filename){
    std::ifstream fin;
    
    fin.open(filename.c_str());
    if (!fin) throw std::runtime_error("Could not open file: "+filename);

    // read header
    flare::CSVRow row;
    fin >> row;
    int ncol = row.size();

    std::vector<std::vector<double>> v;
    v.resize(ncol);
    // read all rows
    while(fin >> row){
        // std::cout << row.get_line_raw() << std::endl;
        for (int i=0; i<ncol; ++i) v[i].push_back(std::stod(row[i]));
    }

    fin.close();

    return v;
}


#endif
