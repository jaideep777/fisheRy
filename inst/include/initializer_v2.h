#ifndef UTILS_IO_INITIALIZER_H_
#define UTILS_IO_INITIALIZER_H_

#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <sstream>
#include <stdexcept>
#include <unordered_map>

/**
	\brief A simple initializer that reads parameters from an ini file.
	
	reference: https://codereview.stackexchange.com/questions/127819/ini-file-parser-in-c
	
	The parameter file must follow the formatting requirements of .ini files. 
	Sections are enclosed with [] and cannot have spaces or comments on the 
	same line. Each section has name-value pairs separated by =. Arrays have 
	values seperated by whitespace. Comments start with a ";" or "#" 
	and can occur anywhere in the file. All content on a line folling the 
	comment character will be ignored. 
	
	Here is an example parameter file:
	
	~~~{.ini}
	; a comment
	; this unnamed section is treated as the global section
	sim_name    =   mySimution    ;another comment
	output_file =  ~/output/test.txt  # yet another comment
	
	# this is also a comment
	; comments cannot be inserted in combinaiton with section headers. 
	; Also, no spaces after ] below
	[section 2]
	graphics    =  1           # Do we want graphics to be on? 
	timesteps   =  1000        ; For how many timesteps do we run the simulation?
	dt          =  0.1
	
	[arrays]
	array1      = 1 2 3 4 5 6  # these numbers can be retreived in a vector
	~~~
*/
namespace io{

class Initializer{
	using section = std::unordered_map<std::string, std::string>;

	private:
	std::unordered_map<std::string, section> sections;
	std::ifstream fin;

	private:
	inline const section& get_section(const std::string& sectionname) const {
		auto found = sections.find(sectionname);
		if (found != sections.end()) return found->second;
		else throw std::runtime_error("Initializer: Cannot find required section ["+sectionname+"]");
	}
	
	inline std::string get_value(const std::string& sectionname, const std::string& keyname) const {
		section sect = get_section(sectionname);
		auto it = sect.find(keyname);
		if (it != sect.end()) return it->second;
		else throw std::runtime_error("Initializer: Could not find required variable [" + keyname + "] in section [" + sectionname + "]");
	}

public:
	inline void parse(std::istream& in, bool add = false, bool verbose = true) {
		// check if we have to clear existing sections
		if (!add) {
			sections.clear();
		}
		// first lambda function for trimming whitespace
		auto ltrim = [](std::string& s) {
			s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
				return !std::isspace(ch);
				}));
			};
		// second lambda function for trimming whitespace
		auto rtrim = [](std::string& s) {
			s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char ch) {
				return !std::isspace(ch);
				}).base(), s.end());
			};
		// third lambda function for trimming both ends
		auto trim = [&](std::string& s) {
			ltrim(s);
			rtrim(s);
			};
		// Start with the global section
		std::string current_section = "global";
		// Read file line by line
		std::string line;
		while (std::getline(in, line)) {
			// Keep original line for logging purposes
			std::string original_line = line; 
			// 1. Handle Comments: Look for the first ';' or '#'
			size_t comment_pos = line.find_first_of(";#");
			if (comment_pos != std::string::npos) {
				// Cut the line right before the comment
				line = line.substr(0, comment_pos);
				// show info
				if (verbose) {
					std::cout << "Trimming comment line [" << original_line << "] to [" << line << "]\n";
				}
			}
			// 2. Trim whitespace around the resulting line
			std::string clean_line = line;
			trim(clean_line);
			// 3. Skip empty lines
			if (clean_line.empty()) {
				continue;
			}
			// 4. Detect Section: [SectionName]
			if (clean_line.front() == '[' && clean_line.back() == ']') {
				// Extract content inside brackets
				std::string inner = clean_line.substr(1, clean_line.size() - 2);
				// The original regex ([^\]]+) implied it couldn't be empty or contain ']'
				if (!inner.empty()) {
					current_section = inner;
					// show info
					if (verbose) {
						std::cout << "--- Section = " << current_section << " ---\n";
					}
					// Processed, skip to next line
					continue; 
				}
			}
			// 5. Detect Value: Key = Value
			size_t eq_pos = clean_line.find('=');
			if (eq_pos != std::string::npos) {
				std::string key = clean_line.substr(0, eq_pos);
				std::string value = clean_line.substr(eq_pos + 1);
				// Trim both key and value
				trim(key);
				trim(value);
				// check if has no internal spaces.
				bool key_has_internal_space = false;
				for (const char c : key) {
					if (std::isspace(static_cast<unsigned char>(c))) {
						key_has_internal_space = true;
						break;
					}
				}
				// check that key and value are non-empty
				if (!key.empty() && !key_has_internal_space && !value.empty()) {
					sections[current_section][key] = value;
					// show info
					if (verbose) {
						std::cout << key << " = " << value << "\n";
					}
					// Processed, skip to next line
					continue;
				}
			}

			// 6. If we get here, it doesn't match anything (Else)
			if (verbose) {
				std::cout << "skipping line [" << original_line << "]\n";
			}
			// throw std::runtime_error("Cannot parse line "+line);
		}
	}
		
	inline void parse(std::string filename, bool add = false, bool verbose = true) {
		fin.open(filename.c_str());
		if (!fin) throw std::invalid_argument("Initializer: Could not open file: "+filename);
		if (verbose) std::cout << "Parsing file: " << filename << "\n";
		parse(fin, add, verbose);
	}

	template<class T>
	T get(const std::string& sectionname, const std::string& keyname) const {
		std::string result = get_value(sectionname, keyname);
		std::stringstream sin(result);
		T val;
		sin >> val;
		return val;	
	}

	template<class T>
	T get(const std::string& keyname) const {
		return get<T>("global", keyname);
	}

	template<class T>
	std::vector<T> get_vector(const std::string& sectionname, const std::string& keyname) const {
		std::string result = get_value(sectionname, keyname);
		std::stringstream sin(result);
		T val;
		std::vector<T> vec;
		while(sin >> val){
			vec.push_back(val);
		}
		return vec;	
	}

	template<class T>
	std::vector<T> get_vector(const std::string& keyname) const {
		return get_vector<T>("global", keyname);
	}
	
	inline void print() const {
		std::cout << "------------------\n";
		std::cout << "> Initializer:\n";
		for (const auto& sec : sections){
			std::cout << "  [" << sec.first << "]\n";
			for (const auto& x : sec.second){
				std::cout << "    " << x.first << " = {" << x.second << "}\n";
			}
		}
		std::cout << "------------------\n";
	}

};


} // namespace io

#endif

