#include <cpp11.hpp>

#include <tekka/individual.hpp>

#include <sstream>
#include <fstream>

cpp11::doubles_matrix<> matrix_byrow(const std::vector<std::vector<double>>& vvd) {
    const size_t nrow = vvd.size();
    const size_t ncol = vvd.at(0u).size();
    cpp11::writable::doubles_matrix<> mat(nrow, ncol);
    for (size_t i = 0; i < nrow; ++i) {
        const auto& row = vvd[i];
        for (size_t j = 0; j < ncol; ++j) {
          mat(i, j) = row[j];
        }
    }
    return mat;
}

[[cpp11::register]]
cpp11::list migration_matrices() {
  const auto& matrices = pbf::Individual::migration_matrices();
  const size_t n = matrices.size();
  cpp11::writable::list output(n);
  for (size_t i = 0; i < n; ++i) {
    output[i] = matrix_byrow(matrices[i]);
  }
  return output;
}

[[cpp11::register]]
std::vector<double> natural_mortality() {
  return pbf::Individual::natural_mortality();
}

[[cpp11::register]]
std::vector<double> fishing_mortality() {
  return pbf::Individual::fishing_mortality();
}

[[cpp11::register]]
std::vector<double> weight_for_age() {
  return pbf::Individual::weight_for_age();
}

[[cpp11::register]]
void read_json(const std::string& file) {
  if (file.empty()) {
    std::istringstream iss(pbf::Individual::default_json());
    pbf::Individual::read_json(iss);
  } else {
    std::ifstream ifs(file);
    pbf::Individual::read_json(ifs);
  }
}
