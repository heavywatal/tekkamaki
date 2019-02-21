// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <tekka/individual.hpp>

Rcpp::NumericMatrix matrix_byrow(const std::vector<std::vector<double>>& vvd) {
    const size_t nrow = vvd.size();
    const size_t ncol = vvd.at(0u).size();
    Rcpp::NumericMatrix mat(nrow, ncol);
    for (size_t i = 0; i < nrow; ++i) {
        const auto& row = vvd[i];
        for (size_t j = 0; j < ncol; ++j) {
          mat(i, j) = row[j];
        }
    }
    return mat;
}

//' Parameter getter
//'
//' @rdname param-getter
//' @export
// [[Rcpp::export]]
Rcpp::List migration_matrices() {
  const auto& matrices = pbf::Individual::migration_matrices();
  const size_t n = matrices.size();
  Rcpp::List output(n);
  for (size_t i = 0; i < n; ++i) {
    output[i] = matrix_byrow(matrices[i]);
  }
  return output;
}

//' @rdname param-getter
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector natural_mortality() {
  return Rcpp::wrap(pbf::Individual::natural_mortality());
}

//' @rdname param-getter
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector fishing_mortality() {
  return Rcpp::wrap(pbf::Individual::fishing_mortality());
}

//' @rdname param-getter
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector weight_for_age() {
  return Rcpp::wrap(pbf::Individual::weight_for_age());
}
