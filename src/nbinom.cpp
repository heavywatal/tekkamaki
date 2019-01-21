// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <blackthunnus/individual.hpp>

//' @details
//' `cpp_rnbinom` generates nbinom random numbers.
//' @rdname nbinom
//' @inheritParams stats::rnbinom
//' @seealso stats::rnbinom
// [[Rcpp::export]]
std::vector<int> cpp_rnbinom(int n, double size, double mu) {
    return pbt::Individual::rnbinom(n, size, mu);
}
