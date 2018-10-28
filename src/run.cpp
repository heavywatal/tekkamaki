// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <blackthunnus/program.hpp>

//' @description
//' `cpp_blackthunnus` is a Rcpp function to call blackthunnus
//' @rdname blackthunnus
// [[Rcpp::export]]
Rcpp::CharacterVector cpp_blackthunnus(const std::vector<std::string>& args) {
    try {
        std::streambuf* obuf = pbt::std_cout_rdbuf(Rcpp::Rcout.rdbuf());
        std::streambuf* ebuf = pbt::std_cerr_rdbuf(Rcpp::Rcerr.rdbuf());
        pbt::Program program(args);
        program.run();
        pbt::std_cout_rdbuf(obuf);
        pbt::std_cout_rdbuf(ebuf);
        return Rcpp::CharacterVector::create(
            Rcpp::Named("sample_family", program.sample_family()),
            Rcpp::Named("demography", program.demography())
        );
    } catch (const std::runtime_error& e) {
        Rcpp::Rcerr << e.what() << "\n";
    }
    return {};
}
