#include <Rcpp.h>
#include <tekka/program.hpp>

//' @details
//' `cpp_tekka` is a Rcpp function to call tekka
//' @rdname tekka
// [[Rcpp::export]]
Rcpp::CharacterVector cpp_tekka(const std::vector<std::string>& args) {
    try {
        std::streambuf* obuf = pbf::std_cout_rdbuf(Rcpp::Rcout.rdbuf());
        std::streambuf* ebuf = pbf::std_cerr_rdbuf(Rcpp::Rcerr.rdbuf());
        pbf::Program program(args);
        program.run();
        pbf::std_cout_rdbuf(obuf);
        pbf::std_cout_rdbuf(ebuf);
        return Rcpp::CharacterVector::create(
            Rcpp::Named("sample_family", program.sample_family()),
            Rcpp::Named("demography", program.demography())
        );
    } catch (const std::runtime_error& e) {
        Rcpp::Rcerr << e.what() << "\n";
    }
    return {};
}
