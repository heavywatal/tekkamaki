#include <cpp11.hpp>

#include <tekka/individual.hpp>

[[cpp11::register]]
std::vector<int> cpp_rnbinom(int n, double size, double mu) {
    return pbf::Individual::rnbinom(n, size, mu);
}
