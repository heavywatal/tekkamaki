% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hsp.R
\name{as_hsp}
\alias{as_hsp}
\alias{write_hsp}
\alias{read_hsp}
\title{Count half-sib pairs in HSP format}
\usage{
as_hsp(samples)

write_hsp(x, path = "hsp.txt")

read_hsp(path)
}
\arguments{
\item{samples}{A \code{sample_family} data.frame of \code{\link[=tekka]{tekka()}} result.}

\item{x}{An outcome of \code{\link[=as_hsp]{as_hsp()}}}

\item{path}{A file name or connection to write to}
}
\value{
A data.frame of "hsp" subclass with six columns:
\itemize{
\item \code{cohort_i}, \code{cohort_j}: birth year of samples
\item \code{location_i}, \code{location_j}: of sampling
\item \code{comps}: the number of possible comparisons between group \emph{i} and \emph{j}.
All samples are compared pairwise, excluding self-comparisons.
The total number of comparisons equals \\(n \\choose 2\\),
where \emph{n} is the total sample size.
\item \code{hsps}: the count of half-sib pairs observed between group \emph{i} and \emph{j}
}
}
\description{
Half-sib pairs are counted between samples grouped by birth year and
sampled location.
}
\details{
\code{\link[=as_hsp]{as_hsp()}} converts a result data frame to HSP format.

\code{\link[=write_hsp]{write_hsp()}} writes a HSP data.frame to a file.
}
\examples{
set.seed(666)
result = tekka("-y20 -l2 --sa 2,2 --sj 2,2")
samples = result$sample_family[[1L]]
as_hsp(samples)
}
\seealso{
\code{\link[=as_hsp2]{as_hsp2()}} for the extended HSP format.

\code{\link[=find_kinship]{find_kinship()}} to count kinship within samples.
}
