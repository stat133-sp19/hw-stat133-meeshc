% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{bin_distribution}
\alias{bin_distribution}
\title{Bin Distribution}
\usage{
bin_distribution(trials, prob)
}
\arguments{
\item{trials}{number of trials}

\item{prob}{probability of success}
}
\value{
data frame with probability distribution
}
\description{
calculates binomial probability distribution
}
\examples{
bin_distribution(trials = 5, prob = 0.5)
}
