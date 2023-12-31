% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mpitb.H.R
\name{mpitb.H}
\alias{mpitb.H}
\alias{mpitb.H.mpitb_set}
\title{Incidence of poverty (\eqn{H})}
\usage{
mpitb.H(object, ...)

\method{mpitb.H}{mpitb_set}(object, ...)
}
\arguments{
\item{object}{a \code{mpitb_set} class object}

\item{...}{other arguments}
}
\value{
\code{mpitb_H} and \code{mpitb_measure} class objects
}
\description{
Calculate the incidence of poverty (\eqn{H}) which is the proportion people who are identified as poor in multidimensional terms, e.g.,
is a function that counts the number of observations whose censored deprivation score is greater than zero \eqn{q = \sum_{i=1}^n 1_{{c_i(k) > 0}}} divided by the total number of population (\eqn{n}).
\deqn{H = \frac{q}{n}}
}
\examples{
library(mpitb)
data <- survey::svydesign(id=~PSU, weights = ~Weight, strata = ~Strata,
        data = swz_mics14)
indicators <- c("Water","Assets","School","Nutrition")
weights <- c(1/6,1/6,1/3,1/3)
cutoff <- c(25,50)
subgroup <- c("Region","Area")

set <- mpitb.set(data, indicators, weights, cutoff, subgroup,
      name = "Example", desc = "SWZ MICS survey 2014")

H <- mpitb.H(set)

## to observe the results in a data.frame format
as.data.frame(H)
}
\references{
\emph{Alkire, S., Foster, J. E., Seth, S., Santos, M. E., Roche, J., & Ballon, P. (2015). Multidimensional poverty measurement and analysis. Oxford University Press.}
}
