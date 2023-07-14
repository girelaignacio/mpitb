
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpitb: a toolbox for estimating multidimensional poverty indices in R

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to …

## Installation

You can install the development version of mpitb from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("girelaignacio/mpitb")
```

## Example

This is a basic example which shows you how calculate the
Multidimensional Poverty Index:

#### Getting started

Load the `"swz_mics14"` dataset from the package. See the structure of
the variables.

``` r
#library(mpitb)
## Use swz_mics14 data
#data <- swz_mics14
```

We are going to estimate poverty disaggregated by different population
subgroups: Region and Area. Please note that if we see the structure of
this data, `Region` and `Area` are `Factor`.

``` r
# str(swz_mics14)
```

#### The first step

Specify the household survey design with `survey` R package and other
arguments to be used in the settings of our estimation of the MPI.

``` r
## Configurate the survey design 
# data <- survey::svydesign(id=~psu, weights = ~hhweight, strata = ~stratum, data = swz_mics14)
# class(data)
```

#### The second step

Set up the data and parameters for our estimation of the MPI with
`mpitb.set` function.

``` r
#set1 <- mpitb.set(data, indicators)
#print(set1)
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
#summary(set1)
```

#### Estimate the Adjusted Headcount Ratio, Incidence and Intensity of Poverty

``` r
#M0 <- mpitb.M0(set1)
#H <- mpitb.H(set1)
#A <- mpitb.A(set1)
```
