
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

This process may take time the first time.

## Example

This is a basic example which shows you how calculate the
Multidimensional Poverty Index:

#### Getting started

Load the `"swz_mics14"` dataset from the package. See the structure of
the variables.

``` r
library(mpitb)
## Use swz_mics14 data
head(swz_mics14)
#>   Household ID Household ID 2   Weight PSU Strata Water Assets School Nutrition
#> 1            1              1 1.137301   1      5     0      1      0         0
#> 2            1              1 1.137301   1      5     0      1      0         0
#> 3            1              1 1.137301   1      5     0      1      0         0
#> 4            1              1 1.137301   1      5     0      1      0         0
#> 5            1              2 1.137301   1      5     0      0      0         0
#> 6            1              2 1.137301   1      5     0      0      0         0
#>   Region  Area
#> 1 Hhohho Rural
#> 2 Hhohho Rural
#> 3 Hhohho Rural
#> 4 Hhohho Rural
#> 5 Hhohho Rural
#> 6 Hhohho Rural
```

We are going to estimate poverty disaggregated by different population
subgroups: Region and Area. Please note that if we see the structure of
this data, `Region` and `Area` are `Factor`.

``` r
str(swz_mics14)
#> 'data.frame':    20739 obs. of  11 variables:
#>  $ Household ID  : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Household ID 2: num  1 1 1 1 2 2 2 2 2 3 ...
#>  $ Weight        : num  1.14 1.14 1.14 1.14 1.14 ...
#>  $ PSU           : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Strata        : num  5 5 5 5 5 5 5 5 5 5 ...
#>  $ Water         : num  0 0 0 0 0 0 0 0 0 1 ...
#>  $ Assets        : num  1 1 1 1 0 0 0 0 0 0 ...
#>  $ School        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ Nutrition     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ Region        : Factor w/ 4 levels "Hhohho","Manzini",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ Area          : Factor w/ 2 levels "Urban","Rural": 2 2 2 2 2 2 2 2 2 2 ...
```

#### The first step

Specify the household survey design with `survey` R package and other
arguments to be used in the settings of our estimation of the MPI.

``` r
## Configurate the survey design 
data <- survey::svydesign(id=~PSU, weights = ~Weight , strata = ~Strata, data = swz_mics14)
class(data)
#> [1] "survey.design2" "survey.design"
```

#### The second step

Specify the set of parameters for our estimation of the MPI with
`mpitb.set` function. This function creates a object of `mpitb_set`
class containing all the information that will be used in further
functions for calculating different AF measures.

Apart from our `data` object, parameters of `mpitb.set` function include
`indicators`, `cutoffs`, `weights` and other optional arguments: `over`
is the vector of the groups, `name` and `desc` are for specifying a
personal name and description of your setting.

``` r
indicators <- c("Water","Assets","School","Nutrition")
weights <- c(1/3,1/3,1/6,1/6)
cutoff <- c(25)
over <- c("Region","Area")
set <- mpitb.set(data, indicators, cutoff, weights, over, name = "SWZ2014 Example", desc = "This is an example taken from the OPHI Summer School 2022")
print(set)
#> --- Specification --- 
#>  Name:  SWZ2014 Example 
#>  Weighting scheme:  equal 
#>  Description:  This is an example taken from the OPHI Summer School 2022 
#> 
#> --- Survey design --- 
#> 20739 observations
#> Stratified 1 - level Cluster Sampling design (with replacement)
#> With (347) clusters.
#> mpitb.set(data, indicators, cutoff, weights, over, name = "SWZ2014 Example", 
#>     desc = "This is an example taken from the OPHI Summer School 2022")
#> 
#> --- Parameters ---
#>  Cut-offs:  1 ( 0.25 )
#>  Indicators:  4 
#> 
#> --- Number of subgroups ---
#> Region : 4 levels
#> 
#> 
#> Area : 2 levels
#> 
#> 
#> No missing values found
```

It is also possible to view a summery of our settings where further
information can be observed such as the distribution of the population,
the uncensored headcount ratios of the indicators in the whole country,
etc…

``` r
#summary(set1)
```

#### Estimate the Adjusted Headcount Ratio, Incidence and Intensity of Poverty

Different AF measures can be calculated using `mpitb.measure` function
types. For example, if the user want to calculate only the M0 measure,
he/she should use `mpitb.M0` function which only takes a `"mpitb_set"`
class object. Analogously, with other measures such as H, A and censored
headcounts.

``` r
M0 <- mpitb.M0(set)
#H <- mpitb.H(set)
#A <- mpitb.A(set)
#headcounts <- mpitb.headcounts(set)
```

This functions estimate the AF measure for overall data by default and
by subgroups if specified. A `mpitb_measure` class object is returned
which contains the estimate and the calculated standard errors for each
level of analysis taking into account the household survey design.

The structure of this object is a list of lists. The list contains the
measures for different poverty cutoffs. If we calculate M0 for K = 25,
M0 is a list of length 1. Each element is a list whose elements contains
the estimated measures for the specified level of analysis keeping the
standard errors as an `atribute` of that element.

``` r
M0
#> [[1]]
#> [[1]]$Overall
#>   Overall 
#> 0.2802228 
#> attr(,"over")
#> [1] "Overall"
#> attr(,"se")
#>     Overall 
#> 0.009810924 
#> 
#> [[1]]$Region
#>     Hhohho    Manzini Shiselweni    Lubombo 
#>  0.2517617  0.2160783  0.3838337  0.3393379 
#> attr(,"over")
#> [1] "Region"
#> attr(,"se")
#>     Hhohho    Manzini Shiselweni    Lubombo 
#> 0.01705064 0.01742673 0.01567143 0.02303003 
#> 
#> [[1]]$Area
#>     Urban     Rural 
#> 0.1007249 0.3439050 
#> attr(,"over")
#> [1] "Area"
#> attr(,"se")
#>      Urban      Rural 
#> 0.01334257 0.01072516 
#> 
#> attr(,"k")
#> [1] 0.25
#> 
#> attr(,"class")
#> [1] "mpitb_M0"      "mpitb_measure"
```

This is quite ugly but necessary for controlling objects. Since the
package is thought for beginners/intermediate R users, extensions of R
base generic functions are included in the package such as
`as.data.frame` which permits the user to examine the results in a more
convenient way.

This makes results easier to control by the user. Once converted the
`mpitb_measure` class object to `data.frame`, the user can easily filter
the results by group, level, cut-off using R functions like `subset` or
funtions from `tidyr` package (depending on what is more confortable to
the user). Here is an example:

``` r
M0.results <- as.data.frame(M0)
head(M0.results)
#>      Over      Level Cut-off Coefficient Standard Error
#> 1 Overall    Overall    0.25   0.2802228    0.009810924
#> 2  Region     Hhohho    0.25   0.2517617    0.017050642
#> 3  Region    Manzini    0.25   0.2160783    0.017426726
#> 4  Region Shiselweni    0.25   0.3838337    0.015671434
#> 5  Region    Lubombo    0.25   0.3393379    0.023030034
#> 6    Area      Urban    0.25   0.1007249    0.013342569
```

In addition, if the user wants to estimate all the measures at once,
she/he can use `mpitb.est` function

``` r
#mpitb.est(set)
```
