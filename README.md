
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mpitb: a toolbox for estimating multidimensional poverty indices in R

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to introduce …

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
#>   Household ID Household ID 2 PSU Strata   Weight Nutrition School Water Assets
#> 1          101         100101   1      5 1.137301         0      0     0      1
#> 2          101         100102   1      5 1.137301         0      0     0      1
#> 3          101         100103   1      5 1.137301         0      0     0      1
#> 4          101         100104   1      5 1.137301         0      0     0      1
#> 5          102         100201   1      5 1.137301         0      0     0      0
#> 6          102         100202   1      5 1.137301         0      0     0      0
#>    Area Region
#> 1 Rural Hhohho
#> 2 Rural Hhohho
#> 3 Rural Hhohho
#> 4 Rural Hhohho
#> 5 Rural Hhohho
#> 6 Rural Hhohho
```

We are going to estimate poverty disaggregated by different population
subgroups: Region and Area. Please note that if we see the structure of
this data, `Region` and `Area` are `Factor`.

``` r
str(swz_mics14)
#> 'data.frame':    20739 obs. of  11 variables:
#>  $ Household ID  : num  101 101 101 101 102 102 102 102 102 103 ...
#>  $ Household ID 2: num  1e+05 1e+05 1e+05 1e+05 1e+05 ...
#>  $ PSU           : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ Strata        : num  5 5 5 5 5 5 5 5 5 5 ...
#>  $ Weight        : num  1.14 1.14 1.14 1.14 1.14 ...
#>  $ Nutrition     : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ School        : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ Water         : num  0 0 0 0 0 0 0 0 0 1 ...
#>  $ Assets        : num  1 1 1 1 0 0 0 0 0 0 ...
#>  $ Area          : Factor w/ 2 levels "Urban","Rural": 2 2 2 2 2 2 2 2 2 2 ...
#>  $ Region        : Factor w/ 4 levels "Hhohho","Manzini",..: 1 1 1 1 1 1 1 1 1 1 ...
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
indicators <- c("Nutrition","School","Water","Assets")
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
#>    Overall 
#> 0.09782088 
#> attr(,"over")
#> [1] "Overall"
#> attr(,"se")
#>     Overall 
#> 0.005230329 
#> attr(,"df")
#> Overall 
#>     339 
#> 
#> [[1]]$Region
#>     Hhohho    Manzini Shiselweni    Lubombo 
#> 0.08441687 0.07104974 0.13478863 0.13006382 
#> attr(,"over")
#> [1] "Region"
#> attr(,"se")
#>      Hhohho     Manzini  Shiselweni     Lubombo 
#> 0.008838600 0.007811974 0.008313157 0.015928619 
#> attr(,"df")
#>     Hhohho    Manzini Shiselweni    Lubombo 
#>         95         96         78         70 
#> 
#> [[1]]$Area
#>      Urban      Rural 
#> 0.03109217 0.12149484 
#> attr(,"over")
#> [1] "Area"
#> attr(,"se")
#>       Urban       Rural 
#> 0.006592045 0.006279293 
#> attr(,"df")
#> Urban Rural 
#>    86   253 
#> 
#> attr(,"k")
#> [1] 25
#> 
#> attr(,"class")
#> [1] "mpitb_M0"      "mpitb_measure"
```

This is quite ugly but necessary for controlling objects. Since the
package is thought for beginners/intermediate R users, extensions of R
base generic functions are included in the package such as
`as.data.frame()` which permits the user to examine the results in a
more convenient way.

This makes results easier to control by the user. Once converted the
`mpitb_measure` class object to `data.frame`, the user can easily filter
the results by group, level, cut-off using R functions like `subset` or
funtions from `tidyr` package (depending on what is more confortable to
the user). Here is an example:

``` r
M0.results <- as.data.frame(M0)
head(M0.results)
#>      Over      Level Cut-off Coefficient Standard Error
#> 1 Overall    Overall      25  0.09782088    0.005230329
#> 2  Region     Hhohho      25  0.08441687    0.008838600
#> 3  Region    Manzini      25  0.07104974    0.007811974
#> 4  Region Shiselweni      25  0.13478863    0.008313157
#> 5  Region    Lubombo      25  0.13006382    0.015928619
#> 6    Area      Urban      25  0.03109217    0.006592045
```

Other typical R methods are included such as `coef()` and `confint()` to
retrieve the estimated coefficients and confidence intervals of the
poverty measures, respectively.

``` r
coef(M0)
#>                   Cut-offs Coefficient
#> Overall.Overall         25  0.09782088
#> Region.Hhohho           25  0.08441687
#> Region.Manzini          25  0.07104974
#> Region.Shiselweni       25  0.13478863
#> Region.Lubombo          25  0.13006382
#> Area.Urban              25  0.03109217
#> Area.Rural              25  0.12149484
confint(M0, parm = "coefficient", level = 0.95)
#>                   Cut-offs Lower Bound (95%) Upper Bound (95%)
#> Overall.Overall         25        0.09726211        0.09837965
#> Region.Hhohho           25        0.08261636        0.08621738
#> Region.Manzini          25        0.06946688        0.07263259
#> Region.Shiselweni       25        0.13291431        0.13666296
#> Region.Lubombo          25        0.12626577        0.13386186
#> Area.Urban              25        0.02967884        0.03250551
#> Area.Rural              25        0.12071736        0.12227232
```

In addition, if the user wants to estimate all the measures at once,
she/he can use `mpitb.est` function

``` r
estimation <- mpitb.est(set)
# All measures are save in a list. We can make use of other functions by accessing to names of the list
names(estimation)
#> [1] "M0"         "H"          "A"          "Headcounts"
as.data.frame(estimation$M0)
#>      Over      Level Cut-off Coefficient Standard Error
#> 1 Overall    Overall      25  0.09782088    0.005230329
#> 2  Region     Hhohho      25  0.08441687    0.008838600
#> 3  Region    Manzini      25  0.07104974    0.007811974
#> 4  Region Shiselweni      25  0.13478863    0.008313157
#> 5  Region    Lubombo      25  0.13006382    0.015928619
#> 6    Area      Urban      25  0.03109217    0.006592045
#> 7    Area      Rural      25  0.12149484    0.006279293
```
