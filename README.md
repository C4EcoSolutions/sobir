sobir: Significance of Boundary Lines in R
================
27/12/2019

`sobir` is an open-source R-library for separating illusions from
boundary lines.

A modified permutation test (based on [Phipson and
Smyth 2010](http://www.statsci.org/smyth/pubs/PermPValuesPreprint.pdf))
is applied to determine whether or not there is a statistically
significant constraint imposed on the maximum or minimum value of one
variable relative to another. In the instances where opposite boundaries
experience a constraint, this analysis is analagous to a correlation
test.

The value of this approach, however, is evident in the instances when
two variables aren’t correlated, but when the extremes of one are
constrained at only one extreme of the other. This is best illustrated
using an example. A few examples are provided below.

The package has been made publically available, but is still under
ongoing development. Any feedback would be appreciated.

## Installation

`sobir` is available on [CRAN](https://cran.r-project.org/) (version
0.1.0). It’s not recommended that this version be used, however; there
are some important improvements and bug fixes in version 0.1.1.9000,
which can be installed using `devtools`.

``` r
library("devtools")
install_github("C4EcoSolutions/sobir")
```

## Usage

``` r
library(sobir)
library(tidyr)
library(dplyr)
library(ggplot2)
```

### Simulated data

The data simulated below are simply two random normal samples with a
mean of 0 and standard deviation of 1.

An artificial boundary effect is imposed by removing all the points
beyond the line \(y = 2x + 2\). This simulates a scenario in which the
maximum value of y is constrained when \(x < 0\).

``` r
set.seed(1)
dat = tibble(x = rnorm(200, mean = 0, sd = 1),
             y = rnorm(200, mean = 0, sd = 1)) %>%
  mutate(bound = 2*x+2,
         beyond_bound = ifelse(bound < y, TRUE, FALSE))

# Define the points that are within and beyond the artificial boundary line
dat_beyond = dplyr::filter(dat, beyond_bound == TRUE)
dat_within = dplyr::filter(dat, beyond_bound == FALSE)
```

The solid blue line is a simple linear regression line for the
constrained data, indicating a poor correlation between the variables,
as expected. The dashed grey line represents the artificial boundary
imposed on the random data; the points that where removed are
represented by the light grey open points.

We can define the area beyond the top-left boundary as the no-data zone.
Whether this no-data zone reflects a significant constraint on the
maximum y values is to be
determined.

``` r
# Visualise the simulated data with the points beyond the boundary removed
dat_within %>%
  ggplot(aes(x = x, y = y)) +
  geom_abline(slope = 2, intercept = 2, linetype = 2, col = "grey") +
  geom_point() +
  geom_point(data = dat_beyond, shape = 1, col = "lightgrey") +
  geom_smooth(method = "lm", col = "blue", se = F) +
  annotate(geom = "text", x = -1.5, y = 1.5, label = "no-data\nzone") +
  lims(x = c(-3,3),
       y = c(-3,3)) +
  theme_bw() 
```

![](sobir-brief-explainer_files/figure-gfm/Visualise%20simulated%20data-1.png)<!-- -->

![](README_Figure%201.png)

To see what the package is assessing in the analysis, the boundaries and
no-data zones can be extracted and visualised using the below functions.

``` r
# Extract boundary points (bpts object)
bpts_within = extract_bpts(dat_within$x, dat_within$y)

# Plot the boundaries
bpts_plot(bpts_within, xlab = "x", ylab = "y") 
```

![](sobir-brief-explainer_files/figure-gfm/Extract%20and%20visualise%20boundary%20points-1.png)<!-- -->

![](README_Figure%202.png)

The analysis involves permuting the data \(nsim\) times to simulate a
random distribution of the sample space. This is the only variable that
needs to be explicitly defined. The greater \(nsim\), the greater the
precision of the analysis but the longer the analysis will take. In this
instance, \(nsim = 100\)

The results can be visualised as histograms of the simulated no-data
zone distributions relative to the observed no-data zone areas.

``` r
# Run the permuation test
set.seed(1)
perm_within = perm_area(dat_within$x, dat_within$y, nsim = 100)

# Plot the results
perm_plot(perm_within, histogram = T)
```

![](sobir-brief-explainer_files/figure-gfm/Run%20the%20analysis-1.png)<!-- -->

![](README_Figure%203.png)

As expected, the only no-data zone that shows a significant constraint
is the top-left where the artificial constraint was imposed.
