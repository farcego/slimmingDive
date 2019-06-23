# slimmingDive

[example](https://github.com/farcego/slimmingDive/raw/master/inst/readme.pgif)


This package process diving data of elephant seals summarized by a
broken-stick algorithm. It filter the data in three steps to select
Drift dives. Drift dives are a particular types of dives that contains
a long, inactive phase in which the seals drifts passively long the
water colum. If these inactive phases are identified, then we can
track the buoyancy of the seal )and its changes(




## Installation


You need `remotes`  (or `devtools`) to install slimmingDive
directly from R as it is not (yet) in CRAN

```R
#install.packages("remotes")
remotes::install_github("farcego/slimingDive")
```
```r
## install.packages('remotes')
remotes::install_packages()
```
or 

```r
## install.packages('devtools')
devtools::install_github()
```

Alternatively, you can download this git repo to your machine and
install it using your prefered procedures
