# slimmingDive
[![DOI](https://zenodo.org/badge/190671173.svg)](https://zenodo.org/badge/latestdoi/190671173)



![example](https://github.com/farcego/slimmingDive/blob/master/inst/readme.gif)


**slimmingDive** provides a suite of funtions to process dive profiles
of southern elephant seals (*Mirounga leonina*), summarized by a
broken-stick algorithm and transmitted through the Argos satellite
system. It has been developed to detect drift dives and associated
drift rates. Drift dives are a particular type of dives that contain a
long, inactive phase in which the seals drift passively along the
water colum. Due the specific characteristics of elephant seals dives,
the observed drift rate is determined primarily by the buoyancy of the
seal (that is, the ratio beteen blubber and lean tissues). It offers an
efective measurement for tracking changes the body condition of the seals,
and its changes, from low resolution dive information.\
**slimmingDive** implements the method reported in Arce et al. (2019),
and requires *jags* (Just another Gibbs sampler) to be installed in
order to work.


## JAGS installation

follow the instructions of this link: http://mcmc-jags.sourceforge.net/


## slimmingDive Installation


You need package `remotes` to install slimmingDive
directly from GitHub as it is not (yet) in CRAN

```R
## install.packages("remotes")
## remotes::install_github("farcego/slimmingDive")
```

Alternatively, you can download this git repo to your machine and
build the package trough 

$ R CMD build --resave-data slimmingDive


## Citation

Arce, F., Bestley, S., Hindell, M. A., McMahon, C. R. & Wotherspoon,
S. 2019. A quantitative, hierarchical approach for detecting drift
dives and tracking buoyancy changes in southern elephant
seals. Scientific Reports: 9, 8936. doi: 10.1038/s41598-019-44970-1


## Aknowledgements

The data contained in the package is provided by the Australian
Integrate Marine Observing System ([IMOS](http://imos.org.au/))
