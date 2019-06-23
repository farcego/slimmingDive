# slimmingDive



![example](https://github.com/farcego/slimmingDive/blob/master/inst/readme.gif)


slimmingDive provides a set of funtions to process dive
profiles from elephant seals summarized by a brocken-stick algorithme
for getting drift dives and drifting rates assotiated to these dives,
accordingly with Arce et al. (2019) proposal. It filter the data in three
steps. Drift dives are a particular types of
dives that contains a long, inactive phase in which the seals drifts
passively long the water colum. If these inactive phases are
identified, then we can track the buoyancy of the seal (and its
changes). It requires jags (Just another Gibs sampler) to work.


## JAGS installation

follow this link: http://mcmc-jags.sourceforge.net/



## slimmingDive Installation


You need package `remotes` to install slimmingDive
directly from R as it is not (yet) in CRAN

```R
#install.packages("remotes")
remotes::install_github("farcego/slimingDive")
```

Alternatively, you can download this git repo to your machine and
install it using your prefered procedures


## Citation

Arce, F., Bestley, S., Hindell, M. A., McMahon, C. R. & Wotherspoon,
S. 2019. A quantitative, hierarchical approach for detecting drift
dives and tracking buoyancy changes in southern elephant
seals. Scientific Reports: 9, 8936.
