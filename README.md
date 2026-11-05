[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2647564.svg)](https://doi.org/10.5281/zenodo.2647564)
[![R-CMD-check](https://github.com/jjvenky/NANNO/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jjvenky/NANNO/actions/workflows/R-CMD-check.yaml)

NANNO
=====

NANNO is an R package that creates a nitrogen isotope biogeochemical model via the [simecol] package that describes freshwater nitrogen isotope biogeochemistry. NANNO does forward simulations and models the changes in nitrogen concentrations and isotopic ratios. NANNO can also be used iteratively to model-fit field data.

NANNO was initially developed to describe changes in nitrogen concentrations and isotopic ratios in a wastewater treatment plant plume in a river.

Installation
------------

To install this package from GitHub:

```
install.packages("remotes")
library(remotes)
install_github("jjvenky/NANNO")
```

Quick start
-----------

```
library(NANNO)

# Forward simulation with the demonstration model
NANNO_test()

# Model-fit the bundled example field data
result <- NANNO_fit(system.file("extdata", "NANNO_example-data.csv",
                                package = "NANNO"))
result$fit_stats
```

Feedback
--------

Questions, suggestions, and bug reports are welcome on the [Issues] page.

To cite NANNO
-------------

If you use NANNO, please cite it as:

* Venkiteswaran JJ, Schiff SL, Ingalls BP. Fate of Wastewater Ammonia and Nitrate Discharged to a Canadian River. FACETS. DOI: [10.1139/facets-2018-0028](https://doi.org/10.1139/facets-2018-0028)

[simecol]:https://cran.r-project.org/web/packages/simecol/index.html
[Issues]:https://github.com/jjvenky/NANNO/issues
