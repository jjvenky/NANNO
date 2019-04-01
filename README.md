NANNO
=====


NANNO is an R package that creates a nitrogen isotope biogeochemical model via the [simecol] package that describes freshwater nitrogen isotope biogeochemistry. NANNO does forward simulations and models the changes in nitrogen concentrations and isotopic ratios. NANNO can also be used iteratively to model-fit field data.

NANNO was initially developed to describe changes in nitrogen concentrations and isotopic ratios in a wastewater treatment plant plumes in rivers.


Installation
------------

To install this package from Github:


```
install.packages("devtools")
library(devtools)
install_github("jjvenky/NANNO@*release", # to get the latest release
                dependencies = TRUE)
```


Feedback
--------

No doubt this software can be improved by questions, suggestions, and bug reports. Please use the [Issues] page or [email] me.



To Cite NANNO
-------------

If you use NANNO, please cite it as:

* Venkiteswaran JJ, Schiff SL, Ingalls BP. Fate of Wastewater Ammonia and Nitrate Discharged to a Canadian River. FACETS. DOI: [10.1139/facets-2018-0028](https://doi.org/10.1139/facets-2018-0028)

[simecol]:https://cran.r-project.org/web/packages/simecol/index.html
[Issues]:https://github.com/jjvenky/NANNO/issues
[email]:https://github.com/jjvenky/NANNO/blob/master/DESCRIPTION
