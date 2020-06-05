<img src="tidymodels_hex.png" align="center" height = "80px" align = "middle"/>

[![R build status](https://github.com/tidymodels/tidymodels/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/tidymodels)
[![Coverage status](https://codecov.io/gh/tidymodels/tidymodels/branch/master/graph/badge.svg)](https://codecov.io/github/tidymodels/tidymodels?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tidymodels)](http://cran.r-project.org/web/packages/tidymodels)
[![Downloads](http://cranlogs.r-pkg.org/badges/tidymodels)](http://cran.rstudio.com/package=tidymodels)
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)


`tidymodels` is a "meta-package" for modeling and statistical analysis that share the underlying design philosophy, grammar, and data structures of the tidyverse.

It includes a core set of packages that are loaded on startup:

* [`broom`](https://broom.tidymodels.org) takes the messy output of built-in functions in R, such as `lm`, `nls`, or `t.test`, and turns them into tidy data frames.

* [`dials`](https://dials.tidymodels.org) has tools to create and manage values of tuning parameters.

* [`dplyr`](http://dplyr.tidyverse.org) contains a grammar for data manipulation. 

* [`ggplot2`](http://ggplot2.tidyverse.org) implements a grammar of graphics. 

* [`infer`](http://infer.netlify.app) is a modern approach to statistical inference.

* [`parsnip`](https://parsnip.tidymodels.org) is a tidy, unified interface to creating models. 

* [`purrr`](http://purrr.tidyverse.org) is a functional programming toolkit.

* [`recipes`](https://recipes.tidymodels.org) is a general data preprocessor with a modern interface. It can create model matrices that incorporate feature engineering, imputation, and other help tools.

* [`rsample`](https://rsample.tidymodels.org) has infrastructure for _resampling_ data so that models can be assessed and empirically validated. 

* [`tibble`](http://tibble.tidyverse.org) has a modern re-imagining of the data frame.

* [`tidyr`](http://tidyr.tidyverse.org) provides tools for reshaping data. 

* [`tune`](https://tune.tidymodels.org) contains the functions to optimize model hyper-parameters.
 
* [`workflows`](https://workflows.tidymodels.org) has methods to combine pre-processing steps and models into a single object. 

* [`yardstick`](https://yardstick.tidymodels.org) contains tools for evaluating models (e.g. accuracy, RMSE, etc.)

There are a few modeling packages that are also installed along with `tidymodels` (but are not attached on startup): 

* [`tidypredict`](https://tidypredict.tidymodels.org) translates some model prediction equations to SQL for high-performance computing.

* [`tidyposterior`](https://tidyposterior.tidymodels.org) can be used to compare models using resampling and Bayesian analysis.

* [`tidytext`](https://juliasilge.github.io/tidytext) contains tidy tools for quantitative text analysis, including basic text summarization, sentiment analysis, and text modeling.


To install:

```r
require(devtools)
devtools::install_github("tidymodels/tidymodels")
```

When loading the package, the versions and conflicts are listed:



```r
library(tidymodels)
```
  
