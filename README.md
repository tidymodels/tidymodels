<img src="tidymodels_hex.png" align="center" height = "80px" align = "middle"/>

[![R build status](https://github.com/tidymodels/tidymodels/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/tidymodels)
[![Coverage status](https://codecov.io/gh/tidymodels/tidymodels/branch/master/graph/badge.svg)](https://codecov.io/github/tidymodels/tidymodels?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tidymodels)](http://cran.r-project.org/web/packages/tidymodels)
[![Downloads](http://cranlogs.r-pkg.org/badges/tidymodels)](http://cran.rstudio.com/package=tidymodels)
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)


`tidymodels` is a "meta-package" for modeling and statistical analysis that share the underlying design philosophy, grammar, and data structures of the tidyverse.

It includes a core set of packages that are loaded on startup:

* [`broom`](https://broom.tidyverse.org/) takes the messy output of built-in functions in R, such as `lm`, `nls`, or `t.test`, and turns them into tidy data frames.

* [`dials`](https://tidymodels.github.io/dials/) has tools to create and manage values of tuning parameters.

* [`dplyr`](http://dplyr.tidyverse.org) contains a grammar for data manipulation. 

* [`ggplot2`](http://ggplot2.tidyverse.org) implements a grammar of graphics. 

* [`infer`](http://infer.netlify.com/) is a modern approach to statistical inference.

* [`parsnip`](https://tidymodels.github.io/parsnip/) is a tidy, unified interface to creating models. 

* [`purrr`](http://purrr.tidyverse.org) is a functional programming toolkit.

* [`recipes`](https://tidymodels.github.io/recipes/) is a general data preprocessor with a modern interface. It can create model matrices that incorporate feature engineering, imputation, and other help tools.

* [`rsample`](https://tidymodels.github.io/rsample/) has infrastructure for _resampling_ data so that models can be assessed and empirically validated. 

* [`tibble`](http://tibble.tidyverse.org) has a modern re-imagining of the data frame.
 
* [`tune`](https://tidymodels.github.io/tune/) contains the functions to optimize model hyper-parameters.
 
* [`workflows`](https://tidymodels.github.io/workflows/) has methods to combine pre-processing steps and models into a single object. 

* [`yardstick`](https://tidymodels.github.io/yardstick/) contains tools for evaluating models (e.g. accuracy, RMSE, etc.)

There are a few modeling packages that are also installed along with `tidymodels` (but are not attached on startup): 

* [`tidypredict`](http://tidypredict.netlify.com/) translates some model prediction equations to SQL for high-performance computing.

* [`tidyposterior`](https://tidymodels.github.io/tidyposterior/) can be used to compare models using resampling and Bayesian analysis.

* [`tidytext`](https://github.com/juliasilge/tidytext) contains tidy tools for quantitative text analysis, including basic text summarization, sentiment analysis, and text modeling.


To install:

```r
require(devtools)
devtools::install_github("tidymodels/tidymodels")
```

When loading the package, the versions and conflicts are listed:



```r
library(tidymodels)
```

```
## Registered S3 method overwritten by 'xts':
##   method     from
##   as.zoo.xts zoo
```

```
## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────── tidymodels 0.0.3 ──
```

```
## ✔ broom     0.5.2          ✔ purrr     0.3.3     
## ✔ dials     0.0.3.9001     ✔ recipes   0.1.7.9001
## ✔ discrim   0.0.1          ✔ rsample   0.0.5     
## ✔ dplyr     0.8.3          ✔ tibble    2.1.3     
## ✔ ggplot2   3.2.1          ✔ tune      0.0.0.9003
## ✔ infer     0.5.0          ✔ yardstick 0.0.4     
## ✔ parsnip   0.0.3.9001
```

```
## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────── tidymodels_conflicts() ──
## ✖ purrr::discard()  masks scales::discard()
## ✖ dplyr::filter()   masks stats::filter()
## ✖ dplyr::lag()      masks stats::lag()
## ✖ ggplot2::margin() masks dials::margin()
## ✖ dials::offset()   masks stats::offset()
## ✖ recipes::step()   masks stats::step()
```
  
