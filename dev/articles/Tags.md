# Using tags

The tidymodels framework has a set of core packages that are loaded and
attached when the tidymodels package is loaded.

There are other sets of packages that can be attached via
`tidymodels::tag_attach(tag)` where the `tag` is a character string.
Existing tags are:

``` r
library(tidymodels)
tag_show()
#> All tags:
#> 
#> Bayesian analysis: 'tidyposterior', 'tidybayes'
#> deep learning: 'keras'
#> ensembles: 'modeltime.ensemble', 'stacks'
#> feature engineering: 'keras', 'textrecipes'
#> parallel processing: 'furrr'
#> parameter tuning: 'finetune'
#> resampling: 'infer', 'tidyposterior'
#> text analysis: 'tidytext', 'keras', 'textrecipes'
#> time series: 'timetk', 'tidyquant', 'sweep', 'modeltime', 'tsibble', 'fable'
#> extra recipes: 'embed', 'textrecipes', 'themis'
#> extra models: 'discrim', 'plsmod', 'rules', 'poissonreg', 'baguette'
#> visualization: 'dotwhisker', 'ggforce', 'patchwork', 'gganimate', 'ggrepel'
#> interpretation: 'vip', 'lime', 'DALEXtra'
```
