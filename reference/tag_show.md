# Facilities for loading and updating other packages

The tidymodels metapackage contains a set of tags for specific topics in
modeling and analysis. These can be used to load packages in groups.

## Usage

``` r
tag_show()

tag_attach(tag)

tag_update(tag)
```

## Arguments

- tag:

  A character strong for the tag. Current options are: 'Bayesian
  analysis', 'deep learning', 'ensembles', 'extra models', 'extra
  recipes', 'feature engineering', 'interpretation', 'parallel
  processing', 'parameter tuning', 'resampling', 'text analysis', 'time
  series', and 'visualization'.

## Examples

``` r
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
