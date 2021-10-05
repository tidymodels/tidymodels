# tidymodels 0.1.4

 * Updated versions

 * Added five package startup messages to point users to helpful documentation. These startup messages are chosen via modulo of the time, _not_ using the RNG, so these messages will not affect reproducibility (#59).

 * Added R Markdown template for a tidymodels analysis (#58).

# tidymodels 0.1.3

 * Re-licensed package from GPL-3 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/tidymodels/issues/51).

 * Updated versions
 
 * Added `tidymodels_prefer()` to help resolve name conficts with other packages.
 
 * Added `workflowsets` as a core package. 

# tidymodels 0.1.2

 * Updated versions
 
 * Removed `crayon` dependency. 
 
 * Updated tags.
 
# tidymodels 0.1.1

 * Updated versions
 
 * `tidyposterior`, `pillar`, `tidypredict`, `tidytext` were removed from the imports list due to an `R CMD check` warning about the number of imports.  
 
 * `tidyr` and `modeldata` were added to the imports list. 

# tidymodels 0.0.4

 * Updated versions

 * Added `workflows`, and `tune` to the core package list. 

 * Moved away from testing via `travis`

# tidymodels 0.0.3

 * Updated versions

# tidymodels 0.0.2

 * Added  `parsnip` and `dials` to the core package list. 

 * Package requirements bumped to current versions.


# tidymodels 0.0.1

First CRAN version.



