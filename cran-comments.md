## Test environments
* Local: Windows 10, R 4.5.1  
* R-hub: Windows, Ubuntu  
* devtools::check() and R CMD check --as-cran passed with 0 ERRORs, 0 WARNINGs, and 2 NOTEs  

## R CMD check results
All examples, tests, and dependencies run successfully.  
The remaining NOTEs are expected and not related to package functionality:

- **Possibly misspelled words** in DESCRIPTION (e.g., "de", "Groot", "et", "al", "dyads") refer to author names or standard terminology.  
- **cmdstanr** is listed under Suggests and made available via  
  `Additional_repositories: https://stan-dev.r-universe.dev`.

## Changes following CRAN reviewer comments
- Added method references in the Description field (Moore et al. 1997, De Groot et al. 2022).  
- Replaced all `\dontrun{}` examples with `\donttest{}`.  
- Updated Description to begin with a full sentence and ensure proper formatting.  
- Confirmed that all examples conditionally skip if neither `cmdstanr` nor `rstan` are installed.  

## Comments
This is the initial CRAN release of **socialSim**.  
The package provides tools to simulate and analyse datasets of social interactions between individuals using hierarchical Bayesian models implemented in Stan.  
It is designed for research on interacting phenotypes and on direct and indirect genetic effects (DGEs and IGEs).
