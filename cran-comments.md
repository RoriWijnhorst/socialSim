## Test environments
* Local: Windows 10, R 4.5.1  
* R-hub: Windows, Ubuntu  
* devtools::check() and R CMD check --as-cran passed with 0 ERRORs, 0 WARNINGs, and 0 NOTEs  

## Changes following CRAN reviewer comments
- Added method references in the Description field (Moore et al. 1997, De Groot et al. 2022). 
- Changes example wrappers to `\donttest{}` because the examples take longer than >5 s to run.

## Comments
This is the initial CRAN release of **socialSim**.  
The package provides tools to simulate and analyse datasets of social interactions between individuals using hierarchical Bayesian models implemented in Stan.  
It is designed for research on interacting phenotypes and on direct and indirect genetic effects (DGEs and IGEs).

