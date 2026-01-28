## Test environments
* Local: Windows 10, R 4.5.1  
* R-hub: Windows, Ubuntu  
* devtools::check() and R CMD check --as-cran passed with 0 ERRORs, 0 WARNINGs, and 0 NOTEs  

## Changes since the previous CRAN release
- Added methodological reference to the preprint to the Description field (Wijnhorst et al 2025.).
- Wrapped long-running examples in `\donttest{}` because they require optional Stan backends and may take longer than 5 seconds to run on CRAN machines.
- Fixed a bug in the internal result summary function (`summarise_results()`) that affected extraction of posterior means from Stan summary matrices. This change improves robustness and does not affect the user-facing API.

## Comments
Maintenance update to the **socialSim** package.
The **socialSim** package provides tools to simulate and analyse datasets of social interactions between individuals using hierarchical Bayesian models implemented in Stan. It is designed for research on interacting phenotypes and on direct and indirect genetic effects (DGEs and IGEs).

