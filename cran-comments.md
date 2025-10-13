## Test environments
* Local Windows 10, R 4.5.1
* R-hub (Windows, Ubuntu)
* devtools::check() and R CMD check --as-cran passed with 1 ERROR, 1 WARNING, and 4 NOTEs

## R CMD check results
The single ERROR and WARNING are related to LaTeX not being available or configured locally:
"pdflatex is not available"
These are not related to package code and will not occur on CRAN servers, where LaTeX is installed.

There were no issues with examples, tests, or package dependencies.

## Comments
This is the initial CRAN release of the **socialSim** package.
It provides tools to simulate and analyse social interaction data using hierarchical Bayesian models implemented in Stan.
