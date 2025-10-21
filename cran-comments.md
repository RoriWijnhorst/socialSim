## Test environments
* Local Windows 10, R 4.5.1
* R-hub (Windows, Ubuntu)
* devtools::check() and R CMD check --as-cran passed with 1 ERROR, 1 WARNING, and 4 NOTEs

## R CMD check results
The single ERROR and WARNING are related to LaTeX not being available or configured locally:
"pdflatex is not available"
These are not related to package code and will not occur on CRAN servers, where LaTeX is installed.

There were no issues with examples, tests, or package dependencies.

## CRAN reviewer comments
"Suggests or Enhances not in mainstream repositories:
     cmdstanr
but no declaration where to get it from?"

The package optionally uses the GitHub package cmdstanr (Stan Development Team), which is listed under Suggests and GitHub repo specificied under Remotes: stan-dev/cmdstanr. Functions run without it, and examples and tests skip when cmdstanr is not available. 

"If there are references describing the methods"

Added two references for more information for the user.

"Please replace dontrun with donttest"

Done, donttest, because example will not be exetuble in <5s.


## Comments
This is the initial CRAN release of the **socialSim** package.
It provides tools to simulate and analyse social interaction data using hierarchical Bayesian models implemented in Stan.
