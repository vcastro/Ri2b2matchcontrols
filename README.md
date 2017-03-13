
Ri2b2matchcontrols
==================

R package that provides functionality to match controls using cem


Installation
------------

RStudio must be installed to run this package.  [[https://www.rstudio.com/products/rstudio/download/ Download RStudio]].

The package can be installed directly from Partners GitLab:

``` r
install.packages("devtools")

## Windows credentials store (will be different in Mac)
creds = git2r::cred_ssh_key("C:\\Users\\{username}\\.ssh\\id_rsa.pub", "C:\\Users\\{username}\\.ssh\\id_rsa")

devtools::install_git("git@gitlab.partners.org:vc070/Ri2b2matchcontrols.git", credentials = creds)
```