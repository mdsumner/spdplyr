
[![Travis-CI Build Status](https://travis-ci.org/mdsumner/spdplyr.svg?branch=master)](https://travis-ci.org/mdsumner/spdplyr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mdsumner/spdplyr?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/spdplyr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/spdplyr)](https://cran.r-project.org/package=spdplyr) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/spdplyr)](http://www.r-pkg.org/pkg/spdplyr) [![Coverage Status](https://img.shields.io/codecov/c/github/mdsumner/spdplyr/master.svg)](https://codecov.io/github/mdsumner/spdplyr?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
spdplyr
=======

`spdplyr` provides methods for the `dplyr` verbs for the `Spatial` classes in the `sp` package.

Installation
------------

Install from CRAN:

``` r
install.packages("spdplyr")
```

The development version of `spdplyr` can be installed directly from Github:

``` r
devtools::install_github("mdsumner/spdplyr")
```

dplyr verbs for Spatial
=======================

Apply `dplyr` verbs to the attribute data of `sp` objects with dplyr verbs.

See `?dplyr-Spatial` for supported verbs.
