# covid19

<!-- badges: start -->
[![Build Status](https://travis-ci.org/msaltieri/covid19.svg?branch=master)](https://travis-ci.org/msaltieri/covid19)
[![Version](https://img.shields.io/badge/version-v1.4.2-blue.svg)](#)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

The R package **covid19** is used to retrieve, analyse and represent
Italian data on the COVID19 outbreak and spreading.

It uses official data issued by the Italian Government through the
[COVID-19](https://github.com/pcm-dpc/COVID-19) repository.

Data are represented using
[flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/), an easy and
interactive dashboard tool for R.

The package **covid19** implements a time-dependent SIR model to predict the
epidemic evolution.

## Dashboard

An online version of the dashboard is available here:
https://covid19.hypermynds.com.

## Getting Started

**covid19** is not available on CRAN yet but it can be downloaded through the
`remotes` package:

```r
remotes::install_github('msaltieri/covid19')
library(covid19)
```

The local dashboard is opened with:

```r
covid19::launch()
```

A new `Codiv` object is initialized by:

```r
covid <- Covid$new()
```

Historical data can be displayed into a `tibble` by:

```r
covid$get()
```

Epidemic projections can be calculated via:

```r
covid$sir()
```

## Documentation

The `Covid` R6 class is documented by Roxygen. To have further information on
all the available methods take a look at the manual:

```r
?covid19::Covid
```
