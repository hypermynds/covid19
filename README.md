# covid19

<!-- badges: start -->
[![Build](https://img.shields.io/badge/build-passing-success.svg)](#)
[![Version](https://img.shields.io/badge/version-v1.0.0-blue.svg)](#)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The R package **covid19** is used to retrieve, analyse and represent
Italian data on the COVID19 outbreak and spreading.

It uses official data issued by the Italian Government through the
[COVID19](https://github.com/pcm-dpc/COVID-19) repository.

Data are represented using [flexdashboard](https://rmarkdown.rstudio.com/flexdashboard/),
an easy and interactive dashboard tool for R.

## Dashboard

An online version of the dashboard is available here:
https://msaltieri.shinyapps.io/covid19.

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
covid <- Covid$new()s
```

Launch the `update` method in order to download the last available data from
the remote repository:

```r
covid$update()
```

Downloaded data can be displayed into a `tibble`:

```r
covid$get()
```

## Documentation

The `Covid` R6 class is documented by Roxygen. To have further information on
all the available methods:

```r
?covid19::Covid
```
