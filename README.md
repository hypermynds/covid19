# covid19

<!-- badges: start -->
[![Build](https://img.shields.io/badge/build-passing-success.svg)](#)
[![Version](https://img.shields.io/badge/version-v0.1.0-blue.svg)](#)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The R package **covid19** is used to retrieve, analyse and represent
Italian data on the COVID19 outbreak and spreading.

It uses official data issued by the Italian Government through the
[COVID19](https://github.com/pcm-dpc/COVID-19) repository.

## Installation

**covid19** is not available on CRAN yet but it can be downloaded through the
**remotes** package:

```r
remotes::install_github('msaltieri/covid19')
```

To open the dashboard, simply go with:

```r
covid19::launch()
```
