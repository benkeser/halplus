# R/`halplus`

[![Travis-CI Build Status](https://travis-ci.org/benkeser/halplus.svg?branch=master)](https://travis-ci.org/benkeser/halplus)
[![AppVeyor Build  Status](https://ci.appveyor.com/api/projects/status/github/benkeser/halplus?branch=master&svg=true)](https://ci.appveyor.com/project/benkeser/halplus)
[![Coverage Status](https://img.shields.io/codecov/c/github/benkeser/halplus/master.svg)](https://codecov.io/github/benkeser/halplus?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/halplus)](http://www.r-pkg.org/pkg/halplus)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![MIT license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)

> The highly adaptive LASSO estimator

---

## Description

`halplus` is an R package implementing the highly adaptive LASSO estimator...

---

## Installation

Install from GitHub using `devtools`:

```{r}
if (!require(devtools)) install.packages(devtools)
devtools::install_github("benkeser/halplus")
```

---

## Issues

If you encounter any bugs or have any specific feature requests, please [file an
issue](https://github.com/benkeser/halplus/issues).

---

## References

[Benkeser, D.C., and van der Laan, M.J. (2016). "The Highly Adaptive LASSO
Estimator." Working
paper.](http://ieeexplore.ieee.org/ielx7/7795280/7796876/07796956.pdf?tp=&arnumber=7796956&isnumber=7796876)

---

## License

&copy; 2016-2017 [David C. Benkeser](http://www.benkeserstatistics.com) and
[Mark J. van der Laan](https://www.stat.berkeley.edu/~laan/)

The contents of this repository are distributed under the MIT license. See
below for details:
```
The MIT License (MIT)

Copyright (c) 2016-2017 David C. Benkeser & Mark J. van der Laan

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
