hal
================
David Benkeser, Chris Kennedy, Oleg Sofrygin

[![Travis-CI Build Status](https://travis-ci.org/benkeser/halplus.svg?branch=master)](https://travis-ci.org/benkeser/drtml)
[![AppVeyor Build  Status](https://ci.appveyor.com/api/projects/status/github/benkeser/halplus?branch=master&svg=true)](https://ci.appveyor.com/project/benkeser/survtmle)
[![CRAN](http://www.r-pkg.org/badges/version/halplus)](http://www.r-pkg.org/pkg/halplus)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![MIT license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)

Working repository for the highly adaptive lasso R package.

## Installation

Install from github using devtools:

```{r}
if (!require(devtools)) install.packages(devtools)
devtools::install_github("benkeser/halplus")
```

## Examples

To be added (see "tests" directory in the meantime).

## References

<a href = "http://ieeexplore.ieee.org/document/7796956/"> Benkeser, D. and van der Laan, M.J. (2016). "The Highly Adaptive Lasso Estimator." Proceedings of the 2016 IEEE International Conference on Data Science and Advanced Analytics. 689-696. doi: 10.1109/DSAA.2016.93. </a>

<a href = "http://biostats.bepress.com/ucbbiostat/paper343">van der Laan, Mark J. (2015). "A Generally Efficient Targeted Minimum Loss Based Estimator." U.C. Berkeley Division of Biostatistics Working Paper Series. Working Paper 343. </a>



License
-------------------
&copy; 2016-2017 [David C. Benkeser](http://www.benkeserstatistics.com)

The contents of this repository are distributed under the MIT license. See
below for details:
```
The MIT License (MIT)

Copyright (c) 2016-2017 David C. Benkeser

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