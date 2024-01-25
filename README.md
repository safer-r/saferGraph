
# cuteGraph <a href="">[<img src="man/figures/logo.png" align="right" height="140" />](https://yushihn.github.io/cuteGraph)</a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/yushiHn/cuteGraph/workflows/R-CMD-check/badge.svg)](https://github.com/yushiHn/cuteGraph/actions)

[![Codecov test coverage](https://codecov.io/github/yushiHn/cuteGraph/coverage.svg?branch=master)](https://app.codecov.io/github/yushiHn/cuteGraph?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/cuteGraph)](https://cran.r-project.org/package=cuteGraph)
[![downloads](https://cranlogs.r-pkg.org/badges/cuteGraph)](https://www.rdocumentation.org/trends)
[![](https://img.shields.io/badge/license-GPL3.0-green.svg)](https://opensource.org/licenses/MITgpl-3-0)
[![rworkflows](https://github.com/yushiHn/cuteGraph/actions/workflows/rworkflows.yml/badge.svg)](https://github.com/yushiHn/cuteGraph/actions/workflows/rworkflows.yml)
<!-- badges: end -->

<br />

## Table of content

   - [Description](#description)
   - [Content](#content)
   - [Versions](#versions)
   - [Installation](#installation)
   - [Licence](#licence)
   - [Citations](#citations)
   - [Credits](#credits)
   - [Acknowledgements](#acknowledgements)

<br />

## Description

Set of R functions for the development of R functions, written according to the [cute_project](https://github.com/gael-millot/cute_project) specifications.

<br />

## Content

| Function | Description |
| --- | --- |
| **close()** | Close only specific graphic windows (devices). |
| **empty_graph()** | Display an empty plot with a text in the middle of the window (for instance to specify that no plot can be drawn). |
| **inter_ticks()** | Verify that object names in the environment defined by the pos argument are identical or not to object names in the above environments (following R Scope). This can be used to verify that names used for objects inside a function or in the working environment do not override names of objects already present in the above R environments, following the R scope. |
| **open()** | Open a pdf or screen (GUI) graphic window and return initial graphic parameters. |
| **post_plot()** | Redesign axis. If x.side = 0, y.side = 0, the function just adds text at topright of the graph and reset par() for next graphics and provides outputs. Provide also positions for legend or additional text on the graph. Use prior_plot() before this function for initial inactivation of the axis drawings. |
| **prior_plot()** | Very convenient to erase the axes for post plot axis redrawing using post_plot(). Reinitialize and set the graphic parameters before plotting. CANNOT be used if no graphic device already opened. |
| **scale()** | Attempt to select nice scale numbers when setting n ticks on a lim axis range. |
| **width()** | Rescale the width of a window to open depending on the number of categories of qualitative variable to plot on the x-axis. Can be used for height, considering that it is as if it was a width. |

<br />

This order can be used:

- width()
- open()
- prior_plot() # not for ggplot2
- plot() or any other plotting
- post_plot() if prior_plot() has been used # not for ggplot2
- close()

<br />
Read `vignette("cuteGraph")` for more details.

<br />

## Versions

The different *cuteGraph* releases are tagged [here](https://github.com/yushiHn/cuteGraph/tags).

<br />

## Installation

*cuteGraph* can be currently be installed from GitHub:

```r
install.packages("remotes")
remotes::install_github("https://github.com/yushiHn/cuteGraph")
```

Older versions can be installed like this:

```r
v <- "v1.0" # desired tag version
remotes::install_github(paste0("https://github.com/yushiHn/cuteGraph/tree/", v))
```

<br />

## Licence

This package can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchandability or fitness for a particular purpose.
See the GNU General Public License for more details at https://www.gnu.org/licenses.

<br />

## Citation

If you are using functions of *cuteGraph*, please cite: 

> Han Y, Serizay J, Millot GA (2023). _The R cuteGraph package_.
> <https://github.com/yushiHn/cuteGraph/>.

<br />

## Credits

[Yushi Han](https://github.com/yushiHn/), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Jacques Serizai](https://github.com/js2264), Spatial Regulation of Genomes team, Institut Pasteur, Paris, France

[Gael A. Millot](https://gitlab.pasteur.fr/gmillot), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

<br />

## Acknowledgements

The developers & maintainers of [R](https://www.r-project.org/) as well as packages used in the *cuteGraph* functions.

