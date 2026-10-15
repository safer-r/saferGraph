
# saferGraph <a href="">[<img src="man/figures/new_saferGraph.png" align="right" height="140" />](https://safer-r.github.io/saferGraph)</a>

<br />

<!-- badges: start -->

[![Codecov test coverage](https://codecov.io/github/safer-r/saferGraph/coverage.svg?branch=master)](https://app.codecov.io/github/safer-r/saferGraph?branch=master)
[![CRAN status](https://img.shields.io/cran/v/saferGraph.svg)](https://cran.r-project.org/package=saferGraph)
[![downloads](https://cranlogs.r-pkg.org/badges/saferGraph)](https://www.rdocumentation.org/trends)
[![](https://img.shields.io/badge/license-GPL3.0-green.svg)](https://opensource.org/licenses/MITgpl-3-0)
[![rworkflows](https://github.com/safer-r/saferGraph/actions/workflows/rworkflows.yml/badge.svg)](https://github.com/safer-r/saferGraph/actions/workflows/rworkflows.yml)
[![Safer-R Status](https://img.shields.io/badge/Safer--R%20status-backbone%20v20.0-brightgreen)](https://github.com/safer-r/.github/blob/main/profile/backbone.R)
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

Set of R functions for the development of R functions, written according to the [safer-R project](https://github.com/safer-r) specifications.

<br />

## Content
<br />

| Function | Description |
| :--- | :--- |
| **close2()** | Close only specific graphic windows (devices). |
| **inter_ticks()** | Define coordinates and values of secondary ticks. |
| **open2()** | Open a pdf or screen (GUI) graphic window and return initial graphic parameters. |
| **post_plot()** | Redesign axis. If x.side = 0, y.side = 0, the function just adds text at topright of the graph and reset par() for next graphics and provides outputs. Provide also positions for legend or additional text on the graph. |
| **scale2()** | Attempt to select nice scale numbers when setting n ticks on a lim axis range. |

<br />

Read `vignette("saferGraph")` for more details.

<br />

## Versions

The different *saferGraph* releases are tagged [here](https://github.com/safer-r/saferGraph/tags).

<br />

## Installation

*saferGraph* can be currently be installed from GitHub:

```r
install.packages("remotes")
remotes::install_github("https://github.com/safer-r/saferGraph") # or open R as admin and remotes::install_github("https://github.com/safer-r/saferGraph", lib = "C:/Program Files/R/R-4.6.0/library")
```

Older versions can be installed like this:

```r
v <- "v1.0" # desired tag version
remotes::install_github(paste0("https://github.com/safer-r/saferGraph/tree/", v))
```

<br />

## Licence

This package can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchandability or fitness for a particular purpose.
See the GNU General Public License for more details at https://www.gnu.org/licenses or in the Licence.txt attached file.

<br />

## Citation

If you are using functions of *saferGraph*, please cite: 

> Wang H, Han Y, Serizay J, Millot GA (2023). _The R saferGraph package_.
> <https://github.com/safer-r/saferGraph/>.

<br />

## Credits

[Haiding Wang](https://github.com/Tintin2710), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Yushi Han](https://github.com/yushiHn/), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Mia Legras](https://github.com/mialgrs), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Jacques Serizay](https://github.com/js2264), Spatial Regulation of Genomes team, Institut Pasteur, Paris, France

[Gael A. Millot](https://github.com/gael-millot), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

<br />

## Acknowledgements

The developers & maintainers of the mentioned softwares and packages, including:

- [R](https://www.r-project.org/)
- [Git](https://git-scm.com/)
- [Github](https://github.com/)


