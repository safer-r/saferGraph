% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/width.R
\name{width}
\alias{width}
\title{width}
\usage{
width(
  categ.nb,
  inches.per.categ.nb = 1,
  ini.window.width = 7,
  inch.left.space,
  inch.right.space,
  boundarie.space = 0.5
)
}
\arguments{
\item{categ.nb}{Single positive numeric value of number of categories to plot.}

\item{inches.per.categ.nb}{Single positive numeric value of inches per unit of categ.nb. 2 means 2 inches for each boxplot for instance.}

\item{ini.window.width}{Single positive numeric value indicating the initial Window width (in inches).}

\item{inch.left.space}{Single positive numeric value indicating the left horizontal margin of the figure region (in inches).}

\item{inch.right.space}{Single positive numeric value indicating the right horizontal margin of the figure region (in inches).}

\item{boundarie.space}{Single positive numeric value indicating the space between the right and left limits of the plotting region and the plot (0.5 means half a category width). It is as if empty categories were added on the right and left of the plot.}
}
\value{
The new window width in inches.
}
\description{
Rescale the width of a window to open depending on the number of categories of qualitative variable to plot on the x-axis.
Can be used for height, considering that it is as if it was a width.

This order can be used:

width()

open()

prior_plot() # not for ggplot2

plot() or any other plotting

post_plot() if prior_plot() has been used # not for ggplot2

close()
}
\examples{
width(categ.nb = 10, inches.per.categ.nb = 0.2, ini.window.width = 7, inch.left.space = 1, inch.right.space = 1, boundarie.space = 0.5)
}
