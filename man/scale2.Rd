% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale2.R
\name{scale2}
\alias{scale2}
\title{scale2}
\usage{
scale2(n, lim, kind = "approx", lib.path = NULL, safer_check = TRUE)
}
\arguments{
\item{n}{Single positive and non null integer value indicating the desired number of main ticks on the axis.}

\item{lim}{Vector of 2 numbers indicating the limit range of the axis. Order of the 2 values matters (for inverted axis). Can be log transformed values.}

\item{kind}{Single character string. Either "approx" (approximative), "strict" (strict) or "strict.cl" (strict clean). If "approx", use the scales::trans_breaks() function to provide an easy to read scale of approximately n ticks spanning the range of the lim argument. If "strict", cut the range of the lim argument into n + 1 equidistant part and return the n numbers at each boundary. This often generates numbers uneasy to read. If "strict.cl", provide an easy to read scale of exactly n ticks, but sometimes not completely spanning the range of the lim argument.}

\item{lib.path}{Character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.}

\item{safer_check}{Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.}
}
\value{
A vector of numbers.
}
\description{
Attempt to select nice scale numbers when setting n ticks on a lim axis range.
}
\examples{
# approximate number of main ticks

ymin = 2 ; 
ymax = 3.101 ; 
n = 5 ; 
scale <- scale2(n = n, lim = c(ymin, ymax), kind = "approx") ; 
scale ; 
par(yaxt = "n", yaxs = "i", las = 1) ; 
plot(ymin:ymax, ymin:ymax, xlim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ;
par(yaxt = "s") ; 
axis(side = 2, at = scale)
}
\author{
Gael Millot <gael.millot@pasteur.fr>

Yushi Han <yushi.han2000@gmail.com>

Haiding Wang <wanghaiding442@gmail.com>
}
