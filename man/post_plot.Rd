\name{post_plot}
\alias{post_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = 1.5, x.label.size = 1.5, x.dist.legend = 0.5, x.nb.inter.tick = 1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = 1.5, y.label.size = 1.5, y.dist.legend = 0.5, y.nb.inter.tick = 1, text.angle = 90, tick.length = 0.5, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.side}{
%%     ~~Describe \code{x.side} here~~
}
  \item{x.log.scale}{
%%     ~~Describe \code{x.log.scale} here~~
}
  \item{x.categ}{
%%     ~~Describe \code{x.categ} here~~
}
  \item{x.categ.pos}{
%%     ~~Describe \code{x.categ.pos} here~~
}
  \item{x.lab}{
%%     ~~Describe \code{x.lab} here~~
}
  \item{x.axis.size}{
%%     ~~Describe \code{x.axis.size} here~~
}
  \item{x.label.size}{
%%     ~~Describe \code{x.label.size} here~~
}
  \item{x.dist.legend}{
%%     ~~Describe \code{x.dist.legend} here~~
}
  \item{x.nb.inter.tick}{
%%     ~~Describe \code{x.nb.inter.tick} here~~
}
  \item{y.side}{
%%     ~~Describe \code{y.side} here~~
}
  \item{y.log.scale}{
%%     ~~Describe \code{y.log.scale} here~~
}
  \item{y.categ}{
%%     ~~Describe \code{y.categ} here~~
}
  \item{y.categ.pos}{
%%     ~~Describe \code{y.categ.pos} here~~
}
  \item{y.lab}{
%%     ~~Describe \code{y.lab} here~~
}
  \item{y.axis.size}{
%%     ~~Describe \code{y.axis.size} here~~
}
  \item{y.label.size}{
%%     ~~Describe \code{y.label.size} here~~
}
  \item{y.dist.legend}{
%%     ~~Describe \code{y.dist.legend} here~~
}
  \item{y.nb.inter.tick}{
%%     ~~Describe \code{y.nb.inter.tick} here~~
}
  \item{text.angle}{
%%     ~~Describe \code{text.angle} here~~
}
  \item{tick.length}{
%%     ~~Describe \code{tick.length} here~~
}
  \item{sec.tick.length}{
%%     ~~Describe \code{sec.tick.length} here~~
}
  \item{bg.color}{
%%     ~~Describe \code{bg.color} here~~
}
  \item{grid.lwd}{
%%     ~~Describe \code{grid.lwd} here~~
}
  \item{grid.col}{
%%     ~~Describe \code{grid.col} here~~
}
  \item{corner.text}{
%%     ~~Describe \code{corner.text} here~~
}
  \item{corner.text.size}{
%%     ~~Describe \code{corner.text.size} here~~
}
  \item{just.label.add}{
%%     ~~Describe \code{just.label.add} here~~
}
  \item{par.reset}{
%%     ~~Describe \code{par.reset} here~~
}
  \item{custom.par}{
%%     ~~Describe \code{custom.par} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, 
    x.lab = "", x.axis.size = 1.5, x.label.size = 1.5, x.dist.legend = 0.5, 
    x.nb.inter.tick = 1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, 
    y.categ.pos = NULL, y.lab = "", y.axis.size = 1.5, y.label.size = 1.5, 
    y.dist.legend = 0.5, y.nb.inter.tick = 1, text.angle = 90, 
    tick.length = 0.5, sec.tick.length = 0.3, bg.color = NULL, 
    grid.lwd = NULL, grid.col = "white", corner.text = "", corner.text.size = 1, 
    just.label.add = FALSE, par.reset = FALSE, custom.par = NULL) 
{
    ini <- match.call(expand.dots = FALSE)
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    if (function.name[1] == "::()") {
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    .pack_and_function_check(fun = c("cuteDev::arg_check"), lib.path = NULL, 
        external.function.name = function.name)
    argum.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(argum.check <- c(argum.check, tempo$problem), 
        text.check <- c(text.check, tempo$text), checked.arg.names <- c(checked.arg.names, 
            tempo$object.name))
    tempo <- cuteDev::arg_check(data = x.side, options = c(0, 
        1, 3), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = x.log.scale, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(x.categ)) {
        tempo <- cuteDev::arg_check(data = x.categ, class = "character", 
            na.contain = TRUE, fun.name = function.name)
        eval(ee)
    }
    if (!is.null(x.categ.pos)) {
        tempo <- cuteDev::arg_check(data = x.categ.pos, class = "vector", 
            mode = "numeric", fun.name = function.name)
        eval(ee)
    }
    tempo <- cuteDev::arg_check(data = x.lab, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = x.axis.size, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = x.label.size, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = x.dist.legend, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = x.nb.inter.tick, class = "vector", 
        typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
        fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = y.side, options = c(0, 
        2, 4), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = y.log.scale, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(y.categ)) {
        tempo <- cuteDev::arg_check(data = y.categ, class = "character", 
            na.contain = TRUE, fun.name = function.name)
        eval(ee)
    }
    if (!is.null(y.categ.pos)) {
        tempo <- cuteDev::arg_check(data = y.categ.pos, class = "vector", 
            mode = "numeric", fun.name = function.name)
        eval(ee)
    }
    tempo <- cuteDev::arg_check(data = y.lab, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = y.axis.size, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = y.label.size, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = y.dist.legend, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = y.nb.inter.tick, class = "vector", 
        typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
        fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = text.angle, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = tick.length, class = "vector", 
        mode = "numeric", length = 1, prop = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = sec.tick.length, class = "vector", 
        mode = "numeric", length = 1, prop = TRUE, fun.name = function.name)
    eval(ee)
    if (!is.null(bg.color)) {
        tempo <- cuteDev::arg_check(data = bg.color, class = "character", 
            length = 1, fun.name = function.name)
        eval(ee)
        if (!(bg.color \%in\% colors() | grepl(pattern = "^#", 
            bg.color))) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": bg.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY colors()")
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
    }
    if (!is.null(grid.lwd)) {
        tempo <- cuteDev::arg_check(data = grid.lwd, class = "vector", 
            mode = "numeric", neg.values = FALSE, fun.name = function.name)
        eval(ee)
    }
    if (!is.null(grid.col)) {
        tempo <- cuteDev::arg_check(data = grid.col, class = "character", 
            length = 1, fun.name = function.name)
        eval(ee)
        if (!(grid.col \%in\% colors() | grepl(pattern = "^#", 
            grid.col))) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": grid.col ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY colors()")
            text.check <- c(text.check, tempo.cat)
            argum.check <- c(argum.check, TRUE)
        }
    }
    tempo <- cuteDev::arg_check(data = corner.text, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = corner.text.size, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = just.label.add, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = par.reset, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(custom.par)) {
        tempo <- cuteDev::arg_check(data = custom.par, typeof = "list", 
            length = 1, fun.name = function.name)
        eval(ee)
    }
    if (!is.null(argum.check)) {
        if (any(argum.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], 
                collapse = "\n"), "\n\n================\n\n"), 
                call. = FALSE)
        }
    }
    if (!(all(class(arg.user.setting) == "list", na.rm = TRUE) & 
        length(arg.user.setting) == 0)) {
        tempo.arg <- names(arg.user.setting)
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = length) == 1L
        if (any(tempo.log) == TRUE) {
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", 
                ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", 
                  "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], 
                  collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    tempo.arg <- c("x.side", "x.log.scale", "x.lab", "x.axis.size", 
        "x.label.size", "x.dist.legend", "x.nb.inter.tick", "y.side", 
        "y.log.scale", "y.lab", "y.axis.size", "y.label.size", 
        "y.dist.legend", "y.nb.inter.tick", "text.angle", "tick.length", 
        "sec.tick.length", "grid.col", "corner.text", "corner.text.size", 
        "just.label.add", "par.reset")
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), 
        inherit = FALSE), FUN = is.null)
    if (any(tempo.log) == TRUE) {
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", 
            ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", 
                "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], 
                collapse = "\n"), "\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    text <- NULL
    par(tcl = -par()$mgp[2] * tick.length)
    if (x.log.scale == TRUE) {
        grid.coord.x <- c(10^par("usr")[1], 10^par("usr")[2])
    }
    else {
        grid.coord.x <- c(par("usr")[1], par("usr")[2])
    }
    if (y.log.scale == TRUE) {
        grid.coord.y <- c(10^par("usr")[3], 10^par("usr")[4])
    }
    else {
        grid.coord.y <- c(par("usr")[3], par("usr")[4])
    }
    if (!is.null(bg.color)) {
        rect(grid.coord.x[1], grid.coord.y[1], grid.coord.x[2], 
            grid.coord.y[2], col = bg.color, border = NA)
    }
    if (!is.null(grid.lwd)) {
        grid(nx = NA, ny = NULL, col = grid.col, lty = 1, lwd = grid.lwd)
    }
    if (x.log.scale == TRUE) {
        x.mid.left.dev.region <- 10^(par("usr")[1] - ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            par("plt")[1] - ((par("usr")[2] - par("usr")[1])/((par("omd")[2] - 
            par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * 
            par("omd")[1]/2)
        x.left.dev.region <- 10^(par("usr")[1] - ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            par("plt")[1] - ((par("usr")[2] - par("usr")[1])/((par("omd")[2] - 
            par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * 
            par("omd")[1])
        x.mid.right.dev.region <- 10^(par("usr")[2] + ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            (1 - par("plt")[2]) + ((par("usr")[2] - par("usr")[1])/((par("omd")[2] - 
            par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * 
            (1 - par("omd")[2])/2)
        x.right.dev.region <- 10^(par("usr")[2] + ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            (1 - par("plt")[2]) + ((par("usr")[2] - par("usr")[1])/((par("omd")[2] - 
            par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * 
            (1 - par("omd")[2]))
        x.mid.left.fig.region <- 10^(par("usr")[1] - ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            par("plt")[1]/2)
        x.left.fig.region <- 10^(par("usr")[1] - ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            par("plt")[1])
        x.mid.right.fig.region <- 10^(par("usr")[2] + ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            (1 - par("plt")[2])/2)
        x.right.fig.region <- 10^(par("usr")[2] + ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            (1 - par("plt")[2]))
        x.left.plot.region <- 10^par("usr")[1]
        x.right.plot.region <- 10^par("usr")[2]
        x.mid.plot.region <- 10^((par("usr")[2] + par("usr")[1])/2)
    }
    else {
        x.mid.left.dev.region <- (par("usr")[1] - ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            par("plt")[1] - ((par("usr")[2] - par("usr")[1])/((par("omd")[2] - 
            par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * 
            par("omd")[1]/2)
        x.left.dev.region <- (par("usr")[1] - ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            par("plt")[1] - ((par("usr")[2] - par("usr")[1])/((par("omd")[2] - 
            par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * 
            par("omd")[1])
        x.mid.right.dev.region <- (par("usr")[2] + ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            (1 - par("plt")[2]) + ((par("usr")[2] - par("usr")[1])/((par("omd")[2] - 
            par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * 
            (1 - par("omd")[2])/2)
        x.right.dev.region <- (par("usr")[2] + ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            (1 - par("plt")[2]) + ((par("usr")[2] - par("usr")[1])/((par("omd")[2] - 
            par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * 
            (1 - par("omd")[2]))
        x.mid.left.fig.region <- (par("usr")[1] - ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            par("plt")[1]/2)
        x.left.fig.region <- (par("usr")[1] - ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            par("plt")[1])
        x.mid.right.fig.region <- (par("usr")[2] + ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            (1 - par("plt")[2])/2)
        x.right.fig.region <- (par("usr")[2] + ((par("usr")[2] - 
            par("usr")[1])/(par("plt")[2] - par("plt")[1])) * 
            (1 - par("plt")[2]))
        x.left.plot.region <- par("usr")[1]
        x.right.plot.region <- par("usr")[2]
        x.mid.plot.region <- (par("usr")[2] + par("usr")[1])/2
    }
    if (y.log.scale == TRUE) {
        y.mid.bottom.dev.region <- 10^(par("usr")[3] - ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            par("plt")[3] - ((par("usr")[4] - par("usr")[3])/((par("omd")[4] - 
            par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * 
            (par("omd")[3]/2))
        y.bottom.dev.region <- 10^(par("usr")[3] - ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            par("plt")[3] - ((par("usr")[4] - par("usr")[3])/((par("omd")[4] - 
            par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * 
            par("omd")[3])
        y.mid.top.dev.region <- 10^(par("usr")[4] + ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3])/((par("omd")[4] - 
            par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * 
            (1 - par("omd")[4])/2)
        y.top.dev.region <- 10^(par("usr")[4] + ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3])/((par("omd")[4] - 
            par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * 
            (1 - par("omd")[4]))
        y.mid.bottom.fig.region <- 10^(par("usr")[3] - ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            par("plt")[3]/2)
        y.bottom.fig.region <- 10^(par("usr")[3] - ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            par("plt")[3])
        y.mid.top.fig.region <- 10^(par("usr")[4] + ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            (1 - par("plt")[4])/2)
        y.top.fig.region <- 10^(par("usr")[4] + ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            (1 - par("plt")[4]))
        y.top.plot.region <- 10^par("usr")[4]
        y.bottom.plot.region <- 10^par("usr")[3]
        y.mid.plot.region <- (par("usr")[3] + par("usr")[4])/2
    }
    else {
        y.mid.bottom.dev.region <- (par("usr")[3] - ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            par("plt")[3] - ((par("usr")[4] - par("usr")[3])/((par("omd")[4] - 
            par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * 
            (par("omd")[3]/2))
        y.bottom.dev.region <- (par("usr")[3] - ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            par("plt")[3] - ((par("usr")[4] - par("usr")[3])/((par("omd")[4] - 
            par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * 
            par("omd")[3])
        y.mid.top.dev.region <- (par("usr")[4] + ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3])/((par("omd")[4] - 
            par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * 
            (1 - par("omd")[4])/2)
        y.top.dev.region <- (par("usr")[4] + ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3])/((par("omd")[4] - 
            par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * 
            (1 - par("omd")[4]))
        y.mid.bottom.fig.region <- (par("usr")[3] - ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            par("plt")[3]/2)
        y.bottom.fig.region <- (par("usr")[3] - ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            par("plt")[3])
        y.mid.top.fig.region <- (par("usr")[4] + ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            (1 - par("plt")[4])/2)
        y.top.fig.region <- (par("usr")[4] + ((par("usr")[4] - 
            par("usr")[3])/(par("plt")[4] - par("plt")[3])) * 
            (1 - par("plt")[4]))
        y.top.plot.region <- par("usr")[4]
        y.bottom.plot.region <- par("usr")[3]
        y.mid.plot.region <- ((par("usr")[3] + par("usr")[4])/2)
    }
    if (any(sapply(FUN = all.equal, c(1, 3), x.side) == TRUE, 
        na.rm = TRUE)) {
        par(xpd = FALSE, xaxt = "s")
        if (is.null(x.categ) & x.log.scale == TRUE) {
            if (any(par()$xaxp[1:2] == 0L, na.rm = TRUE)) {
                if (par()$xaxp[1] == 0L) {
                  par(xaxp = c(10^-30, par()$xaxp[2:3]))
                }
                if (par()$xaxp[2] == 0L) {
                  par(xaxp = c(par()$xaxp[1], 10^-30, par()$xaxp[3]))
                }
            }
            axis(side = x.side, at = c(10^par()$usr[1], 10^par()$usr[2]), 
                labels = rep("", 2), lwd = 1, lwd.ticks = 0)
            mtext(side = x.side, text = x.lab, line = x.dist.legend/0.2, 
                las = 0, cex = x.label.size)
            par(tcl = -par()$mgp[2] * sec.tick.length)
            suppressWarnings(rug(10^outer(c((log10(par("xaxp")[1]) - 
                1):log10(par("xaxp")[2])), log10(1:10), "+"), 
                ticksize = NA, side = x.side))
            par(tcl = -par()$mgp[2] * tick.length)
            axis(side = x.side, at = c(1e-15, 1e-14, 1e-13, 1e-12, 
                1e-11, 1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 
                1e-04, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 
                1e+05, 1e+06, 1e+07, 1e+08, 1e+09, 1e+10), labels = expression(10^-15, 
                10^-14, 10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 
                10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 
                10^-1, 10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 
                10^7, 10^8, 10^9, 10^10), lwd = 0, lwd.ticks = 1, 
                cex.axis = x.axis.size)
            x.text <- 10^par("usr")[2]
        }
        else if (is.null(x.categ) & x.log.scale == FALSE) {
            axis(side = x.side, at = c(par()$usr[1], par()$usr[2]), 
                labels = rep("", 2), lwd = 1, lwd.ticks = 0)
            axis(side = x.side, at = round(seq(par()$xaxp[1], 
                par()$xaxp[2], length.out = par()$xaxp[3] + 1), 
                2), cex.axis = x.axis.size)
            mtext(side = x.side, text = x.lab, line = x.dist.legend/0.2, 
                las = 0, cex = x.label.size)
            if (x.nb.inter.tick > 0) {
                inter.tick.unit <- (par("xaxp")[2] - par("xaxp")[1])/par("xaxp")[3]
                par(tcl = -par()$mgp[2] * sec.tick.length)
                suppressWarnings(rug(seq(par("xaxp")[1] - 10 * 
                  inter.tick.unit, par("xaxp")[2] + 10 * inter.tick.unit, 
                  by = inter.tick.unit/(1 + x.nb.inter.tick)), 
                  ticksize = NA, x.side))
                par(tcl = -par()$mgp[2] * tick.length)
            }
            x.text <- par("usr")[2]
        }
        else if ((!is.null(x.categ)) & x.log.scale == FALSE) {
            if (is.null(x.categ.pos)) {
                x.categ.pos <- 1:length(x.categ)
            }
            else if (length(x.categ.pos) != length(x.categ)) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": x.categ.pos MUST BE THE SAME LENGTH AS x.categ")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            par(xpd = TRUE)
            if (isTRUE(all.equal(x.side, 1))) {
                segments(x0 = x.left.plot.region, x1 = x.right.plot.region, 
                  y0 = y.bottom.plot.region, y1 = y.bottom.plot.region)
                text(x = x.categ.pos, y = y.mid.bottom.fig.region, 
                  labels = x.categ, srt = text.angle, cex = x.axis.size)
            }
            else if (isTRUE(all.equal(x.side, 3))) {
                segments(x0 = x.left.plot.region, x1 = x.right.plot.region, 
                  y0 = y.top.plot.region, y1 = y.top.plot.region)
                text(x = x.categ.pos, y = y.mid.top.fig.region, 
                  labels = x.categ, srt = text.angle, cex = x.axis.size)
            }
            else {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": ARGUMENT x.side CAN ONLY BE 1 OR 3")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            par(xpd = FALSE)
            x.text <- par("usr")[2]
        }
        else {
            tempo.cat <- paste0("ERROR IN ", function.name, ": PROBLEM WITH THE x.side (", 
                x.side, ") OR x.log.scale (", x.log.scale, ") ARGUMENTS")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    else {
        x.text <- par("usr")[2]
    }
    if (any(sapply(FUN = all.equal, c(2, 4), y.side) == TRUE, 
        na.rm = TRUE)) {
        par(xpd = FALSE, yaxt = "s")
        if (is.null(y.categ) & y.log.scale == TRUE) {
            if (any(par()$yaxp[1:2] == 0L, na.rm = TRUE)) {
                if (par()$yaxp[1] == 0L) {
                  par(yaxp = c(10^-30, par()$yaxp[2:3]))
                }
                if (par()$yaxp[2] == 0L) {
                  par(yaxp = c(par()$yaxp[1], 10^-30, par()$yaxp[3]))
                }
            }
            axis(side = y.side, at = c(10^par()$usr[3], 10^par()$usr[4]), 
                labels = rep("", 2), lwd = 1, lwd.ticks = 0)
            par(tcl = -par()$mgp[2] * sec.tick.length)
            suppressWarnings(rug(10^outer(c((log10(par("yaxp")[1]) - 
                1):log10(par("yaxp")[2])), log10(1:10), "+"), 
                ticksize = NA, side = y.side))
            par(tcl = -par()$mgp[2] * tick.length)
            axis(side = y.side, at = c(1e-15, 1e-14, 1e-13, 1e-12, 
                1e-11, 1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 
                1e-04, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 
                1e+05, 1e+06, 1e+07, 1e+08, 1e+09, 1e+10), labels = expression(10^-15, 
                10^-14, 10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 
                10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 
                10^-1, 10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 
                10^7, 10^8, 10^9, 10^10), lwd = 0, lwd.ticks = 1, 
                cex.axis = y.axis.size)
            y.text <- 10^(par("usr")[4] + (par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
                par("plt")[3]) * (1 - par("plt")[4]))
            mtext(side = y.side, text = y.lab, line = y.dist.legend/0.2, 
                las = 0, cex = y.label.size)
        }
        else if (is.null(y.categ) & y.log.scale == FALSE) {
            axis(side = y.side, at = c(par()$usr[3], par()$usr[4]), 
                labels = rep("", 2), lwd = 1, lwd.ticks = 0)
            axis(side = y.side, at = round(seq(par()$yaxp[1], 
                par()$yaxp[2], length.out = par()$yaxp[3] + 1), 
                2), cex.axis = y.axis.size)
            mtext(side = y.side, text = y.lab, line = y.dist.legend/0.2, 
                las = 0, cex = y.label.size)
            if (y.nb.inter.tick > 0) {
                inter.tick.unit <- (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3]
                par(tcl = -par()$mgp[2] * sec.tick.length)
                suppressWarnings(rug(seq(par("yaxp")[1] - 10 * 
                  inter.tick.unit, par("yaxp")[2] + 10 * inter.tick.unit, 
                  by = inter.tick.unit/(1 + y.nb.inter.tick)), 
                  ticksize = NA, side = y.side))
                par(tcl = -par()$mgp[2] * tick.length)
            }
            y.text <- (par("usr")[4] + (par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
                par("plt")[3]) * (1 - par("plt")[4]))
        }
        else if ((!is.null(y.categ)) & y.log.scale == FALSE) {
            if (is.null(y.categ.pos)) {
                y.categ.pos <- 1:length(y.categ)
            }
            else if (length(y.categ.pos) != length(y.categ)) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": y.categ.pos MUST BE THE SAME LENGTH AS y.categ")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            axis(side = y.side, at = y.categ.pos, labels = rep("", 
                length(y.categ)), lwd = 0, lwd.ticks = 1)
            par(xpd = TRUE)
            if (isTRUE(all.equal(y.side, 2))) {
                text(x = x.mid.left.fig.region, y = y.categ.pos, 
                  labels = y.categ, srt = text.angle, cex = y.axis.size)
            }
            else if (isTRUE(all.equal(y.side, 4))) {
                text(x = x.mid.right.fig.region, y = y.categ.pos, 
                  labels = y.categ, srt = text.angle, cex = y.axis.size)
            }
            else {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": ARGUMENT y.side CAN ONLY BE 2 OR 4")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            par(xpd = FALSE)
            y.text <- (par("usr")[4] + (par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
                par("plt")[3]) * (1 - par("plt")[4]))
        }
        else {
            tempo.cat <- paste0("ERROR IN ", function.name, ": PROBLEM WITH THE y.side (", 
                y.side, ") OR y.log.scale (", y.log.scale, ") ARGUMENTS")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    else {
        y.text <- (par("usr")[4] + (par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
            par("plt")[3]) * (1 - par("plt")[4]))
    }
    par(xpd = NA)
    text(x = x.mid.right.fig.region, y = y.text, corner.text, 
        adj = c(1, 1.1), cex = corner.text.size)
    if (just.label.add == TRUE & isTRUE(all.equal(x.side, 0)) & 
        x.lab != "") {
        text(x = x.mid.plot.region, y = y.mid.bottom.fig.region, 
            x.lab, adj = c(0.5, 0.5), cex = x.label.size)
    }
    if (just.label.add == TRUE & isTRUE(all.equal(y.side, 0)) & 
        y.lab != "") {
        text(x = y.mid.plot.region, y = x.mid.left.fig.region, 
            y.lab, adj = c(0.5, 0.5), cex = y.label.size)
    }
    par(xpd = FALSE)
    if (par.reset == TRUE) {
        tempo.par <- open(pdf = FALSE, return.output = TRUE)
        invisible(dev.off())
        if (!is.null(custom.par)) {
            if (!names(custom.par) \%in\% names(tempo.par$ini.par)) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": custom.par ARGUMENT SHOULD HAVE THE NAMES OF THE COMPARTMENT LIST COMING FROM THE par() LIST")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            par(custom.par)
            text <- c(text, "\nGRAPH PARAMETERS SET TO VALUES DEFINED BY custom.par ARGUMENT\n")
        }
        else {
            par(tempo.par$ini.par)
            text <- c(text, "\nGRAPH PARAMETERS RESET TO par() DEFAULT VALUES\n")
        }
    }
    output <- list(x.mid.left.dev.region = x.mid.left.dev.region, 
        x.left.dev.region = x.left.dev.region, x.mid.right.dev.region = x.mid.right.dev.region, 
        x.right.dev.region = x.right.dev.region, x.mid.left.fig.region = x.mid.left.fig.region, 
        x.left.fig.region = x.left.fig.region, x.mid.right.fig.region = x.mid.right.fig.region, 
        x.right.fig.region = x.right.fig.region, x.left.plot.region = x.left.plot.region, 
        x.right.plot.region = x.right.plot.region, x.mid.plot.region = x.mid.plot.region, 
        y.mid.bottom.dev.region = y.mid.bottom.dev.region, y.bottom.dev.region = y.bottom.dev.region, 
        y.mid.top.dev.region = y.mid.top.dev.region, y.top.dev.region = y.top.dev.region, 
        y.mid.bottom.fig.region = y.mid.bottom.fig.region, y.bottom.fig.region = y.bottom.fig.region, 
        y.mid.top.fig.region = y.mid.top.fig.region, y.top.fig.region = y.top.fig.region, 
        y.top.plot.region = y.top.plot.region, y.bottom.plot.region = y.bottom.plot.region, 
        y.mid.plot.region = y.mid.plot.region, text = text)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
