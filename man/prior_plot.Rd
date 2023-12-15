\name{prior_plot}
\alias{prior_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prior_plot(param.reinitial = FALSE, xlog.scale = FALSE, ylog.scale = FALSE, remove.label = TRUE, remove.x.axis = TRUE, remove.y.axis = TRUE, std.x.range = TRUE, std.y.range = TRUE, down.space = 1, left.space = 1, up.space = 1, right.space = 1, orient = 1, dist.legend = 3.5, tick.length = 0.5, box.type = "n", amplif.label = 1, amplif.axis = 1, display.extend = FALSE, return.par = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param.reinitial}{
%%     ~~Describe \code{param.reinitial} here~~
}
  \item{xlog.scale}{
%%     ~~Describe \code{xlog.scale} here~~
}
  \item{ylog.scale}{
%%     ~~Describe \code{ylog.scale} here~~
}
  \item{remove.label}{
%%     ~~Describe \code{remove.label} here~~
}
  \item{remove.x.axis}{
%%     ~~Describe \code{remove.x.axis} here~~
}
  \item{remove.y.axis}{
%%     ~~Describe \code{remove.y.axis} here~~
}
  \item{std.x.range}{
%%     ~~Describe \code{std.x.range} here~~
}
  \item{std.y.range}{
%%     ~~Describe \code{std.y.range} here~~
}
  \item{down.space}{
%%     ~~Describe \code{down.space} here~~
}
  \item{left.space}{
%%     ~~Describe \code{left.space} here~~
}
  \item{up.space}{
%%     ~~Describe \code{up.space} here~~
}
  \item{right.space}{
%%     ~~Describe \code{right.space} here~~
}
  \item{orient}{
%%     ~~Describe \code{orient} here~~
}
  \item{dist.legend}{
%%     ~~Describe \code{dist.legend} here~~
}
  \item{tick.length}{
%%     ~~Describe \code{tick.length} here~~
}
  \item{box.type}{
%%     ~~Describe \code{box.type} here~~
}
  \item{amplif.label}{
%%     ~~Describe \code{amplif.label} here~~
}
  \item{amplif.axis}{
%%     ~~Describe \code{amplif.axis} here~~
}
  \item{display.extend}{
%%     ~~Describe \code{display.extend} here~~
}
  \item{return.par}{
%%     ~~Describe \code{return.par} here~~
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
function (param.reinitial = FALSE, xlog.scale = FALSE, ylog.scale = FALSE, 
    remove.label = TRUE, remove.x.axis = TRUE, remove.y.axis = TRUE, 
    std.x.range = TRUE, std.y.range = TRUE, down.space = 1, left.space = 1, 
    up.space = 1, right.space = 1, orient = 1, dist.legend = 3.5, 
    tick.length = 0.5, box.type = "n", amplif.label = 1, amplif.axis = 1, 
    display.extend = FALSE, return.par = FALSE) 
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
    tempo <- cuteDev::arg_check(data = param.reinitial, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = xlog.scale, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = ylog.scale, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = remove.label, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = remove.x.axis, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = remove.y.axis, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = std.x.range, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = std.y.range, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = down.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = left.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = up.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = right.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = orient, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = dist.legend, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = tick.length, class = "vector", 
        mode = "numeric", length = 1, prop = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = box.type, options = c("o", 
        "l", "7", "c", "u", "]", "n"), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = amplif.label, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = amplif.axis, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = display.extend, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = return.par, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
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
    tempo.arg <- c("param.reinitial", "xlog.scale", "ylog.scale", 
        "remove.label", "remove.x.axis", "remove.y.axis", "std.x.range", 
        "std.y.range", "down.space", "left.space", "up.space", 
        "right.space", "orient", "dist.legend", "tick.length", 
        "box.type", "amplif.label", "amplif.axis", "display.extend", 
        "return.par")
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
    if (is.null(dev.list())) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THIS FUNCTION CANNOT BE USED IF NO GRAPHIC DEVICE ALREADY OPENED (dev.list() IS CURRENTLY NULL)")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (param.reinitial == TRUE) {
        if (!all(names(dev.cur()) == "null device", na.rm = TRUE)) {
            active.wind.nb <- dev.cur()
        }
        else {
            active.wind.nb <- 0
        }
        if (Sys.info()["sysname"] == "Windows") {
            grDevices::windows()
            ini.par <- par(no.readonly = FALSE)
            invisible(dev.off())
        }
        else if (Sys.info()["sysname"] == "Linux") {
            if (file.exists(paste0(getwd(), "/Rplots.pdf"))) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": THIS FUNCTION CANNOT BE USED ON LINUX WITH param.reinitial SET TO TRUE IF A Rplots.pdf FILE ALREADY EXISTS HERE: ", 
                  getwd())
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            else {
                open.fail <- suppressWarnings(try(grDevices::X11(), 
                  silent = TRUE))[]
                if (is.null(open.fail)) {
                  ini.par <- par(no.readonly = FALSE)
                  invisible(dev.off())
                }
                else if (file.exists(paste0(getwd(), "/Rplots.pdf"))) {
                  ini.par <- par(no.readonly = FALSE)
                  invisible(dev.off())
                  file.remove(paste0(getwd(), "/Rplots.pdf"))
                }
                else {
                  tempo.cat <- ("ERROR IN function.name\nTHIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM\nTO OVERCOME THIS, PLEASE USE A PDF GRAPHIC INTERFACE AND RERUN")
                  stop(paste0("\n\n================\n\n", tempo.cat, 
                    "\n\n================\n\n"), call. = FALSE)
                }
            }
        }
        else {
            grDevices::quartz()
            ini.par <- par(no.readonly = FALSE)
            invisible(dev.off())
        }
        if (!all(names(dev.cur()) == "null device", na.rm = TRUE)) {
            invisible(dev.set(active.wind.nb))
            par(ini.par)
        }
    }
    if (remove.x.axis == TRUE) {
        par(xaxt = "n")
    }
    else {
        par(xaxt = "s")
    }
    if (remove.y.axis == TRUE) {
        par(yaxt = "n")
    }
    else {
        par(yaxt = "s")
    }
    if (std.x.range == TRUE) {
        par(xaxs = "i")
    }
    else {
        par(xaxs = "r")
    }
    if (std.y.range == TRUE) {
        par(yaxs = "i")
    }
    else {
        par(yaxs = "r")
    }
    par(mai = c(down.space, left.space, up.space, right.space), 
        ann = !remove.label, las = orient, mgp = c(dist.legend/0.2, 
            1, 0), xpd = display.extend, bty = box.type, cex.lab = amplif.label, 
        cex.axis = amplif.axis)
    par(tcl = -par()$mgp[2] * tick.length)
    if (xlog.scale == TRUE) {
        par(xaxt = "n", xlog = TRUE)
    }
    else {
        par(xlog = FALSE)
    }
    if (ylog.scale == TRUE) {
        par(yaxt = "n", ylog = TRUE)
    }
    else {
        par(ylog = FALSE)
    }
    if (return.par == TRUE) {
        tempo.par <- par()
        return(tempo.par)
    }
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
