\name{empty_graph}
\alias{empty_graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
empty_graph(text = NULL, text.size = 1, title = NULL, title.size = 1.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
%%     ~~Describe \code{text} here~~
}
  \item{text.size}{
%%     ~~Describe \code{text.size} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{title.size}{
%%     ~~Describe \code{title.size} here~~
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
function (text = NULL, text.size = 1, title = NULL, title.size = 1.5) 
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
    ee <- expression(argum.check = c(argum.check, tempo$problem), 
        text.check = c(text.check, tempo$text), checked.arg.names = c(checked.arg.names, 
            tempo$object.name))
    if (!is.null(text)) {
        tempo <- cuteDev::arg_check(data = text, class = "vector", 
            mode = "character", length = 1, fun.name = function.name)
        eval(ee)
    }
    tempo <- cuteDev::arg_check(data = text.size, class = "vector", 
        mode = "numeric", length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(title)) {
        tempo <- cuteDev::arg_check(data = title, class = "vector", 
            mode = "character", length = 1, fun.name = function.name)
        eval(ee)
    }
    tempo <- cuteDev::arg_check(data = title.size, class = "vector", 
        mode = "numeric", length = 1, fun.name = function.name)
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
    tempo.arg <- c("text.size", "title.size")
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
    ini.par <- par(no.readonly = TRUE)
    par(ann = FALSE, xaxt = "n", yaxt = "n", mar = rep(1, 4), 
        bty = "n", xpd = NA)
    plot(1, 1, type = "n")
    x.left.dev.region <- (par("usr")[1] - ((par("usr")[2] - par("usr")[1])/(par("plt")[2] - 
        par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1])/((par("omd")[2] - 
        par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * 
        par("omd")[1])
    y.top.dev.region <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3])/(par("plt")[4] - 
        par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - 
        par("usr")[3])/((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - 
        par("plt")[3]))) * (1 - par("omd")[4]))
    if (!is.null(text)) {
        text(x = 1, y = 1, labels = text, cex = text.size)
    }
    if (!is.null(title)) {
        text(x = x.left.dev.region, y = y.top.dev.region, labels = title, 
            adj = c(0, 1), cex = title.size)
    }
    par(ini.par)
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
