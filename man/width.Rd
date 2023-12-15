\name{width}
\alias{width}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
width(categ.nb, inches.per.categ.nb = 1, ini.window.width = 7, inch.left.space, inch.right.space, boundarie.space = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{categ.nb}{
%%     ~~Describe \code{categ.nb} here~~
}
  \item{inches.per.categ.nb}{
%%     ~~Describe \code{inches.per.categ.nb} here~~
}
  \item{ini.window.width}{
%%     ~~Describe \code{ini.window.width} here~~
}
  \item{inch.left.space}{
%%     ~~Describe \code{inch.left.space} here~~
}
  \item{inch.right.space}{
%%     ~~Describe \code{inch.right.space} here~~
}
  \item{boundarie.space}{
%%     ~~Describe \code{boundarie.space} here~~
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
function (categ.nb, inches.per.categ.nb = 1, ini.window.width = 7, 
    inch.left.space, inch.right.space, boundarie.space = 0.5) 
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
    mandat.args <- c("categ.nb", "inch.left.space", "inch.right.space")
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, 
        collapse = "),missing("), "))")))
    if (any(tempo)) {
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", 
            ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, 
                collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    argum.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(argum.check = c(argum.check, tempo$problem), 
        text.check = c(text.check, tempo$text), checked.arg.names = c(checked.arg.names, 
            tempo$object.name))
    tempo <- cuteDev::arg_check(data = categ.nb, class = "vector", 
        typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
        neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = inches.per.categ.nb, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = ini.window.width, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = inch.left.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = inch.right.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = boundarie.space, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
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
    tempo.arg <- c("categ.nb", "inches.per.categ.nb", "ini.window.width", 
        "inch.left.space", "inch.right.space", "boundarie.space")
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
    range.max <- categ.nb + boundarie.space
    range.min <- boundarie.space
    window.width <- inch.left.space + inch.right.space + inches.per.categ.nb * 
        (range.max - range.min)
    return(window.width)
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
