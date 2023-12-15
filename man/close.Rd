\name{close}
\alias{close}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
close(kind = "pdf", return.text = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kind}{
%%     ~~Describe \code{kind} here~~
}
  \item{return.text}{
%%     ~~Describe \code{return.text} here~~
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
function (kind = "pdf", return.text = FALSE) 
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
    tempo <- cuteDev::arg_check(data = kind, options = c("windows", 
        "quartz", "x11", "X11", "pdf", "bmp", "png", "tiff"), 
        fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = return.text, class = "logical", 
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
    tempo.arg <- c("kind", "return.text")
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
    text <- paste0("THE REQUIRED KIND OF GRAPHIC DEVICES TO CLOSE ARE ", 
        paste(kind, collapse = " "))
    if (Sys.info()["sysname"] == "Windows") {
        if (any(kind \%in\% c("windows", "quartz", "x11", "X11"))) {
            tempo <- kind \%in\% c("windows", "quartz", "x11", 
                "X11")
            kind[tempo] <- "windows"
        }
    }
    else if (Sys.info()["sysname"] == "Linux") {
        if (any(kind \%in\% c("windows", "quartz", "x11", "X11"))) {
            tempo.device <- suppressWarnings(try(X11(), silent = TRUE))[]
            if (!is.null(tempo.device)) {
                text <- paste0(text, "\nCANNOT CLOSE GUI GRAPHIC DEVICES AS REQUIRED BECAUSE THIS LINUX SYSTEM DOES NOT HAVE IT")
            }
            else {
                tempo <- kind \%in\% c("windows", "quartz", "x11", 
                  "X11")
                kind[tempo] <- names(dev.list()[length(dev.list())])
                invisible(dev.off())
            }
        }
    }
    else {
        if (any(kind \%in\% c("windows", "quartz", "x11", "X11"))) {
            tempo <- kind \%in\% c("windows", "quartz", "x11", 
                "X11")
            kind[tempo] <- "quartz"
        }
    }
    kind <- unique(kind)
    if (length(dev.list()) != 0) {
        for (i in length(names(dev.list())):1) {
            if (names(dev.list())[i] \%in\% kind) {
                text <- paste0(text, "\n", names(dev.list())[i], 
                  " DEVICE NUMBER ", dev.list()[i], " HAS BEEN CLOSED")
                invisible(dev.off(dev.list()[i]))
            }
        }
    }
    if (return.text == TRUE) {
        return(text)
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
