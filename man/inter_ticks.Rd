\name{inter_ticks}
\alias{inter_ticks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inter_ticks(lim, log = "log10", breaks = NULL, n = NULL, warn.print = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lim}{
%%     ~~Describe \code{lim} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{warn.print}{
%%     ~~Describe \code{warn.print} here~~
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
function (lim, log = "log10", breaks = NULL, n = NULL, warn.print = TRUE) 
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
    mandat.args <- c("lim")
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
    tempo <- cuteDev::arg_check(data = lim, class = "vector", 
        mode = "numeric", length = 2, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = log, options = c("no", 
        "log2", "log10"), length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(breaks)) {
        tempo <- cuteDev::arg_check(data = breaks, class = "vector", 
            mode = "numeric", fun.name = function.name)
        eval(ee)
    }
    if (!is.null(n)) {
        tempo <- cuteDev::arg_check(data = n, class = "vector", 
            typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
            fun.name = function.name)
        eval(ee)
    }
    tempo <- cuteDev::arg_check(data = warn.print, class = "vector", 
        mode = "logical", length = 1, fun.name = function.name)
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
    tempo.arg <- c("lim", "log", "warn.print")
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
    if (all(diff(lim) == 0L, na.rm = TRUE)) {
        tempo.cat <- paste0("ERROR IN ", function.name, "\nlim ARGUMENT HAS A NULL RANGE (2 IDENTICAL VALUES): ", 
            paste(lim, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    else if (any(lim \%in\% c(Inf, -Inf))) {
        tempo.cat <- paste0("ERROR IN ", function.name, "\nlim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (log == "no" & is.null(breaks)) {
        tempo.cat <- paste0("ERROR IN ", function.name, "\nbreaks ARGUMENT CANNOT BE NULL IF log ARGUMENT IS \"no\"")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!is.null(breaks)) {
        if (length(breaks) < 2) {
            tempo.cat <- paste0("ERROR IN ", function.name, "\nbreaks ARGUMENT MUST HAVE 2 VALUES AT LEAST (OTHERWISE, INTER TICK POSITIONS CANNOT BE COMPUTED): ", 
                paste(breaks, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (!isTRUE(all.equal(diff(sort(breaks)), rep(diff(sort(breaks))[1], 
            length(diff(sort(breaks))))))) {
            tempo.cat <- paste0("ERROR IN ", function.name, "\nbreaks ARGUMENT MUST HAVE EQUIDISTANT VALUES (OTHERWISE, EQUAL NUMBER OF INTER TICK BETWEEN MAIN TICKS CANNOT BE COMPUTED): ", 
                paste(breaks, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (!is.null(n)) {
        if (n <= 0) {
            tempo.cat <- paste0("ERROR IN ", function.name, "\nn ARGUMENT MUST BE A POSITIVE AND NON NULL INTEGER: ", 
                paste(n, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    lim.rank <- rank(lim)
    if (log != "no") {
        ini.scipen <- options()$scipen
        options(scipen = -1000)
        power10.exp <- as.integer(substring(text = 10^lim, first = (regexpr(pattern = "\\+|\\-", 
            text = 10^lim))))
        options(scipen = ini.scipen)
        tick.pos <- unique(as.vector(outer(2:10, ifelse(log == 
            "log2", 2, 10)^((power10.exp[1] - ifelse(diff(lim.rank) > 
            0, 1, -1)):(power10.exp[2] + ifelse(diff(lim.rank) > 
            0, 1, -1))))))
        tick.pos <- sort(tick.pos, decreasing = ifelse(diff(lim.rank) > 
            0, FALSE, TRUE))
        if (log == "log2") {
            tick.values <- tick.pos[tick.pos >= min(2^lim) & 
                tick.pos <= max(2^lim)]
            tick.pos <- log2(tick.values)
        }
        else if (log == "log10") {
            tick.values <- tick.pos[tick.pos >= min(10^lim) & 
                tick.pos <= max(10^lim)]
            tick.pos <- log10(tick.values)
        }
    }
    else {
        breaks.rank <- rank(c(breaks[1], breaks[length(breaks)]))
        if (diff(breaks.rank) != diff(lim.rank)) {
            breaks <- sort(breaks, decreasing = ifelse(diff(lim.rank) < 
                0, TRUE, FALSE))
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count, ") VALUES IN breaks ARGUMENT NOT IN THE SAME ORDER AS IN lim ARGUMENT -> VALUES REORDERED AS IN lim: ", 
                paste(breaks, collapse = " "))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                paste0(warn, "\n\n", tempo.warn)))
            breaks.rank <- rank(c(breaks[1], breaks[length(breaks)]))
        }
        main.tick.dist <- mean(diff(breaks), na.rm = TRUE)
        tick.dist <- main.tick.dist/(n + 1)
        tempo.extra.margin <- max(abs(diff(breaks)), na.rm = TRUE)
        tick.pos <- seq(if (diff(breaks.rank) > 0) {
            breaks[1] - tempo.extra.margin
        }
        else {
            breaks[1] + tempo.extra.margin
        }, if (diff(breaks.rank) > 0) {
            breaks[length(breaks)] + tempo.extra.margin
        }
        else {
            breaks[length(breaks)] - tempo.extra.margin
        }, by = tick.dist)
        tick.pos <- tick.pos[tick.pos >= min(lim) & tick.pos <= 
            max(lim)]
        tick.values <- tick.pos
    }
    if (any(is.na(tick.pos) | !is.finite(tick.pos), na.rm = TRUE)) {
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, 
            ": NA or Inf GENERATED FOR THE INTER TICK POSITIONS: ", 
            paste(tick.pos, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    if (length(tick.pos) == 0L) {
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count, ") NO INTER TICKS COMPUTED BETWEEN THE LIMITS INDICATED: ", 
            paste(lim, collapse = " "))
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, 
            "\n\n", tempo.warn)))
    }
    output <- list(log = log, coordinates = tick.pos, values = tick.values, 
        warn = warn)
    if (warn.print == TRUE & !is.null(warn)) {
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", 
            warn), call. = FALSE))
    }
    on.exit(expr = options(warning.length = ini.warning.length), 
        add = TRUE)
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
