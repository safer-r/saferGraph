\name{scale}
\alias{scale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scale(n, lim, kind = "approx", lib.path = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{lim}{
%%     ~~Describe \code{lim} here~~
}
  \item{kind}{
%%     ~~Describe \code{kind} here~~
}
  \item{lib.path}{
%%     ~~Describe \code{lib.path} here~~
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
function (n, lim, kind = "approx", lib.path = NULL) 
{
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    if (function.name[1] == "::()") {
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    if (!is.null(lib.path)) {
        if (!all(typeof(lib.path) == "character")) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", 
                paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else if (!all(dir.exists(lib.path), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", 
                paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else {
            .libPaths(new = sub(x = lib.path, pattern = "/$|\\\\$", 
                replacement = ""))
            lib.path <- .libPaths()
        }
    }
    else {
        lib.path <- .libPaths()
    }
    .pack_and_function_check(fun = c("ggplot2::ggplot_build", 
        "ggplot2::ggplot", "ggplot2::scale_y_continuous", "scales::trans_breaks", 
        "cuteDev::arg_check", "cuteTool::round"), lib.path = lib.path, 
        external.function.name = function.name)
    mandat.args <- c("n", "lim")
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
    tempo <- cuteDev::arg_check(data = n, class = "vector", typeof = "integer", 
        length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, 
        fun.name = function.name)
    eval(ee)
    if (tempo$problem == FALSE & isTRUE(all.equal(n, 0))) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": n ARGUMENT MUST BE A NON NULL AND POSITIVE INTEGER")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    tempo <- cuteDev::arg_check(data = lim, class = "vector", 
        mode = "numeric", length = 2, fun.name = function.name)
    eval(ee)
    if (tempo$problem == FALSE & all(diff(lim) == 0L, na.rm = TRUE)) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": lim ARGUMENT HAS A NULL RANGE (2 IDENTICAL VALUES)")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    else if (tempo$problem == FALSE & any(lim \%in\% c(Inf, -Inf))) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": lim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    tempo <- cuteDev::arg_check(data = kind, options = c("approx", 
        "strict", "strict.cl"), length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(lib.path)) {
        tempo <- cuteDev::arg_check(data = lib.path, class = "vector", 
            mode = "character", fun.name = function.name)
        eval(ee)
        if (tempo$problem == FALSE) {
            if (!all(dir.exists(lib.path), na.rm = TRUE)) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", 
                  paste(lib.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
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
    tempo.arg <- c("n", "lim", "kind")
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
    lim.rank <- rank(lim)
    lim <- sort(lim)
    if (kind == "approx") {
        output <- ggplot2::ggplot_build(ggplot2::ggplot() + ggplot2::scale_y_continuous(breaks = scales::trans_breaks(trans = "identity", 
            inv = "identity", n = n), limits = lim))$layout$panel_params[[1]]$y$breaks
        if (!is.null(attributes(output))) {
            output <- unlist(attributes(output))
        }
        output <- output[!is.na(output)]
    }
    else if (kind == "strict") {
        output <- cuteTool::round(seq(lim[1], lim[2], length.out = n), 
            2)
    }
    else if (kind == "strict.cl") {
        tempo.range <- diff(sort(lim))
        tempo.max <- max(lim)
        tempo.min <- min(lim)
        mid <- tempo.min + (tempo.range/2)
        tempo.inter <- tempo.range/(n + 1)
        if (tempo.inter == 0L) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE INTERVAL BETWEEN TWO TICKS OF THE SCALE IS NULL. MODIFY THE lim OR n ARGUMENT")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        log10.abs.lim <- 200
        log10.range <- (-log10.abs.lim):log10.abs.lim
        log10.vec <- 10^log10.range
        round.vec <- c(5, 4, 3, 2.5, 2, 1.25, 1)
        dec.table <- outer(log10.vec, round.vec)
        ini.scipen <- options()$scipen
        options(scipen = -1000)
        if (any(grepl(pattern = "\\+", x = tempo.inter), na.rm = TRUE)) {
            power10.exp <- as.integer(substring(text = tempo.inter, 
                first = (regexpr(pattern = "\\+", text = tempo.inter) + 
                  1)))
            mantisse <- as.numeric(substr(x = tempo.inter, start = 1, 
                stop = (regexpr(pattern = "\\+", text = tempo.inter) - 
                  2)))
        }
        else if (any(grepl(pattern = "\\-", x = tempo.inter), 
            na.rm = TRUE)) {
            power10.exp <- as.integer(substring(text = tempo.inter, 
                first = (regexpr(pattern = "\\-", text = tempo.inter))))
            mantisse <- as.numeric(substr(x = tempo.inter, start = 1, 
                stop = (regexpr(pattern = "\\-", text = tempo.inter) - 
                  2)))
        }
        else {
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 1")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        tempo.scale <- dec.table[log10.range == power10.exp, 
            ]
        inter.select <- NULL
        for (i1 in 1:length(tempo.scale)) {
            tempo.first.tick <- trunc((tempo.min + tempo.scale[i1])/tempo.scale[i1]) * 
                (tempo.scale[i1])
            tempo.last.tick <- tempo.first.tick + tempo.scale[i1] * 
                (n - 1)
            if ((tempo.first.tick >= tempo.min) & (tempo.last.tick <= 
                tempo.max)) {
                inter.select <- tempo.scale[i1]
                (break)()
            }
        }
        if (is.null(inter.select)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 2")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        options(scipen = ini.scipen)
        tempo.mid <- trunc((mid + (-1:1) * inter.select)/inter.select) * 
            inter.select
        mid.tick <- tempo.mid[which.min(abs(tempo.mid - mid))]
        if (isTRUE(all.equal(n, rep(1, length(n))))) {
            output <- mid.tick
        }
        else if (isTRUE(all.equal(n, rep(2, length(n))))) {
            output <- mid.tick
            tempo.min.dist <- mid.tick - inter.select - tempo.min
            tempo.max.dist <- tempo.max - mid.tick + inter.select
            if (tempo.min.dist <= tempo.max.dist) {
                output <- c(mid.tick, mid.tick + inter.select)
            }
            else {
                output <- c(mid.tick - inter.select, mid.tick)
            }
        }
        else if ((n/2 - trunc(n/2)) > 0.1) {
            output <- c(mid.tick - (trunc(n/2):1) * inter.select, 
                mid.tick, mid.tick + (1:trunc(n/2)) * inter.select)
        }
        else if ((n/2 - trunc(n/2)) < 0.1) {
            tempo.min.dist <- mid.tick - trunc(n/2) * inter.select - 
                tempo.min
            tempo.max.dist <- tempo.max - mid.tick + trunc(n/2) * 
                inter.select
            if (tempo.min.dist <= tempo.max.dist) {
                output <- c(mid.tick - ((trunc(n/2) - 1):1) * 
                  inter.select, mid.tick, mid.tick + (1:trunc(n/2)) * 
                  inter.select)
            }
            else {
                output <- c(mid.tick - (trunc(n/2):1) * inter.select, 
                  mid.tick, mid.tick + (1:(trunc(n/2) - 1)) * 
                    inter.select)
            }
        }
        else {
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 3")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (min(output) < tempo.min) {
            output <- c(output[-1], max(output) + inter.select)
        }
        else if (max(output) > tempo.max) {
            output <- c(min(output) - inter.select, output[-length(output)])
        }
        if (min(output) < tempo.min | max(output) > tempo.max) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 4")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (any(is.na(output))) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 5 (NA GENERATION)")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    else {
        tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 6")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (diff(lim.rank) < 0) {
        output <- rev(output)
    }
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
