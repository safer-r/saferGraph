\name{open}
\alias{open}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
open(pdf = TRUE, pdf.path = "working.dir", pdf.name = "graph", width = 7, height = 7, paper = "special", pdf.overwrite = FALSE, rescale = "fixed", remove.read.only = TRUE, return.output = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pdf}{
%%     ~~Describe \code{pdf} here~~
}
  \item{pdf.path}{
%%     ~~Describe \code{pdf.path} here~~
}
  \item{pdf.name}{
%%     ~~Describe \code{pdf.name} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{paper}{
%%     ~~Describe \code{paper} here~~
}
  \item{pdf.overwrite}{
%%     ~~Describe \code{pdf.overwrite} here~~
}
  \item{rescale}{
%%     ~~Describe \code{rescale} here~~
}
  \item{remove.read.only}{
%%     ~~Describe \code{remove.read.only} here~~
}
  \item{return.output}{
%%     ~~Describe \code{return.output} here~~
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
function (pdf = TRUE, pdf.path = "working.dir", pdf.name = "graph", 
    width = 7, height = 7, paper = "special", pdf.overwrite = FALSE, 
    rescale = "fixed", remove.read.only = TRUE, return.output = FALSE) 
{
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
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
    tempo <- cuteDev::arg_check(data = pdf, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = pdf.path, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = pdf.name, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = width, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = height, class = "vector", 
        mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = paper, options = c("a4", 
        "letter", "legal", "us", "executive", "a4r", "USr", "special", 
        "A4", "LETTER", "LEGAL", "US"), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = pdf.overwrite, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = rescale, options = c("R", 
        "fit", "fixed"), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = remove.read.only, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = return.output, class = "logical", 
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
    tempo.arg <- c("pdf", "pdf.path", "pdf.name", "width", "height", 
        "paper", "pdf.overwrite", "rescale", "remove.read.only", 
        "return.output")
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
    if (pdf.path == "working.dir") {
        pdf.path <- getwd()
    }
    else {
        if (grepl(x = pdf.path, pattern = ".+/$")) {
            pdf.path <- sub(x = pdf.path, pattern = "/$", replacement = "")
        }
        else if (grepl(x = pdf.path, pattern = ".+[\\]$")) {
            pdf.path <- sub(x = pdf.path, pattern = "[\\]$", 
                replacement = "")
        }
        if (dir.exists(pdf.path) == FALSE) {
            tempo.cat <- paste0("ERROR IN ", function.name, "\npdf.path ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY\n", 
                pdf.path)
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (Sys.info()["sysname"] == "Windows") {
        open.fail <- NULL
        grDevices::windows()
        ini.par <- par(no.readonly = remove.read.only)
        invisible(dev.off())
    }
    else if (Sys.info()["sysname"] == "Linux") {
        if (pdf == TRUE) {
            if (exists(".Random.seed", envir = .GlobalEnv)) {
                tempo.random.seed <- .Random.seed
                on.exit(assign(".Random.seed", tempo.random.seed, 
                  env = .GlobalEnv))
            }
            else {
                on.exit(set.seed(NULL))
            }
            set.seed(NULL)
            tempo.code <- sample(x = 1:1e+07, size = 1)
            while (file.exists(paste0(pdf.path, "/recover_ini_par", 
                tempo.code, ".pdf")) == TRUE) {
                tempo.code <- tempo.code + 1
            }
            grDevices::pdf(width = width, height = height, file = paste0(pdf.path, 
                "/recover_ini_par", tempo.code, ".pdf"), paper = paper)
            ini.par <- par(no.readonly = remove.read.only)
            invisible(dev.off())
            file.remove(paste0(pdf.path, "/recover_ini_par", 
                tempo.code, ".pdf"))
        }
        else {
            if (file.exists(paste0(getwd(), "/Rplots.pdf"))) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  "\nTHIS FUNCTION CANNOT BE USED ON LINUX IF A Rplots.pdf FILE ALREADY EXISTS HERE\n", 
                  getwd())
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            else {
                open.fail <- suppressWarnings(try(grDevices::X11(), 
                  silent = TRUE))[]
                if (is.null(open.fail)) {
                  ini.par <- par(no.readonly = remove.read.only)
                  invisible(dev.off())
                }
                else if (file.exists(paste0(getwd(), "/Rplots.pdf"))) {
                  file.remove(paste0(getwd(), "/Rplots.pdf"))
                  tempo.cat <- ("ERROR IN function.name\nTHIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM\nTO OVERCOME THIS, EITHER SET THE X GRAPHIC INTERFACE OF THE SYSTEM OR SET THE pdf ARGUMENT OF THE function.name FUNCTION TO TRUE AND RERUN")
                  stop(paste0("\n\n================\n\n", tempo.cat, 
                    "\n\n================\n\n"), call. = FALSE)
                }
            }
        }
    }
    else {
        open.fail <- NULL
        grDevices::quartz()
        ini.par <- par(no.readonly = remove.read.only)
        invisible(dev.off())
    }
    zone.ini <- matrix(1, ncol = 1)
    if (pdf == TRUE) {
        if (grepl(x = pdf.name, pattern = "\\.pdf$")) {
            pdf.name <- sub(x = pdf.name, pattern = "\\.pdf$", 
                replacement = "")
        }
        pdf.loc <- paste0(pdf.path, "/", pdf.name, ".pdf")
        if (file.exists(pdf.loc) == TRUE & pdf.overwrite == FALSE) {
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", 
                pdf.loc, " FILE ALREADY EXISTS AND CANNOT BE OVERWRITTEN DUE TO pdf.overwrite ARGUMENT SET TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else {
            grDevices::pdf(width = width, height = height, file = pdf.loc, 
                paper = paper)
        }
    }
    else if (pdf == FALSE) {
        pdf.loc <- NULL
        if (Sys.info()["sysname"] == "Windows") {
            grDevices::windows(width = width, height = height, 
                rescale = rescale)
        }
        else if (Sys.info()["sysname"] == "Linux") {
            if (!is.null(open.fail)) {
                tempo.cat <- "ERROR IN function.name\nTHIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM\nTO OVERCOME THIS, EITHER SET THE X GRAPHIC INTERFACE OF THE SYSTEM OR SET THE pdf ARGUMENT OF THE function.name FUNCTION TO TRUE AND RERUN"
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            else {
                grDevices::X11(width = width, height = height)
            }
        }
        else {
            grDevices::quartz(width = width, height = height)
        }
    }
    if (return.output == TRUE) {
        output <- list(pdf.loc = pdf.loc, ini.par = ini.par, 
            zone.ini = zone.ini, dim = dev.size())
        return(output)
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
