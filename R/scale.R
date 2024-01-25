#' @title scale
#' @description
#' Attempt to select nice scale numbers when setting n ticks on a lim axis range.
#' @param n Single positive and non null integer value indicating the desired number of main ticks on the axis.
#' @param lim Vector of 2 numbers indicating the limit range of the axis. Order of the 2 values matters (for inverted axis). Can be log transformed values.
#' @param kind Single character string. Either "approx" (approximative), "strict" (strict) or "strict.cl" (strict clean). If "approx", use the scales::trans_breaks() function to provide an easy to read scale of approximately n ticks spanning the range of the lim argument. If "strict", cut the range of the lim argument into n + 1 equidistant part and return the n numbers at each boundary. This often generates numbers uneasy to read. If "strict.cl", provide an easy to read scale of exactly n ticks, but sometimes not completely spanning the range of the lim argument.
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @returns A vector of numbers.
#' @details 
#' REQUIRED PACKAGES
#' 
#' cuteDev
#' 
#' cuteTool
#' 
#' if kind = "approx":
#' 
#' ggplot2
#' 
#' scales
#' 
#' 
#' REQUIRED FUNCTIONS FROM THE cute PACKAGE
#' 
#' arg_check()
#' 
#' round()
#' @examples
#' # approximate number of main ticks
#' 
#' ymin = 2 ; 
#' ymax = 3.101 ; 
#' n = 5 ; 
#' scale <- scale(n = n, lim = c(ymin, ymax), kind = "approx") ; 
#' scale ; 
#' par(yaxt = "n", yaxs = "i", las = 1) ; 
#' plot(ymin:ymax, ymin:ymax, xlim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ;
#' par(yaxt = "s") ; 
#' axis(side = 2, at = scale)
#' 
#' 
#' # strict number of main ticks
#' 
#' ymin = 2 ; 
#' ymax = 3.101 ; 
#' n = 5 ; 
#' scale <- scale(n = n, lim = c(ymin, ymax), kind = "strict") ; 
#' scale ; 
#' par(yaxt = "n", yaxs = "i", las = 1) ; 
#' plot(ymin:ymax, ymin:ymax, xlim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; 
#' par(yaxt = "s") ; 
#' axis(side = 2, at = scale)
#' 
#' 
#' # strict "clean" number of main ticks
#' 
#' ymin = 2 ; 
#' ymax = 3.101 ; 
#' n = 5 ; 
#' scale <- scale(n = n, lim = c(ymin, ymax), kind = "strict.cl") ; 
#' scale ; 
#' par(yaxt = "n", yaxs = "i", las = 1) ; 
#' plot(ymin:ymax, ymin:ymax, xlim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; 
#' par(yaxt = "s") ; 
#' axis(side = 2, at = scale)
#' 
#' 
#' # approximate number of main ticks, scale inversion
#' 
#' ymin = 3.101 ; 
#' ymax = 2 ; 
#' n = 5 ; 
#' scale <- scale(n = n, lim = c(ymin, ymax), kind = "approx") ; 
#' scale ; 
#' par(yaxt = "n", yaxs = "i", las = 1) ; 
#' plot(ymin:ymax, ymin:ymax, xlim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; 
#' par(yaxt = "s") ; 
#' axis(side = 2, at = scale)
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales trans_breaks
#' @importFrom cuteDev arg_check
#' @importFrom cuteTool round2
#' @export
scale <- function(
        n, 
        lim, 
        kind = "approx", 
        lib.path = NULL
){
    # DEBUGGING
    # n = 9 ; lim = c(2, 3.101) ; kind = "approx" ; lib.path = NULL # for function debugging
    # n = 10 ; lim = c(1e-4, 1e6) ; kind = "approx" ; lib.path = NULL # for function debugging
    # package name
    package.name <- "cuteGraph"
    # end package name
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)ini <- match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # check of lib.path
    if( ! is.null(lib.path)){
        if( ! all(typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            .libPaths(new = sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- .libPaths()
        }
    }else{
        lib.path <- .libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }
    # end check of lib.path
    
    # check of the required function from the required packages
    .pack_and_function_check(
        fun = c(
            "ggplot2::ggplot_build",
            "ggplot2::ggplot",
            "ggplot2::scale_y_continuous",
            "scales::trans_breaks",
            "cuteDev::arg_check",
            "cuteTool::round2"
        ),
        lib.path = lib.path,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking
    
    
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "n", 
        "lim"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check = c(argum.check, tempo$problem) , text.check = c(text.check, tempo$text) , checked.arg.names = c(checked.arg.names, tempo$object.name))
    tempo <- cuteDev::arg_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    if(tempo$problem == FALSE & isTRUE(all.equal(n, 0))){ # isTRUE(all.equal(n, 0)) equivalent to n == 0 but deals with floats (approx ok)
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: n ARGUMENT MUST BE A NON NULL AND POSITIVE INTEGER")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE) # 
    }
    tempo <- cuteDev::arg_check(data = lim, class = "vector", mode = "numeric", length = 2, fun.name = function.name) ; eval(ee)
    if(tempo$problem == FALSE & all(diff(lim) == 0L, na.rm = TRUE)){ # isTRUE(all.equal(diff(lim), rep(0, length(diff(lim))))) not used because we strictly need zero as a result
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: lim ARGUMENT HAS A NULL RANGE (2 IDENTICAL VALUES)")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }else if(tempo$problem == FALSE & any(lim %in% c(Inf, -Inf))){
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: lim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    tempo <- cuteDev::arg_check(data = kind, options = c("approx", "strict", "strict.cl"), length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(lib.path)){
        tempo <- cuteDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
                tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                argum.check <- c(argum.check, TRUE)
            }
        }
    }
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using cuteDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-c(
        "n", 
        "lim", 
        "kind"
        # "lib.path" = NULL # inactivated because can be null
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # end other checkings
    
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # end second round of checking and data preparation
    
    # main code
    lim.rank <- rank(lim) # to deal with inverted axis
    lim <- sort(lim)
    if(kind == "approx"){
        output <- ggplot2::ggplot_build(ggplot2::ggplot() + ggplot2::scale_y_continuous(
            breaks = scales::trans_breaks(
                trans = "identity", 
                inv = "identity", 
                n = n
            ), 
            limits = lim
        ))$layout$panel_params[[1]]$y$breaks # pretty() alone is not appropriate: tempo.pret <-  pretty(seq(lim[1] ,lim[2], length.out = n)) ; tempo.pret[tempo.pret > = lim[1] & tempo.pret < = lim[2]]. # in ggplot 3.3.0, tempo.coord$y.major_source replaced by tempo.coord$y$breaks
        if( ! is.null(attributes(output))){ # layout$panel_params[[1]]$y$breaks can be characters (labels of the axis). In that case, it has attributes that corresponds to positions
            output <- unlist(attributes(output))
        }
        output <- output[ ! is.na(output)]
    }else if(kind == "strict"){
        output <- cuteTool::round2(seq(lim[1] ,lim[2], length.out = n), 2)
    }else if(kind == "strict.cl"){
        tempo.range <- diff(sort(lim))
        tempo.max <- base::max(lim)
        tempo.min <- base::min(lim)
        mid <- tempo.min + (tempo.range/2) # middle of axis
        tempo.inter <- tempo.range / (n + 1) # current interval between two ticks, between 0 and Inf
        if(tempo.inter == 0L){ # isTRUE(all.equal(tempo.inter, rep(0, length(tempo.inter)))) not used because we strictly need zero as a result
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE INTERVAL BETWEEN TWO TICKS OF THE SCALE IS NULL. MODIFY THE lim OR n ARGUMENT")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        log10.abs.lim <- 200
        log10.range <- (-log10.abs.lim):log10.abs.lim
        log10.vec <- 10^log10.range
        round.vec <- c(5, 4, 3, 2.5, 2, 1.25, 1)
        dec.table <- outer(log10.vec, round.vec) # table containing the scale units (row: power of ten from -201 to +199, column: the 5, 2.5, 2, 1.25, 1 notches
        
        # recover the number of leading zeros in tempo.inter
        ini.scipen <- options()$scipen
        options(scipen = -1000) # force scientific format
        if(any(grepl(pattern = "\\+", x = tempo.inter), na.rm = TRUE)){ # tempo.inter > 1
            power10.exp <- as.integer(substring(text = tempo.inter, first = (regexpr(pattern = "\\+", text = tempo.inter) + 1))) # recover the power of 10. Example recover 08 from 1e+08
            mantisse <- as.numeric(substr(x = tempo.inter, start = 1, stop = (regexpr(pattern = "\\+", text = tempo.inter) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
        }else if(any(grepl(pattern = "\\-", x = tempo.inter), na.rm = TRUE)){ # tempo.inter < 1
            power10.exp <- as.integer(substring(text = tempo.inter, first = (regexpr(pattern = "\\-", text = tempo.inter)))) # recover the power of 10. Example recover 08 from 1e+08
            mantisse <- as.numeric(substr(x = tempo.inter, start = 1, stop = (regexpr(pattern = "\\-", text = tempo.inter) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
        }else{
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 1")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        tempo.scale <- dec.table[log10.range == power10.exp, ]
        # new interval 
        inter.select <- NULL
        for(i1 in 1:length(tempo.scale)){
            tempo.first.tick <- trunc((tempo.min + tempo.scale[i1]) / tempo.scale[i1]) * (tempo.scale[i1]) # this would be use to have a number not multiple of tempo.scale[i1]: ceiling(tempo.min) + tempo.scale[i1] * 10^power10.exp
            tempo.last.tick <- tempo.first.tick + tempo.scale[i1] * (n - 1)
            if((tempo.first.tick >= tempo.min) & (tempo.last.tick <= tempo.max)){
                inter.select <- tempo.scale[i1]
                break()
            }
        }
        if(is.null(inter.select)){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 2")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        options(scipen = ini.scipen) # restore the initial scientific penalty
        # end new interval 
        # centering the new scale 
        tempo.mid <- trunc((mid + (-1:1) * inter.select) / inter.select) * inter.select # tempo middle tick closest to the middle axis
        mid.tick <- tempo.mid[which.min(abs(tempo.mid - mid))]
        if(isTRUE(all.equal(n, rep(1, length(n))))){ # isTRUE(all.equal(n, rep(1, length(n)))) is similar to n == 1L but deals with float
            output <- mid.tick
        }else if(isTRUE(all.equal(n, rep(2, length(n))))){ # isTRUE(all.equal(n, rep(0, length(n)))) is similar to n == 2L but deals with float
            output <- mid.tick
            tempo.min.dist <- mid.tick - inter.select - tempo.min
            tempo.max.dist <- tempo.max - mid.tick + inter.select
            if(tempo.min.dist <= tempo.max.dist){ # distance between lowest tick and bottom axis <= distance between highest tick and top axis. If yes, extra tick but at the top, otherwise at the bottom
                output <- c(mid.tick, mid.tick + inter.select)
            }else{
                output <- c(mid.tick - inter.select, mid.tick)
            }
        }else if((n / 2 - trunc(n / 2)) > 0.1){ # > 0.1 to avoid floating point. Because result can only be 0 or 0.5. Thus, > 0.1 means odd number
            output <- c(mid.tick - (trunc(n / 2):1) * inter.select, mid.tick, mid.tick + (1:trunc(n / 2)) * inter.select)
        }else if((n / 2 - trunc(n / 2)) < 0.1){ # < 0.1 to avoid floating point. Because result can only be 0 or 0.5. Thus, < 0.1 means even number
            tempo.min.dist <- mid.tick - trunc(n / 2) * inter.select - tempo.min
            tempo.max.dist <- tempo.max - mid.tick + trunc(n / 2) * inter.select
            if(tempo.min.dist <= tempo.max.dist){ # distance between lowest tick and bottom axis <= distance between highest tick and top axis. If yes, extra tick but at the bottom, otherwise at the top
                output <- c(mid.tick - ((trunc(n / 2) - 1):1) * inter.select, mid.tick, mid.tick + (1:trunc(n / 2)) * inter.select)
            }else{
                output <- c(mid.tick - (trunc(n / 2):1) * inter.select, mid.tick, mid.tick + (1:(trunc(n / 2) - 1)) * inter.select)
            }
        }else{
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 3")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        # end centering the new scale 
        # last check
        if(base::min(output) < tempo.min){
            output <- c(output[-1], base::max(output) + inter.select) # remove the lowest tick and add a tick at the top
        }else if( base::max(output) > tempo.max){
            output <- c(base::min(output) - inter.select, output[-length(output)])
        }
        if(base::min(output) < tempo.min | base::max(output) > tempo.max){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 4")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(is.na(output))){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 5 (NA GENERATION)")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        # end last check
    }else{
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: CODE INCONSISTENCY 6")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(diff(lim.rank) < 0){
        output <- rev(output)
    }
    # output
    # warning output
    # end warning output
    return(output)
    # end output
    # end main code
}

