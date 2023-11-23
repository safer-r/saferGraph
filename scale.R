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
#' if kind = "approx":
#' 
#' ggplot2
#' 
#' scales
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#' 
#' fun_round()
#' @examples
#' # approximate number of main ticks
#' 
#' ymin = 2 ; 
#' ymax = 3.101 ; 
#' n = 5 ; 
#' scale <- fun_scale(n = n, lim = c(ymin, ymax), kind = "approx") ; 
#' scale ; 
#' par(yaxt = "n", yaxs = "i", las = 1) ; 
#' plot(ymin:ymax, ymin:ymax, xlim = range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ;
#' par(yaxt = "s") ; 
#' axis(side = 2, at = scale)
#' 
#' 
#' # strict number of main ticks
#' 
#' ymin = 2 ; 
#' ymax = 3.101 ; 
#' n = 5 ; 
#' scale <- fun_scale(n = n, lim = c(ymin, ymax), kind = "strict") ; 
#' scale ; 
#' par(yaxt = "n", yaxs = "i", las = 1) ; 
#' plot(ymin:ymax, ymin:ymax, xlim = range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; 
#' par(yaxt = "s") ; 
#' axis(side = 2, at = scale)
#' 
#' 
#' # strict "clean" number of main ticks
#' 
#' ymin = 2 ; 
#' ymax = 3.101 ; 
#' n = 5 ; 
#' scale <- fun_scale(n = n, lim = c(ymin, ymax), kind = "strict.cl") ; 
#' scale ; 
#' par(yaxt = "n", yaxs = "i", las = 1) ; 
#' plot(ymin:ymax, ymin:ymax, xlim = range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; 
#' par(yaxt = "s") ; 
#' axis(side = 2, at = scale)
#' 
#' 
#' # approximate number of main ticks, scale inversion
#' 
#' ymin = 3.101 ; 
#' ymax = 2 ; 
#' n = 5 ; 
#' scale <- fun_scale(n = n, lim = c(ymin, ymax), kind = "approx") ; 
#' scale ; 
#' par(yaxt = "n", yaxs = "i", las = 1) ; 
#' plot(ymin:ymax, ymin:ymax, xlim = range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ; 
#' par(yaxt = "s") ; 
#' axis(side = 2, at = scale)
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales trans_breaks
#' @export
fun_scale <- function(
        n, 
        lim, 
        kind = "approx", 
        lib.path = NULL
){
    # DEBUGGING
    # n = 9 ; lim = c(2, 3.101) ; kind = "approx" ; lib.path = NULL # for function debugging
    # n = 10 ; lim = c(1e-4, 1e6) ; kind = "approx" ; lib.path = NULL # for function debugging
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # end initial argument checking
    # required function checking
    req.function <- c(
        "fun_check", 
        "fun_round"
    )
    tempo <- NULL
    for(i1 in req.function){
        if(length(find(i1, mode = "function")) == 0L){
            tempo <- c(tempo, i1)
        }
    }
    if( ! is.null(tempo)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nREQUIRED cute FUNCTION", ifelse(length(tempo) > 1, "S ARE", " IS"), " MISSING IN THE R ENVIRONMENT:\n", paste0(tempo, collapse = "()\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end required function checking
    
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    
    # arg with no default values
    mandat.args <- c(
        "n", 
        "lim"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    # argument primary checking
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    if(tempo$problem == FALSE & isTRUE(all.equal(n, 0))){ # isTRUE(all.equal(n, 0)) equivalent to n == 0 but deals with floats (approx ok)
        tempo.cat <- paste0("ERROR IN ", function.name, ": n ARGUMENT MUST BE A NON NULL AND POSITIVE INTEGER")
        text.check <- c(text.check, tempo.cat)
        arg.check <- c(arg.check, TRUE) # 
    }
    tempo <- fun_check(data = lim, class = "vector", mode = "numeric", length = 2, fun.name = function.name) ; eval(ee)
    if(tempo$problem == FALSE & all(diff(lim) == 0L, na.rm = TRUE)){ # isTRUE(all.equal(diff(lim), rep(0, length(diff(lim))))) not used because we strictly need zero as a result
        tempo.cat <- paste0("ERROR IN ", function.name, ": lim ARGUMENT HAS A NULL RANGE (2 IDENTICAL VALUES)")
        text.check <- c(text.check, tempo.cat)
        arg.check <- c(arg.check, TRUE)
    }else if(tempo$problem == FALSE & any(lim %in% c(Inf, -Inf))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": lim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
        text.check <- c(text.check, tempo.cat)
        arg.check <- c(arg.check, TRUE)
    }
    tempo <- fun_check(data = kind, options = c("approx", "strict", "strict.cl"), length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(lib.path)){
        tempo <- fun_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
        if(tempo$problem == FALSE){
            if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
                tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                arg.check <- c(arg.check, TRUE)
            }
        }
    }
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument primary checking with fun_check()
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
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
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # end other checkings
    
    # reserved word checking
    # end reserved word checking
    # end second round of checking and data preparation

    # main code
    lim.rank <- rank(lim) # to deal with inverted axis
    lim <- sort(lim)
    if(kind == "approx"){
        # package checking
        fun_pack(req.package = c("ggplot2"), lib.path = lib.path)
        fun_pack(req.package = c("scales"), lib.path = lib.path)
        # end package checking
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
        output <- fun_round(seq(lim[1] ,lim[2], length.out = n), 2)
    }else if(kind == "strict.cl"){
        tempo.range <- diff(sort(lim))
        tempo.max <- max(lim)
        tempo.min <- min(lim)
        mid <- tempo.min + (tempo.range/2) # middle of axis
        tempo.inter <- tempo.range / (n + 1) # current interval between two ticks, between 0 and Inf
        if(tempo.inter == 0L){ # isTRUE(all.equal(tempo.inter, rep(0, length(tempo.inter)))) not used because we strictly need zero as a result
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE INTERVAL BETWEEN TWO TICKS OF THE SCALE IS NULL. MODIFY THE lim OR n ARGUMENT")
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
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 1")
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
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 2")
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
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 3")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        # end centering the new scale 
        # last check
        if(min(output) < tempo.min){
            output <- c(output[-1], max(output) + inter.select) # remove the lowest tick and add a tick at the top
        }else if( max(output) > tempo.max){
            output <- c(min(output) - inter.select, output[-length(output)])
        }
        if(min(output) < tempo.min | max(output) > tempo.max){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 4")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(any(is.na(output))){
            tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 5 (NA GENERATION)")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        # end last check
    }else{
        tempo.cat <- paste0("ERROR IN ", function.name, ": CODE INCONSISTENCY 6")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(diff(lim.rank) < 0){
        output <- rev(output)
    }
    # output
    return(output)
    # end output
    # end main code
}

