#' @title inter_ticks
#' @description
#' Define coordinates and values of secondary ticks.
#' @param lim Vector of 2 numbers indicating the limit range of the axis. Order of the 2 values matters (for inverted axis). If log argument is "log2" or "log10", values in lim must be already log transformed. Thus, negative or zero values are allowed.
#' @param log Single character string. Either "log2" (values in the lim argument are log2 transformed) or "log10" (values in the lim argument are log10 transformed), or "no".
#' @param breaks Mandatory vector of numbers indicating the main ticks values/positions when log argument is "no". Ignored when log argument is "log2" or "log10".
#' @param n Single numeric value indicating the number of secondary ticks between each main tick when log argument is "no". Ignored when log argument is "log2" or "log10".
#' @param warn.print Single logical value. Print potential warning messages at the end of the execution? If FALSE, warning messages are never printed, but can still be recovered in the returned list.
#' @returns 
#' A list containing :
#' 
#' - $log: value of the log argument used.
#' - $coordinates: the coordinates of the secondary ticks on the axis, between the lim values.
#' - $values: the corresponding values associated to each coordinate (with log scale, 2^$values or 10^$values is equivalent to the labels of the axis).
#' - $warn: the potential warning messages. Use cat() for proper display. NULL if no warning.
#' @details 
#' REQUIRED PACKAGES
#' 
#' cuteDev
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' arg_check()
#' @examples
#' # no log scale
#' 
#' inter_ticks(lim = c(-4,4), log = "no", breaks = c(-2, 0, 2), n = 3)
#' inter_ticks(lim = c(10, 0), log = "no", breaks = c(10, 8, 6, 4, 2, 0), n = 4)
#' 
#' 
#' # log2
#' 
#' inter_ticks(lim = c(-4,4), log = "log2")
#' 
#' 
#' # log10
#' 
#' inter_ticks(lim = c(-2,3), log = "log10")
#' @export
inter_ticks <- function(
        lim, 
        log = "log10", 
        breaks = NULL, 
        n = NULL, 
        warn.print = TRUE
){
    # DEBUGGING
    # lim = c(2, 3.101) ; log = "no" ; breaks = NULL ; n = NULL ; warn.print = TRUE # for function debugging
    # lim = c(0, 26.5) ; log = "no" ; breaks = c(0, 10, 20) ; n = 3 # for function debugging
    # lim = c(10, 0); log = "no"; breaks = c(10, 8, 6, 4, 2, 0); n = 4 # for function debugging
    # lim = c(-10, -20); log = "no"; breaks = c(-20, -15, -10); n = 4 # for function debugging
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # package checking
    # check of lib.path
    # end check of lib.path
    # required function checking
    req.function <- c(
        "arg_check"
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
    
    
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "lim"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check <- c(argum.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- arg_check(data = lim, class = "vector", mode = "numeric", length = 2, fun.name = function.name) ; eval(ee)
    tempo <- arg_check(data = log, options = c("no", "log2", "log10"), length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(breaks)){
        tempo <- arg_check(data = breaks, class = "vector", mode = "numeric", fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(n)){
        tempo <- arg_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
    }
    tempo <- arg_check(data = warn.print, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
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
        "lim", 
        "log", 
        # "breaks", # inactivated because can be null
        # "n", # inactivated because can be null
        "warn.print" 
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    if(all(diff(lim) == 0L, na.rm = TRUE)){ # isTRUE(all.equal(diff(lim), rep(0, length(diff(lim))))) not used because we strictly need zero as a result
        tempo.cat <- paste0("ERROR IN ", function.name, "\nlim ARGUMENT HAS A NULL RANGE (2 IDENTICAL VALUES): ", paste(lim, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if(any(lim %in% c(Inf, -Inf))){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nlim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(log == "no" & is.null(breaks)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nbreaks ARGUMENT CANNOT BE NULL IF log ARGUMENT IS \"no\"")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! is.null(breaks)){
        if(length(breaks) < 2){
            tempo.cat <- paste0("ERROR IN ", function.name, "\nbreaks ARGUMENT MUST HAVE 2 VALUES AT LEAST (OTHERWISE, INTER TICK POSITIONS CANNOT BE COMPUTED): ", paste(breaks, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if( ! isTRUE(all.equal(diff(sort(breaks)), rep(diff(sort(breaks))[1], length(diff(sort(breaks))))))){ # isTRUE(all.equal(n, 0)) equivalent to n == 0 but deals with floats (approx ok)
            tempo.cat <- paste0("ERROR IN ", function.name, "\nbreaks ARGUMENT MUST HAVE EQUIDISTANT VALUES (OTHERWISE, EQUAL NUMBER OF INTER TICK BETWEEN MAIN TICKS CANNOT BE COMPUTED): ", paste(breaks, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if( ! is.null(n)){
        if(n <= 0){
            tempo.cat <- paste0("ERROR IN ", function.name, "\nn ARGUMENT MUST BE A POSITIVE AND NON NULL INTEGER: ", paste(n, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # end other checkings
    
    # reserved word checking to avoid bugs
    # end reserved word checking to avoid bugs
    # end second round of checking and data preparation
    # main code
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    lim.rank <- rank(lim) # to deal with inverse axis
    if(log != "no"){
        ini.scipen <- options()$scipen
        options(scipen = -1000) # force scientific format
        power10.exp <- as.integer(substring(text = 10^lim, first = (regexpr(pattern = "\\+|\\-", text = 10^lim)))) # recover the power of 10, i.e., integer part of lim. Example recover 08 from 1e+08. Works for log2
        # mantisse <- as.numeric(substr(x = 10^lim, start = 1, stop = (regexpr(pattern = "\\+|\\-", text = 10^lim) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
        options(scipen = ini.scipen) # restore the initial scientific penalty
        tick.pos <- unique(as.vector(outer(2:10, ifelse(log == "log2", 2, 10)^((power10.exp[1] - ifelse(diff(lim.rank) > 0, 1, -1)):(power10.exp[2] + ifelse(diff(lim.rank) > 0, 1, -1)))))) # use log10(2:10) even if log2: it is to get log values between 0 and 1
        tick.pos <- sort(tick.pos, decreasing = ifelse(diff(lim.rank) > 0, FALSE, TRUE))
        if(log == "log2"){
            tick.values <- tick.pos[tick.pos >= min(2^lim) & tick.pos <= max(2^lim)]
            tick.pos <- log2(tick.values)
        }else if(log == "log10"){
            tick.values <- tick.pos[tick.pos >= min(10^lim) & tick.pos <= max(10^lim)]
            tick.pos <- log10(tick.values)
        }
    }else{
        # if(length(breaks) > 1){ # not required because already checked above
        breaks.rank <- rank(c(breaks[1], breaks[length(breaks)]))
        if(diff(breaks.rank) != diff(lim.rank)){
            breaks <- sort(breaks, decreasing = ifelse(diff(lim.rank) < 0, TRUE, FALSE))
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count,") VALUES IN breaks ARGUMENT NOT IN THE SAME ORDER AS IN lim ARGUMENT -> VALUES REORDERED AS IN lim: ", paste(breaks, collapse = " "))
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
            breaks.rank <- rank(c(breaks[1], breaks[length(breaks)]))
        }
        # }
        main.tick.dist <- mean(diff(breaks), na.rm = TRUE)
        tick.dist <- main.tick.dist / (n + 1)
        tempo.extra.margin <- max(abs(diff(breaks)), na.rm = TRUE)
        tick.pos <- seq(
            if(diff(breaks.rank) > 0){breaks[1] - tempo.extra.margin}else{breaks[1] + tempo.extra.margin}, 
            if(diff(breaks.rank) > 0){breaks[length(breaks)] + tempo.extra.margin}else{breaks[length(breaks)] - tempo.extra.margin}, 
            by = tick.dist
        )
        tick.pos <- tick.pos[tick.pos >= min(lim) & tick.pos <= max(lim)]
        tick.values <- tick.pos
    }
    if(any(is.na(tick.pos) | ! is.finite(tick.pos), na.rm = TRUE)){ 
        tempo.cat <- paste0("INTERNAL CODE ERROR IN ", function.name, ": NA or Inf GENERATED FOR THE INTER TICK POSITIONS: ", paste(tick.pos, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(length(tick.pos) == 0L){
        warn.count <- warn.count + 1
        tempo.warn <- paste0("(", warn.count,") NO INTER TICKS COMPUTED BETWEEN THE LIMITS INDICATED: ", paste(lim, collapse = " "))
        warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
    }
    # output
    # warning output
    # end warning output
    output <- list(log = log, coordinates = tick.pos, values = tick.values, warn = warn)
    if(warn.print == TRUE & ! is.null(warn)){
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE)) # to recover the warning messages, see $warn
    }
    on.exit(exp = options(warning.length = ini.warning.length), add = TRUE)
    return(output)
    # end output
    # end main code
}
