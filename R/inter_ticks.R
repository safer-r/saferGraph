#' @title inter_ticks
#' @description
#' Define coordinates and values of secondary ticks.
#' @param lim Vector of 2 numbers indicating the limit range of the axis. Order of the 2 values matters (for inverted axis). If log argument is "log2" or "log10", values in lim must be already log transformed. Thus, negative or zero values are allowed.
#' @param log Single character string. Either "log2" (values in the lim argument are log2 transformed) or "log10" (values in the lim argument are log10 transformed), or "no".
#' @param breaks Mandatory vector of numbers indicating the main ticks values/positions when log argument is "no". Ignored when log argument is "log2" or "log10".
#' @param n Single numeric value indicating the number of secondary ticks between each main tick when log argument is "no". Ignored when log argument is "log2" or "log10".
#' @param warn.print Single logical value. Print potential warning messages at the end of the execution? If FALSE, warning messages are never printed, but can still be recovered in the returned list.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns 
#' A list containing :
#' 
#' - $log: value of the log argument used.
#' 
#' - $coordinates: the coordinates of the secondary ticks on the axis, between the lim values.
#' 
#' - $values: the corresponding values associated to each coordinate (with log scale, 2^$values or 10^$values is equivalent to the labels of the axis).
#' 
#' - $warn: the potential warning messages. Use cat() for proper display. NULL if no warning.
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
#' @importFrom saferDev arg_check
#' @export
inter_ticks <- function(
        lim, 
        log = "log10", 
        breaks = NULL, 
        n = NULL, 
        warn.print = TRUE,
        safer_check = TRUE
){
    # DEBUGGING
    # lim = c(2, 3.101) ; log = "no" ; breaks = NULL ; n = NULL ; warn.print = TRUE ; safer_check = TRUE# for function debugging
    # lim = c(0, 26.5) ; log = "no" ; breaks = c(0, 10, 20) ; n = 3 ; safer_check = TRUE # for function debugging
    # lim = c(10, 0); log = "no"; breaks = c(10, 8, 6, 4, 2, 0); n = 4 ; safer_check = TRUE # for function debugging
    # lim = c(-10, -20); log = "no"; breaks = c(-20, -15, -10); n = 4 ; safer_check = TRUE# for function debugging
    # package name
    package.name <- "saferGraph"
    # end package name
    # function name
    ini <- base::match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # package checking
    # check of lib.path
    # end check of lib.path
    # check of the required function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
        fun = base::c(
            "saferDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    }
    # end check of the required function from the required packages
    # end package checking

    
    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "lim"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "),base::missing("), "))")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = lim, class = "vector", mode = "numeric", length = 2, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = log, options = base::c("no", "log2", "log10"), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(breaks)){
        tempo <- saferDev::arg_check(data = breaks, class = "vector", mode = "numeric", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(n)){
        tempo <- saferDev::arg_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = warn.print, class = "vector", mode = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking

    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) == "list", na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <-base:: paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "lim", 
        "log", 
        # "breaks", # inactivated because can be null
        # "n", # inactivated because can be null
        "warn.print" ,
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    if(base::all(base::diff(lim) == 0L, na.rm = TRUE)){ # isTRUE(all.equal(diff(lim), rep(0, length(diff(lim))))) not used because we strictly need zero as a result
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nlim ARGUMENT HAS A NULL RANGE (2 IDENTICAL VALUES): ", base::paste(lim, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if(base::any(lim %in% base::c(Inf, -Inf))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nlim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(log == "no" & base::is.null(breaks)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nbreaks ARGUMENT CANNOT BE NULL IF log ARGUMENT IS \"no\"")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! base::is.null(breaks)){
        if(base::length(breaks) < 2){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nbreaks ARGUMENT MUST HAVE 2 VALUES AT LEAST (OTHERWISE, INTER TICK POSITIONS CANNOT BE COMPUTED): ", base::paste(breaks, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if( ! base::isTRUE(base::all.equal(base::diff(base::sort(breaks)), base::rep(base::diff(base::sort(breaks))[1], base::length(base::diff(base::sort(breaks))))))){ # isTRUE(all.equal(n, 0)) equivalent to n == 0 but deals with floats (approx ok)
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nbreaks ARGUMENT MUST HAVE EQUIDISTANT VALUES (OTHERWISE, EQUAL NUMBER OF INTER TICK BETWEEN MAIN TICKS CANNOT BE COMPUTED): ", base::paste(breaks, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(n)){
        if(n <= 0){
            tempo.cat <-base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nn ARGUMENT MUST BE A POSITIVE AND NON NULL INTEGER: ", base::paste(n, collapse = " "))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # end other checkings
    # end second round of checking and data preparation
    # main code
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    lim.rank <- base::rank(lim) # to deal with inverse axis
    if(log != "no"){
        ini.scipen <- base::options()$scipen
        base::options(scipen = -1000) # force scientific format
        power10.exp <- base::as.integer(base::substring(text = 10^lim, first = (base::regexpr(pattern = "\\+|\\-", text = 10^lim)))) # recover the power of 10, i.e., integer part of lim. Example recover 08 from 1e+08. Works for log2
        # mantisse <- as.numeric(substr(x = 10^lim, start = 1, stop = (regexpr(pattern = "\\+|\\-", text = 10^lim) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
        base::options(scipen = ini.scipen) # restore the initial scientific penalty
        tick.pos <- base::unique(base::as.vector(base::outer(2:10, base::ifelse(log == "log2", 2, 10)^((power10.exp[1] - base::ifelse(base::diff(lim.rank) > 0, 1, -1)):(power10.exp[2] + base::ifelse(base::diff(lim.rank) > 0, 1, -1)))))) # use log10(2:10) even if log2: it is to get log values between 0 and 1
        tick.pos <- base::sort(tick.pos, decreasing = base::ifelse(base::diff(lim.rank) > 0, FALSE, TRUE))
        if(log == "log2"){
            tick.values <- tick.pos[tick.pos >= base::min(2^lim) & tick.pos <= base::max(2^lim)]
            tick.pos <- base::log2(tick.values)
        }else if(log == "log10"){
            tick.values <- tick.pos[tick.pos >= base::min(10^lim) & tick.pos <= base::max(10^lim)]
            tick.pos <- base::log10(tick.values)
        }
    }else{
        # if(length(breaks) > 1){ # not required because already checked above
        breaks.rank <- base::rank(base::c(breaks[1], breaks[base::length(breaks)]))
        if(base::diff(breaks.rank) != base::diff(lim.rank)){
            breaks <- base::sort(breaks, decreasing = base::ifelse(base::diff(lim.rank) < 0, TRUE, FALSE))
            warn.count <- warn.count + 1
            tempo.warn <- base::paste0("(", warn.count,") VALUES IN breaks ARGUMENT NOT IN THE SAME ORDER AS IN lim ARGUMENT -> VALUES REORDERED AS IN lim: ", base::paste(breaks, collapse = " "))
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
            breaks.rank <- base::rank(base::c(breaks[1], breaks[base::length(breaks)]))
        }
        # }
        main.tick.dist <- base::mean(base::diff(breaks), na.rm = TRUE)
        tick.dist <- main.tick.dist / (n + 1)
        tempo.extra.margin <- base::max(base::abs(base::diff(breaks)), na.rm = TRUE)
        tick.pos <- base::seq(
            if(base::diff(breaks.rank) > 0){breaks[1] - tempo.extra.margin}else{breaks[1] + tempo.extra.margin}, 
            if(base::diff(breaks.rank) > 0){breaks[base::length(breaks)] + tempo.extra.margin}else{breaks[base::length(breaks)] - tempo.extra.margin}, 
            by = tick.dist
        )
        tick.pos <- tick.pos[tick.pos >= base::min(lim) & tick.pos <= base::max(lim)]
        tick.values <- tick.pos
    }
    if(base::any(base::is.na(tick.pos) | ! base::is.finite(tick.pos), na.rm = TRUE)){ 
        tempo.cat <- base::paste0("INTERNAL CODE ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: NA or Inf GENERATED FOR THE INTER TICK POSITIONS: ", base::paste(tick.pos, collapse = " "))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(base::length(tick.pos) == 0L){
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") NO INTER TICKS COMPUTED BETWEEN THE LIMITS INDICATED: ", base::paste(lim, collapse = " "))
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
    }
    # output
    # warning output
    # end warning output
    output <- base::list(log = log, coordinates = tick.pos, values = tick.values, warn = warn)
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE)) # to recover the warning messages, see $warn
    }
    base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    base::return(output)
    # end output
    # end main code
}
