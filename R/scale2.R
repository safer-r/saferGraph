#' @title scale2
#' @description
#' Attempt to select nice scale numbers when setting n ticks on a lim axis range.
#' @param n Single positive and non null integer value indicating the desired number of main ticks on the axis.
#' @param lim Vector of 2 numbers indicating the limit range of the axis. Order of the 2 values matters (for inverted axis). Can be log transformed values.
#' @param kind Single character string. Either "approx" (approximative), "strict" (strict) or "strict.cl" (strict clean). If "approx", use the scales::trans_breaks() function to provide an easy to read scale of approximately n ticks spanning the range of the lim argument. If "strict", cut the range of the lim argument into n + 1 equidistant part and return the n numbers at each boundary. This often generates numbers uneasy to read. If "strict.cl", provide an easy to read scale of exactly n ticks, but sometimes not completely spanning the range of the lim argument.
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the required packages if not in the default directories. Ignored if NULL.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns A vector of numbers.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' # approximate number of main ticks
#' 
#' ymin = 2 ; 
#' ymax = 3.101 ; 
#' n = 5 ; 
#' scale <- scale2(n = n, lim = c(ymin, ymax), kind = "approx") ; 
#' scale ; 
#' par(yaxt = "n", yaxs = "i", las = 1) ; 
#' plot(ymin:ymax, ymin:ymax, xlim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], ylim = base::range(scale, ymin, ymax)[order(c(ymin, ymax))], xlab = "DEFAULT SCALE", ylab = "NEW SCALE") ;
#' par(yaxt = "s") ; 
#' axis(side = 2, at = scale)
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales trans_breaks
#' @importFrom saferDev arg_check
#' @importFrom saferTool round2
#' @export
scale2 <- function(
        n, 
        lim, 
        kind = "approx", 
        lib.path = NULL,
        safer_check = TRUE
){
    # DEBUGGING
    # n = 9 ; lim = c(2, 3.101) ; kind = "approx" ; lib.path = NULL ; safer_check = TRUE # for function debugging
    # n = 10 ; lim = c(1e-4, 1e6) ; kind = "approx" ; lib.path = NULL ; safer_check = TRUE # for function debugging
    # package name
    package.name <- "saferGraph"
    # end package name
    # function name
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)ini <- match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            external.function.name = function.name,
            external.package.name = package.name
)
    }
    # end critical operator checking
    # check of lib.path
    if( ! base::is.null(lib.path)){
        if( ! base::all(base::typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            base::.libPaths(new = base::sub(x = lib.path, pattern = "/$|\\\\$", replacement = "")) # .libPaths(new = ) add path to default path. BEWARE: .libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib.path <- base::.libPaths()
        }
    }else{
        lib.path <- base::.libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }
    # end check of lib.path
    
    # check of the required function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
        fun = base::c(
            "ggplot2::ggplot_build",
            "ggplot2::ggplot",
            "ggplot2::scale_y_continuous",
            "scales::trans_breaks",
            "saferDev::arg_check",
            "saferTool::round2"
        ),
        lib.path = lib.path,
        external.function.name = function.name,
        external.package.name = package.name
    )
    }
    # end check of the required function from the required packages
    # end package checking
    
    
    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "n", 
        "lim"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "),base::missing("), "))")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if(tempo$problem == FALSE & base::isTRUE(base::all.equal(n, 0))){ # isTRUE(all.equal(n, 0)) equivalent to n == 0 but deals with floats (approx ok)
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nn ARGUMENT MUST BE A NON NULL AND POSITIVE INTEGER")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE) # 
    }
    tempo <- saferDev::arg_check(data = lim, class = "vector", mode = "numeric", length = 2, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if(tempo$problem == FALSE & base::all(base::diff(lim) == 0L, na.rm = TRUE)){ # isTRUE(all.equal(diff(lim), rep(0, length(diff(lim))))) not used because we strictly need zero as a result
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nlim ARGUMENT HAS A NULL RANGE (2 IDENTICAL VALUES)")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }else if(tempo$problem == FALSE & base::any(lim %in% base::c(Inf, -Inf))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nlim ARGUMENT CANNOT CONTAIN -Inf OR Inf VALUES")
        text.check <- base::c(text.check, tempo.cat)
        argum.check <- base::c(argum.check, TRUE)
    }
    tempo <- saferDev::arg_check(data = kind, options = base::c("approx", "strict", "strict.cl"), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(lib.path)){
        tempo <- saferDev::arg_check(data = lib.path, class = "vector", mode = "character", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if(tempo$problem == FALSE){
            if( ! base::all(base::dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nDIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", base::paste(lib.path, collapse = "\n"))
                text.check <- base::c(text.check, tempo.cat)
                argum.check <- base::c(argum.check, TRUE)
            }
        }
    }
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = any)) & base::lapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-base::c(
        "n", 
        "lim", 
        "kind",
        # "lib.path" = NULL, # inactivated because can be null
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    lim.rank <- base::rank(lim) # to deal with inverted axis
    lim <- base::sort(lim)
    if(kind == "approx"){
        output <- ggplot2::ggplot_build(ggplot2::ggplot() + ggplot2::scale_y_continuous(
            breaks = scales::trans_breaks(
                trans = "identity", 
                inv = "identity", 
                n = n
            ), 
            limits = lim
        ))$layout$panel_params[[1]]$y$breaks # pretty() alone is not appropriate: tempo.pret <-  pretty(seq(lim[1] ,lim[2], length.out = n)) ; tempo.pret[tempo.pret > = lim[1] & tempo.pret < = lim[2]]. # in ggplot 3.3.0, tempo.coord$y.major_source replaced by tempo.coord$y$breaks
        if( ! base::is.null(base::attributes(output))){ # layout$panel_params[[1]]$y$breaks can be characters (labels of the axis). In that case, it has attributes that corresponds to positions
            output <- base::unlist(base::attributes(output))
        }
        output <- output[ ! base::is.na(output)]
    }else if(kind == "strict"){
        output <- saferTool::round2(base::seq(lim[1] ,lim[2], length.out = n), 2, safer_check = FALSE)
    }else if(kind == "strict.cl"){
        tempo.range <- base::diff(base::sort(lim))
        tempo.max <- base::max(lim)
        tempo.min <- base::min(lim)
        mid <- tempo.min + (tempo.range/2) # middle of axis
        tempo.inter <- tempo.range / (n + 1) # current interval between two ticks, between 0 and Inf
        if(tempo.inter == 0L){ # isTRUE(all.equal(tempo.inter, rep(0, length(tempo.inter)))) not used because we strictly need zero as a result
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE INTERVAL BETWEEN TWO TICKS OF THE SCALE IS NULL. MODIFY THE lim OR n ARGUMENT")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        log10.abs.lim <- 200
        log10.range <- (-log10.abs.lim):log10.abs.lim
        log10.vec <- 10^log10.range
        round.vec <- base::c(5, 4, 3, 2.5, 2, 1.25, 1)
        dec.table <- base::outer(log10.vec, round.vec) # table containing the scale units (row: power of ten from -201 to +199, column: the 5, 2.5, 2, 1.25, 1 notches
        
        # recover the number of leading zeros in tempo.inter
        ini.scipen <- base::options()$scipen
        base::options(scipen = -1000) # force scientific format
        if(base::any(base::grepl(pattern = "\\+", x = tempo.inter), na.rm = TRUE)){ # tempo.inter > 1
            power10.exp <- base::as.integer(base::substring(text = tempo.inter, first = (base::regexpr(pattern = "\\+", text = tempo.inter) + 1))) # recover the power of 10. Example recover 08 from 1e+08
            mantisse <- base::as.numeric(base::substr(x = tempo.inter, start = 1, stop = (base::regexpr(pattern = "\\+", text = tempo.inter) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
        }else if(base::any(base::grepl(pattern = "\\-", x = tempo.inter), na.rm = TRUE)){ # tempo.inter < 1
            power10.exp <- base::as.integer(base::substring(text = tempo.inter, first = (base::regexpr(pattern = "\\-", text = tempo.inter)))) # recover the power of 10. Example recover 08 from 1e+08
            mantisse <- base::as.numeric(base::substr(x = tempo.inter, start = 1, stop = (base::regexpr(pattern = "\\-", text = tempo.inter) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nCODE INCONSISTENCY 1")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        tempo.scale <- dec.table[log10.range == power10.exp, ]
        # new interval 
        inter.select <- NULL
        for(i1 in 1:base::length(tempo.scale)){
            tempo.first.tick <- base::trunc((tempo.min + tempo.scale[i1]) / tempo.scale[i1]) * (tempo.scale[i1]) # this would be use to have a number not multiple of tempo.scale[i1]: ceiling(tempo.min) + tempo.scale[i1] * 10^power10.exp
            tempo.last.tick <- tempo.first.tick + tempo.scale[i1] * (n - 1)
            if((tempo.first.tick >= tempo.min) & (tempo.last.tick <= tempo.max)){
                inter.select <- tempo.scale[i1]
                break()
            }
        }
        if(base::is.null(inter.select)){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nCODE INCONSISTENCY 2")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        base::options(scipen = ini.scipen) # restore the initial scientific penalty
        # end new interval 
        # centering the new scale 
        tempo.mid <- base::trunc((mid + (-1:1) * inter.select) / inter.select) * inter.select # tempo middle tick closest to the middle axis
        mid.tick <- tempo.mid[base::which.min(base::abs(tempo.mid - mid))]
        if(base::isTRUE(base::all.equal(n, base::rep(1, base::length(n))))){ # isTRUE(all.equal(n, rep(1, length(n)))) is similar to n == 1L but deals with float
            output <- mid.tick
        }else if(base::isTRUE(base::all.equal(n, base::rep(2, base::length(n))))){ # isTRUE(all.equal(n, rep(0, length(n)))) is similar to n == 2L but deals with float
            output <- mid.tick
            tempo.min.dist <- mid.tick - inter.select - tempo.min
            tempo.max.dist <- tempo.max - mid.tick + inter.select
            if(tempo.min.dist <= tempo.max.dist){ # distance between lowest tick and bottom axis <= distance between highest tick and top axis. If yes, extra tick but at the top, otherwise at the bottom
                output <- base::c(mid.tick, mid.tick + inter.select)
            }else{
                output <- base::c(mid.tick - inter.select, mid.tick)
            }
        }else if((n / 2 - base::trunc(n / 2)) > 0.1){ # > 0.1 to avoid floating point. Because result can only be 0 or 0.5. Thus, > 0.1 means odd number
            output <- base::c(mid.tick - (base::trunc(n / 2):1) * inter.select, mid.tick, mid.tick + (1:base::trunc(n / 2)) * inter.select)
        }else if((n / 2 - base::trunc(n / 2)) < 0.1){ # < 0.1 to avoid floating point. Because result can only be 0 or 0.5. Thus, < 0.1 means even number
            tempo.min.dist <- mid.tick - base::trunc(n / 2) * inter.select - tempo.min
            tempo.max.dist <- tempo.max - mid.tick + base::trunc(n / 2) * inter.select
            if(tempo.min.dist <= tempo.max.dist){ # distance between lowest tick and bottom axis <= distance between highest tick and top axis. If yes, extra tick but at the bottom, otherwise at the top
                output <- base::c(mid.tick - ((base::trunc(n / 2) - 1):1) * inter.select, mid.tick, mid.tick + (1:base::trunc(n / 2)) * inter.select)
            }else{
                output <- base::c(mid.tick - (base::trunc(n / 2):1) * inter.select, mid.tick, mid.tick + (1:(base::trunc(n / 2) - 1)) * inter.select)
            }
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nCODE INCONSISTENCY 3")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        # end centering the new scale 
        # last check
        if(base::min(output) < tempo.min){
            output <- base::c(output[-1], base::max(output) + inter.select) # remove the lowest tick and add a tick at the top
        }else if( base::max(output) > tempo.max){
            output <- base::c(base::min(output) - inter.select, output[-base::length(output)])
        }
        if(base::min(output) < tempo.min | base::max(output) > tempo.max){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nCODE INCONSISTENCY 4")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if(base::any(base::is.na(output))){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nCODE INCONSISTENCY 5 (NA GENERATION)")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        # end last check
    }else{
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nCODE INCONSISTENCY 6")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(base::diff(lim.rank) < 0){
        output <- base::rev(output)
    }
    # output
    # warning output
    # end warning output
    base::return(output)
    # end output
    # end main code
}

