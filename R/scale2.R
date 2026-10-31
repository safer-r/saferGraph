#' @title scale2
#' @description
#' Attempt to select nice scale numbers when setting n ticks on a lim axis range.
#' @param n Single positive and non null integer value indicating the desired number of main ticks on the axis.
#' @param lim Vector of 2 numbers indicating the limit range of the axis. Order of the 2 values matters (for inverted axis). Can be log transformed values.
#' @param kind Single character string. Either "approx" (approximative), "strict" (strict) or "strict.cl" (strict clean). If "approx", use the scales::trans_breaks() function to provide an easy to read scale of approximately n ticks spanning the range of the lim argument. If "strict", cut the range of the lim argument into n + 1 equidistant part and return the n numbers at each boundary. This often generates numbers uneasy to read. If "strict.cl", provide an easy to read scale of exactly n ticks, but sometimes not completely spanning the range of the lim argument.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) required functions and related packages effectively present in local R libraries and 2) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}. Of note, in \code{arg_check()}, \code{error_text} is also used at the end of the string returned when no problem is detected.
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
#' scale <- scale2(n = n, lim = c(ymin, ymax), kind = "strict") ; 
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
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # n = 9 ; lim = c(2, 3.101) ; kind = "approx" ; lib_path = NULL ; safer_check = TRUE # for function debugging
    # n = 10 ; lim = c(1e-4, 1e6) ; kind = "approx" ; lib_path = NULL ; safer_check = TRUE # for function debugging

    #### package name
    package_name <- "saferGraph" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of values if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 0)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name,
        collapse = NULL, 
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    intern_error_text_start <- base::paste0(
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present in your main code 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"), 
            " MANDATORY IN SAFER-R FUNCTIONS:\n", 
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

    ######## arg with no default values
    # optional section: remove the code if none of your arguments has no default value
    no_def_args <- base::c(
        "n", 
        "lim"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(no_def_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(no_def_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "n", 
        "lim", 
        "kind",
        "safer_check" 
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "n", 
        "lim", 
        "kind",
        "safer_check", 
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval, 
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # Mandataory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval, 
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ), 
                FUN = function(x){
                    base::all(x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        if(base::all(base::mode(x = safer_check) == "function", na.rm = TRUE)){
            safer_check <- base::deparse1(expr = safer_check, collapse = "", width.cutoff = 500L)
        }
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n", 
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    # if(safer_check == TRUE){ # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
            if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
            }
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
            tempo_log <- ! base::dir.exists(paths = lib_path)
            tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
            tempo_cat_b[tempo_cat_b == ""] <- "\"\""
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE DIRECTORY PATH",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""), 
                " INDICATED IN THE lib_path ARGUMENT DO", 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"), 
                " NOT EXIST:\n", 
                base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
        }
    }else{
        lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    # } # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = -1, envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "ggplot2::aes",
                "ggplot2::ggplot_build",
                "ggplot2::ggplot",
                "ggplot2::scale_y_continuous",
                "ggplot2::waiver",
                "scales::censor",
                "scales::trans_breaks",
                "saferDev::arg_check",
                "saferTool::round2", 
                # internal functions required in this code
                "saferDev:::.base_op_check"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = -1, envir = )
    # add here in the internal functions that are used in your main code (copy-paste the line above and replace .base_op_check by the name of the internal function
    # not mandatory if your function is not designed for submission to the CRAN

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object_name))
    # copy - paste ththe line below as much, one for each of the arguments of your own function
    tempo <- saferDev::arg_check(data = n, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) 
    tempo <- saferDev::arg_check(data = lim, class = NULL, typeof = NULL, mode = "numeric", length = 2, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) 
    tempo <- saferDev::arg_check(data = kind, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("approx", "strict", "strict.cl"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # example of setting. Adapt to your own function or remove
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    # optional section: remove the code if no graphics used in your functions
    # check the number of graphic devices on exit
    dev_list <- grDevices::dev.list() 
    # This check is here in case the developer has not correctly fill tempo_arg
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    base::on.exit(
        expr = if(base::length(x = dev_list) != base::length(x = grDevices::dev.list())){
            tempo_cat <- base::paste0(
                "INTERNAL ERROR IN THE BACKBONE PART OF ", 
                intern_error_text_start, 
                "SOME GRAPHIC DEVICES WERE OPENED BY ", 
                function_name, 
                " BUT NOT CLOSED BEFORE END OF EXECUTION.\n\nIF IT IS EXPECTED, JUST REMOVE THE CODE DISPLAYING THIS MESSAGE INSIDE ", 
                function_name, 
                ".\n\nOTHERWISE, THE PROBLEM COMES FROM OPENED GRAPHIC DEVICES BEFORE RUNNING ", 
                function_name, 
                " (n = ", 
                base::length(x = dev_list), 
                ") AND AFTER (n = ", 
                base::length(x = grDevices::dev.list()), 
                ").", 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)

        }, 
        add = TRUE, 
        after = TRUE
    )
    # nocov end
    # end check the number of graphic devices on exit
    # restore the graphic parameters on exit
    if(base::length(x = grDevices::dev.list()) > 0){
        par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = "warning") # to recover the present graphical parameters
        base::on.exit(expr = base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = "warning"), add = TRUE, after = TRUE)
    }
    # end restore the graphic parameters on exit
    ######## end graphic device checking

    ######## other checkings
    if(base::isTRUE(x = base::all.equal(target = n, current = 0))){ # isTRUE(all.equal(n, 0)) equivalent to n == 0 but deals with floats (approx ok)
        tempo_cat <- base::paste0(
            error_text_start, 
            "n ARGUMENT MUST BE A NON NULL AND POSITIVE INTEGER:\n",
            n, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::all(base::diff(x = lim) == 0L, na.rm = TRUE)){ # isTRUE(all.equal(diff(lim), rep(0, length(diff(lim))))) not used because we strictly need zero as a result
        tempo_cat <- base::paste0(
            error_text_start, 
            "lim ARGUMENT HAS A NULL RANGE (2 IDENTICAL VALUES):\n",
            base::paste0(lim, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    lim_rank <- base::rank(x = lim, na.last = TRUE, ties.method = base::c("average", "first", "last", "random", "max", "min")) # to deal with inverted axis
    lim <- base::sort(x = lim, decreasing = FALSE)
    if(kind == "approx"){
        output <- ggplot2::ggplot_build(plot = ggplot2::ggplot(data = NULL, mapping = ggplot2::aes(x = , y = ), environment = base::environment(fun = NULL)) + ggplot2::scale_y_continuous(
            breaks = scales::trans_breaks(
                trans = "identity", 
                inv = "identity", 
                n = n
            ), 
            limits = lim, name = ggplot2::waiver(), minor_breaks = ggplot2::waiver(), n.breaks = NULL, labels = ggplot2::waiver(), expand = ggplot2::waiver(), oob = scales::censor, na.value = NA_real_, transform = "identity", trans = base::quote(expr = ), guide = ggplot2::waiver(), position = "left", sec.axis = ggplot2::waiver() # trans = base::quote(expr = ): value returned by lifecycle::deprecated(), the default value of the deprecated trans argument (written without the lifecycle package, not imported by saferGraph)
        ))$layout$panel_params[[1]]$y$breaks # pretty() alone is not appropriate: tempo.pret <-  pretty(seq(lim[1] ,lim[2], length.out = n)) ; tempo.pret[tempo.pret > = lim[1] & tempo.pret < = lim[2]]. # in ggplot 3.3.0, tempo.coord$y.major_source replaced by tempo.coord$y$breaks
        if( ! base::is.null(x = base::attributes(x = output))){ # layout$panel_params[[1]]$y$breaks can be characters (labels of the axis). In that case, it has attributes that corresponds to positions # nocov start (unreachable: output from scales::trans_breaks() with trans = "identity" is always a plain numeric vector without attributes, as verified on R 4.6.0)
            output <- base::unlist(x = base::attributes(x = output), recursive = TRUE, use.names = TRUE)
        } # nocov end
        output <- output[ ! base::is.na(x = output)]
    }else if(kind == "strict"){
        output <- saferTool::round2(data = base::seq(lim[1] ,lim[2], length.out = n), dec_nb = 2, after_lead_zero = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")
    }else if(kind == "strict.cl"){
        tempo_range <- base::diff(x = base::sort(x = lim, decreasing = FALSE))
        tempo_max <- base::max(lim, na.rm = FALSE)
        tempo_min <- base::min(lim, na.rm = FALSE)
        mid <- tempo_min + (tempo_range/2) # middle of axis
        tempo_inter <- tempo_range / (n + 1) # current interval between two ticks, between 0 and Inf
        if(tempo_inter == 0L){ # isTRUE(all.equal(tempo_inter, rep(0, length(tempo_inter)))) not used because we strictly need zero as a result
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE INTERVAL BETWEEN TWO TICKS OF THE SCALE IS NULL. MODIFY THE lim OR n ARGUMENT", 
            collapse = NULL, 
            recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        log10_abs_lim <- 200
        log10_range <- (-log10_abs_lim):log10_abs_lim
        log10_vec <- 10^log10_range
        round_vec <- base::c(5, 4, 3, 2.5, 2, 1.25, 1)
        dec_table <- base::outer(X = log10_vec, Y = round_vec, FUN = "*") # table containing the scale units (row: power of ten from -201 to +199, column: the 5, 2.5, 2, 1.25, 1 notches
        
        # recover the number of leading zeros in tempo_inter
        ini_scipen <- base::options()$scipen
        base::options(scipen = -9) # force scientific format
        if(base::any(base::grepl(pattern = "\\+", x = tempo_inter, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = TRUE)){ # tempo_inter > 1
            power10_exp <- base::as.integer(x = base::substring(text = tempo_inter, first = (base::regexpr(pattern = "\\+", text = tempo_inter, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) + 1), last = 1000000L)) # recover the power of 10. Example recover 08 from 1e+08
            mantisse <- base::as.numeric(x = base::substr(x = tempo_inter, start = 1, stop = (base::regexpr(pattern = "\\+", text = tempo_inter, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
        }else if(base::any(base::grepl(pattern = "\\-", x = tempo_inter, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = TRUE)){ # tempo_inter < 1
            power10_exp <- base::as.integer(x = base::substring(text = tempo_inter, first = (base::regexpr(pattern = "\\-", text = tempo_inter, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)), last = 1000000L)) # recover the power of 10. Example recover 08 from 1e+08
            mantisse <- base::as.numeric(x = base::substr(x = tempo_inter, start = 1, stop = (base::regexpr(pattern = "\\-", text = tempo_inter, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) - 2))) # recover the mantisse. Example recover 1.22 from 1.22e+08
        }else{ # nocov start (internal error: impossible to reach with argument values, because tempo_inter is always either in scientific notation with + or -, or a plain decimal)
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 1 IN ",
                intern_error_text_start,
                "CODE INCONSISTENCY.",
                intern_error_text_end,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        } # nocov end
        tempo_scale <- dec_table[log10_range == power10_exp, ]
        # new interval 
        inter_select <- NULL
        for(i1 in base::seq_along(along.with = tempo_scale)){
            if(base::is.null(x = inter_select)){ # loop stop condition instead of break(), which is not allowed by saferDev::colons_check()
                tempo_first_tick <- base::trunc(x = (tempo_min + tempo_scale[i1]) / tempo_scale[i1]) * (tempo_scale[i1]) # this would be use to have a number not multiple of tempo_scale[i1]: ceiling(tempo_min) + tempo_scale[i1] * 10^power10_exp
                tempo_last_tick <- tempo_first_tick + tempo_scale[i1] * (n - 1)
                if((tempo_first_tick >= tempo_min) & (tempo_last_tick <= tempo_max)){
                    inter_select <- tempo_scale[i1]
                }
            }
        }
        if(base::is.null(x = inter_select)){ # nocov start (internal error: impossible to reach with argument values, because at least one value of tempo_scale always fits between tempo_min and tempo_max)
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 2 IN ",
                intern_error_text_start,
                "CODE INCONSISTENCY.",
                intern_error_text_end,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        } # nocov end
        base::options(scipen = ini_scipen) # restore the initial scientific penalty
        # end new interval 
        # centering the new scale 
        tempo_mid <- base::trunc(x = (mid + (-1:1) * inter_select) / inter_select) * inter_select # tempo middle tick closest to the middle axis
        mid_tick <- tempo_mid[base::which.min(x = base::abs(x = tempo_mid - mid))]
        if(base::isTRUE(x = base::all.equal(target = n, current = base::rep(x = 1, base::length(x = n))))){ # isTRUE(all.equal(n, rep(1, length(n)))) is similar to n == 1L but deals with float
            output <- mid_tick
        }else if(base::isTRUE(x = base::all.equal(target = n, current = base::rep(x = 2, base::length(x = n))))){ # isTRUE(all.equal(n, rep(0, length(n)))) is similar to n == 2L but deals with float
            output <- mid_tick
            tempo_min_dist <- mid_tick - inter_select - tempo_min
            tempo_max_dist <- tempo_max - mid_tick + inter_select
            if(tempo_min_dist <= tempo_max_dist){ # distance between lowest tick and bottom axis <= distance between highest tick and top axis. If yes, extra tick but at the top, otherwise at the bottom
                output <- base::c(mid_tick, mid_tick + inter_select)
            }else{ # nocov start (unreachable: mid_tick is always within inter_select of mid, so mid_tick - inter_select - tempo_min <= tempo_max - mid_tick + inter_select always holds)
                output <- base::c(mid_tick - inter_select, mid_tick)
            } # nocov end
        }else if((n / 2 - base::trunc(x = n / 2)) > 0.1){ # > 0.1 to avoid floating point. Because result can only be 0 or 0.5. Thus, > 0.1 means odd number
            output <- base::c(mid_tick - (base::trunc(x = n / 2):1) * inter_select, mid_tick, mid_tick + (1:base::trunc(x = n / 2)) * inter_select)
        }else if((n / 2 - base::trunc(x = n / 2)) < 0.1){ # < 0.1 to avoid floating point. Because result can only be 0 or 0.5. Thus, < 0.1 means even number
            tempo_min_dist <- mid_tick - base::trunc(x = n / 2) * inter_select - tempo_min
            tempo_max_dist <- tempo_max - mid_tick + base::trunc(x = n / 2) * inter_select
            if(tempo_min_dist <= tempo_max_dist){ # distance between lowest tick and bottom axis <= distance between highest tick and top axis. If yes, extra tick but at the bottom, otherwise at the top
                output <- base::c(mid_tick - ((base::trunc(x = n / 2) - 1):1) * inter_select, mid_tick, mid_tick + (1:base::trunc(x = n / 2)) * inter_select)
            }else{ # nocov start (unreachable: same geometric argument as for n == 2, with mid_tick always within inter_select of mid)
                output <- base::c(mid_tick - (base::trunc(x = n / 2):1) * inter_select, mid_tick, mid_tick + (1:(base::trunc(x = n / 2) - 1)) * inter_select)
            } # nocov end
        }else{ # nocov start (internal error: impossible to reach with argument values, because n / 2 - trunc(n / 2) is exactly 0 or 0.5)
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 3 IN ",
                intern_error_text_start,
                "CODE INCONSISTENCY.",
                intern_error_text_end,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        } # nocov end
        # end centering the new scale 
        # last check
        if(base::min(output, na.rm = FALSE) < tempo_min){ # nocov start (unreachable: mid_tick is always within inter_select of mid, so min(output) = mid_tick - trunc(n/2) * inter_select >= tempo_min always holds)
            output <- base::c(output[-1], base::max(output, na.rm = FALSE) + inter_select) # remove the lowest tick and add a tick at the top
        }else if( base::max(output, na.rm = FALSE) > tempo_max){ # unreachable for the same geometric reason (kept for safety)
            output <- base::c(base::min(output, na.rm = FALSE) - inter_select, output[-base::length(x = output)])
        } # nocov end
        if(base::min(output, na.rm = FALSE) < tempo_min | base::max(output, na.rm = FALSE) > tempo_max){ # nocov start (internal error: unreachable after the shifts above, kept for safety)
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 4 IN ",
                intern_error_text_start,
                "CODE INCONSISTENCY.",
                intern_error_text_end,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        } # nocov end
        if(base::any(base::is.na(x = output), na.rm = FALSE)){ # nocov start (internal error: impossible to reach with valid argument values)
            tempo_cat <- base::paste0(
                "INTERNAL ERROR 5 IN ",
                intern_error_text_start,
                "CODE INCONSISTENCY: NA GENERATION.",
                intern_error_text_end,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        } # nocov end
        # end last check
    }else{ # nocov start (internal error: kind is already restricted to "approx", "strict" and "strict.cl" by arg_check())
        tempo_cat <- base::paste0(
            "INTERNAL ERROR 6 IN ",
            intern_error_text_start,
            "CODE INCONSISTENCY.",
            intern_error_text_end,
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    } # nocov end
    if(base::diff(x = lim_rank) < 0){
        output <- base::rev(x = output)
    }
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output
}

