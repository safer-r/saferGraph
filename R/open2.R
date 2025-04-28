#' @title open2
#' @description
#' Open a pdf or screen (GUI) graphic window and return initial graphic parameters.
#' 
#' This order can be used:
#' 
#' width()
#' 
#' open2()
#' 
#' prior_plot() # not for ggplot2
#' 
#' plot() or any other plotting
#' 
#' post_plot() if prior_plot() has been used # not for ggplot2
#' 
#' close2()
#' @param pdf Single logical value. Use pdf display? If FALSE, a GUI is opened.
#' @param pdf_path Single character string. Where the pdf is saved (do not terminate by / or \\). Write "working.dir" if working directory is required (default). Ignored if pdf == FALSE.
#' @param pdf_name Single character string. Name of the pdf file containing the graphs (the .pdf extension is added by the function, if not detected in the name end). Ignored if pdf == FALSE.
#' @param width Single positive numeric value indicating the width of the window (in inches).
#' @param height Single positive numeric value indicating the height of the window (in inches).
#' @param paper Single character string. Paper argument of the pdf function (paper format). Only used for pdf(). Either "a4", "letter", "legal", "us", "executive", "a4r", "USr" or "special". If "special", means that the paper dimension will be width and height. With another paper format, if width or height is over the size of the paper, width or height will be modified such that the plot is adjusted to the paper dimension (see $dim in the returned list below to see the modified dimensions). Ignored if pdf == FALSE.
#' @param pdf_overwrite Single logical value. Existing pdf can be overwritten? . Ignored if pdf == FALSE.
#' @param rescale Kind of GUI. Either "R", "fit", or "fixed". Ignored on Mac and Linux OS. See ?windows for details.
#' @param remove_read_only Single logical value. Remove the read only (R.O.) graphical parameters? If TRUE, the graphical parameters are returned without the R.O. parameters. The returned $ini.par list can be used to set the par() of a new graphical device. If FALSE, graphical parameters are returned with the R.O. parameters, which provides information like text dimension (see ?par() ). The returned $ini.par list can be used to set the par() of a new graphical device, but generate a warning message. Ignored if return.output == FALSE. 
#' @param return.output Single logical value. Return output ? If TRUE the output list is displayed.
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) correct lib_path argument value 2) required functions and related packages effectively present in local R lybraries and 3) R classical operators (like "<-") not overwritten by another package because of the R scope. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns
#' A list containing:
#' 
#' - $pdf.loc: path of the pdf created.
#' 
#' - $ini.par: initial par() parameters.
#' 
#' - $zone.ini: initial window spliting.
#' 
#' - $dim: dimension of the graphical device (in inches).
#' @details 
#' WARNINGS
#'
#' On Linux, use pdf = TRUE, if (GUI) graphic window is not always available, meaning that X is not installed (clusters for instance). Use X11() in R to test if available.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' \dontrun{
#' # Screen devices should not be used in examples
#' open2(pdf = FALSE, pdf_path = ".", pdf_name = "graph", width = 7, height = 7, paper = "special", pdf_overwrite = FALSE, return.output = TRUE)
#' }
#' @importFrom saferDev arg_check
#' @export
open2 <- function(
    pdf = TRUE, 
    pdf_path = "working.dir", 
    pdf_name = "graph", 
    width = 7, 
    height = 7, 
    paper = "special", 
    pdf_overwrite = FALSE, 
    rescale = "fixed", 
    remove_read_only = TRUE, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = "" 
){
    # DEBUGGING
    # pdf = TRUE ; pdf_path = "C:/Users/Gael/Desktop" ; pdf_name = "graphs" ; width = 7 ; height = 7 ; paper = "special" ; pdf_overwrite = FALSE ; rescale = "fixed" ; remove_read_only = TRUE ; return.output = TRUE ; safer_check = TRUE # for function debugging
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
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
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
    # use this in the error_text of safer functions if present below 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg with no default values
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "pdf", 
        "pdf_path", 
        "pdf_name", 
        "width", 
        "height", 
        "paper", 
        "pdf_overwrite", 
        "rescale", 
        "remove_read_only", 
        "return.output", 
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
                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
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
    if(safer_check == TRUE){
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
                ini_lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base:::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        pkg_log <- "saferDev" %in% base::rownames(x = utils::installed.packages(lib.loc = lib_path, priority = NULL, noCache = FALSE, fields = NULL, subarch = .Platform$r_arch), do.NULL = TRUE, prefix = "row")
        if( ! base::all(pkg_log, na.rm = TRUE)){
            tempo <- req_package[ ! pkg_log]
            tempo_cat <- base::paste0(
                error_text_start,  
                "REQUIRED PACKAGE", 
                base::ifelse(test = base::length(x = tempo) == 1L, yes = base::paste0(":\n", tempo, "\n\n", collapse = "\n", recycle0 = FALSE), no = base::paste0("S:\n", base::paste0(tempo, collapse = "\n", recycle0 = FALSE), "\n\n", collapse = "\n", recycle0 = FALSE)), 
                "MUST BE INSTALLED IN", 
                base::ifelse(test = base::length(x = lib_path) == 1L, yes = "", no = " ONE OF THESE FOLDERS"), 
                ":\n", 
                base::paste0(lib_path, collapse = "\n", recycle0 = FALSE),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        saferDev::is_function_here(
            fun = base::c(
                "saferDev::arg_check"
            ),
            safer_check = TRUE, #one must be true
            lib_path = lib_path, 
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
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
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = pdf, class = "logical", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = pdf_path, class = "character", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = pdf_name, class = "character", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = width, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = height, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = paper, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("a4", "letter", "legal", "us", "executive", "a4r", "USr", "special", "A4", "LETTER", "LEGAL", "US"), all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = pdf_overwrite, class = "logical", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = rescale, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("R", "fit", "fixed"), all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = remove_read_only, class = "logical", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = return.output, class = "logical", typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
    tempo_arg <- base::c(
        "pdf_path",
        "pdf_name"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    if(pdf_path == "working.dir"){
        pdf_path <- base::getwd()
    }else{
        if(base::grepl(x = pdf_path, pattern = ".+/$")){
            pdf_path <- base::sub(x = pdf_path, pattern = "/$", replacement = "") # remove the last /
        }else if(base::grepl(x = pdf_path, pattern = ".+[\\]$")){ # or ".+\\\\$" # cannot be ".+\$" because \$ does not exist contrary to \n
            pdf_path <- base::sub(x = pdf_path, pattern = "[\\]$", replacement = "") # remove the last /
        }
        if(base::dir.exists(pdf_path) == FALSE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\npdf_path ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY\n", pdf_path)
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # par.ini recovery
    # cannot use pdf(file = NULL), because some small differences between pdf() and other devices. For instance, differences with windows() for par()$fin, par()$pin and par()$plt
    if(base::Sys.info()["sysname"] == "Windows"){ # Note that .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
        open.fail <- NULL
        grDevices::windows()
        ini.par <- graphics::par(no.readonly = remove_read_only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
        base::invisible(grDevices::dev.off()) # close the new window
    }else if(base::Sys.info()["sysname"] == "Linux"){
        if(pdf == TRUE){# cannot use pdf(file = NULL), because some small differences between pdf() and other devices. For instance, differences with windows() for par()$fin, par()$pin and par()$plt
            if(base::exists(".Random.seed", envir = .GlobalEnv)){ # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
                tempo.random.seed <- .Random.seed
                base::on.exit(base::assign(".Random.seed", tempo.random.seed, envir = .GlobalEnv))
            }else{
                base::on.exit(base::set.seed(NULL)) # inactivate seeding -> return to complete randomness
            }
            base::set.seed(NULL)
            tempo.code <- base::sample(x = 1:1e7, size = 1)
            while(base::file.exists(base::paste0(pdf_path, "/recover_ini_par", tempo.code, ".pdf")) == TRUE){
                tempo.code <- tempo.code + 1
            }
            grDevices::pdf(width = width, height = height, file=base::paste0(pdf_path, "/recover_ini_par", tempo.code, ".pdf"), paper = paper)
            ini.par <- graphics::par(no.readonly = remove_read_only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
            base::invisible(grDevices::dev.off()) # close the pdf window
            base::file.remove(base::paste0(pdf_path, "/recover_ini_par", tempo.code, ".pdf")) # remove the pdf file
        }else{
            # test if X11 can be opened
            if(base::file.exists(base::paste0(base::getwd(), "/Rplots.pdf"))){
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHIS FUNCTION CANNOT BE USED ON LINUX IF A Rplots.pdf FILE ALREADY EXISTS HERE\n", base::getwd())
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                open.fail <- base::suppressWarnings(base::try(grDevices::X11(), silent = TRUE))[] # try to open a X11 window. If open.fail == NULL, no problem, meaning that the X11 window is opened. If open.fail != NULL, a pdf can be opened here base::paste0(getwd(), "/Rplots.pdf")
                if(base::is.null(open.fail)){
                    ini.par <- graphics::par(no.readonly = remove_read_only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
                    base::invisible(grDevices::dev.off()) # close the new window
                }else if(base::file.exists(base::paste0(base::getwd(), "/Rplots.pdf"))){
                    base::file.remove(base::paste0(base::getwd(), "/Rplots.pdf")) # remove the pdf file
                    tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM\nTO OVERCOME THIS, EITHER SET THE X GRAPHIC INTERFACE OF THE SYSTEM OR SET THE pdf ARGUMENT OF THE ", function.name, " FUNCTION TO TRUE AND RERUN")
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
            }
        }
    }else{
        open.fail <- NULL
        grDevices::quartz()
        ini.par <- graphics::par(no.readonly = remove_read_only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
        base::invisible(grDevices::dev.off()) # close the new window
    }
    # end par.ini recovery 
    zone.ini <- base::matrix(1, ncol=1) # to recover the initial parameters for next figure region when device region split into several figure regions
    if(pdf == TRUE){
        if(base::grepl(x = pdf_name, pattern = "\\.pdf$")){
            pdf_name <- base::sub(x = pdf_name, pattern = "\\.pdf$", replacement = "") # remove the last .pdf
        }
        pdf.loc <- base::paste0(pdf_path, "/", pdf_name, ".pdf")
        if(base::file.exists(pdf.loc) == TRUE & pdf_overwrite == FALSE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", pdf.loc, " FILE ALREADY EXISTS AND CANNOT BE OVERWRITTEN DUE TO pdf_overwrite ARGUMENT SET TO FALSE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            grDevices::pdf(width = width, height = height, file=pdf.loc, paper = paper)
        }
    }else if(pdf == FALSE){
        pdf.loc <- NULL
        if(base::Sys.info()["sysname"] == "Windows"){ # .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
            grDevices::windows(width = width, height = height, rescale = rescale)
        }else if(base::Sys.info()["sysname"] == "Linux"){
            if( ! base::is.null(open.fail)){
                tempo.cat <- base::paste0("ERROR IN ", function.name, " \nTHIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM\nTO OVERCOME THIS, EITHER SET THE X GRAPHIC INTERFACE OF THE SYSTEM OR SET THE pdf ARGUMENT OF THE ", function.name, " FUNCTION TO TRUE AND RERUN")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                grDevices::X11(width = width, height = height)
            }
        }else{
            grDevices::quartz(width = width, height = height)
        }
    }
    #### end main code

    #### warning output
    #### end warning output

    #### output
    if(return.output == TRUE){
        output <- base::list(pdf.loc = pdf.loc, ini.par = ini.par, zone.ini = zone.ini, dim = grDevices::dev.size())
        base::return(output)
    }
    #### end output
}