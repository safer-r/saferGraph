#' @title post_plot
#' @description
#' Redesign axis. If x.side = 0, y.side = 0, the function just adds text at topright of the graph and reset par() for next graphics and provides outputs (see below).
#' 
#' Provide also positions for legend or additional text on the graph.
#' 
#' Use prior_plot() before this function for initial inactivation of the axis drawings.
#' @param x.side Single integer value indicating the axis at the bottom (1) or top (3) of the region figure. Write 0 for no change.
#' @param x.log.scale Single logical value. Log scale for the x-axis?
#' @param x.categ Vector of character indicating the categories when the x-axis is qualitative(stripchart, boxplot).
#' @param x.categ.pos Numeric vector of identical length than x.categ indicating the position of the categories names. If NULL, this will be 1:length(levels()).
#' @param x.lab Single character string of the label of the x-axis. If x.side == 0 and x.lab != "", then x.lab is printed.
#' @param x.axis.size Single positive numeric value to increase or decrease the size of the x axis numbers. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2. Also control the size of displayed categories.
#' @param x.label.size Single positive numeric value to increase or decrease the size of the x axis legend text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2.
#' @param x.dist.legend Single positive numeric value to move x-axis legends away (first number of mgp argument of par() but in inches).
#' @param x.nb.inter.tick Single positive integer value indicating the number of secondary ticks between main ticks on x-axis (only if not log scale). 0 means no secondary ticks.
#' @param y.side Single integer of either 0, 2 or 4 value to display the axis at the left (2) or right (4) of the region figure. Write 0 for no change.
#' @param y.log.scale Single logical value. Log scale for the y-axis?
#' @param y.categ Vector of character indicating the categories when the y-axis is qualitative(stripchart, boxplot).
#' @param y.categ.pos Numeric vector of identical length than y.categ indicating the position of the categories names. If NULL, this will be 1:length(levels()).
#' @param y.lab Label of the y-axis. If y.side == 0 and y.lab != "", then y.lab is printed.
#' @param y.axis.size Single positive numeric value to increase or decrease the size of the y axis numbers. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2. Also control the size of displayed categories.
#' @param y.label.size Single positive numeric value to increase or decrease the size of the y axis legend text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2.
#' @param y.dist.legend Single positive numeric value to move y-axis legends away (first number of mgp argument of par() but in inches).
#' @param y.nb.inter.tick Single positive integer value indicating the number of secondary ticks between main ticks on y-axis (only if not log scale). 0 means no secondary ticks.
#' @param text.angle Single numeric value for the angle of the text when axis is qualitative.
#' @param tick.length Single positive proportion value indicating the length of the main ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc., 0 for no ticks).
#' @param sec.tick.length Single positive proportion value indicating the length of the secondary ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc., 0 for no ticks).
#' @param bg.color Background color of the plot region. Either (1) NULL (no color), or (2) a single character string or integer. Color can be a color name (see ?colors() in R), an hexadecimal color code, or an integer (according to palette()). BEWARE: cover/hide an existing plot !
#' @param grid.lwd Vector of positive numeric values. If non NULL, both activate the grid lines and specify the line widths.
#' @param grid.col Grid line colors (only if grid.lwd non NULL). Either (1) NULL (no color), or (2) a vector of character strings or integers. Color can be color names (see ?colors() in R), hexadecimal color codes, or integers (according to palette()).
#' @param corner.text Single character string adding a text at the top right corner of the window.
#' @param corner.text.size Single positive numeric value to increase or decrease the size of the text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2.
#' @param par.reset Single logical value that resets all the graphics parameters. BEWARE: TRUE can generate display problems, mainly in graphic devices with multiple figure regions.
#' @param just.label.add Single logical value that just add axis labels (legend). If TRUE, at least (x.side == 0 & x.lab != "") or (y.side == 0 & y.lab != "") must be set to display the corresponding x.lab or y.lab.
#' @param custom.par List that provides the parameters that reset all the graphics parameters. BEWARE: if NULL and par.reset == TRUE, the default par() parameters are used.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{c(lib_path, .libPaths())}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns 
#' A list containing: 
#' 
#' - $x.mid.left.dev.region: middle of the left margin of the device region, in coordinates of the x-axis.
#' 
#' - $x.left.dev.region: left side of the left margin (including the potential margin of the device region), in coordinates of the x-axis.
#' 
#' - $x.mid.right.dev.region: middle of the right margin of the device region, in coordinates of the x-axis.
#' 
#' - $x.right.dev.region: right side of the right margin (including the potential margin of the device region), in coordinates of the x-axis.
#' 
#' - $x.mid.left.fig.region: middle of the left margin of the figure region, in coordinates of the x-axis.
#' 
#' - $x.left.fig.region: left side of the left margin, in coordinates of the x-axis.
#' 
#' - $x.mid.right.fig.region: middle of the right margin of the figure region, in coordinates of the x-axis.
#' 
#' - $x.right.fig.region: right side of the right margin, in coordinates of the x-axis.
#' 
#' - $x.left.plot.region: left side of the plot region, in coordinates of the x-axis.
#' 
#' - $x.right.plot.region: right side of the plot region, in coordinates of the x-axis.
#' 
#' - $x.mid.plot.region: middle of the plot region, in coordinates of the x-axis.
#' 
#' - $y.mid.bottom.dev.region: middle of the bottom margin of the device region, in coordinates of the y-axis.
#' 
#' - $y.bottom.dev.region: bottom side of the bottom margin (including the potential margin of the device region), in coordinates of the y-axis.
#' 
#' - $y.mid.top.dev.region: middle of the top margin of the device region, in coordinates of the y-axis.
#' 
#' - $y.top.dev.region: top side of the top margin (including the potential margin of the device region), in coordinates of the y-axis.
#' 
#' - $y.mid.bottom.fig.region: middle of the bottom margin of the figure region, in coordinates of the y-axis.
#' 
#' - $y.bottom.fig.region: bottom of the bottom margin of the figure region, in coordinates of the y-axis.
#' 
#' - $y.mid.top.fig.region: middle of the top margin of the figure region, in coordinates of the y-axis.
#' 
#' - $y.top.fig.region: top of the top margin of the figure region, in coordinates of the y-axis.
#' 
#' - $y.top.plot.region: top of the plot region, in coordinates of the y-axis.
#' 
#' - $y.bottom.plot.region: bottom of the plot region, in coordinates of the y-axis.
#' 
#' - $y.mid.plot.region: middle of the plot region, in coordinates of the y-axis.
#' 
#' - $text: warning text
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' \dontrun{
#' # Screen devices should not be used in examples
#' 
#' # Example of log axis with redrawn x-axis and y-axis:
#'
#' prior.par <- prior_plot(param.reinitial = TRUE) ; 
#' plot(1:100) ; 
#' post_plot(x.side = 1, x.lab = "Values", y.side = 2, y.lab = "TEST", y.axis.size = 1, y.label.size = 2, y.dist.legend = 0.6)
#' }
#' @importFrom saferDev arg_check
#' @export
post_plot <- function(
        x.side = 0, 
        x.log.scale = FALSE, 
        x.categ = NULL, 
        x.categ.pos = NULL, 
        x.lab = "", 
        x.axis.size = 1.5, 
        x.label.size = 1.5, 
        x.dist.legend = 0.5, 
        x.nb.inter.tick = 1, 
        y.side = 0, 
        y.log.scale = FALSE, 
        y.categ = NULL, 
        y.categ.pos = NULL, 
        y.lab = "", 
        y.axis.size = 1.5, 
        y.label.size = 1.5, 
        y.dist.legend = 0.5, 
        y.nb.inter.tick = 1, 
        text.angle = 90, 
        tick.length = 0.5, 
        sec.tick.length = 0.3, 
        bg.color = NULL, 
        grid.lwd = NULL, 
        grid.col = "white", 
        corner.text = "", 
        corner.text.size = 1, 
        just.label.add = FALSE, 
        par.reset = FALSE, 
        custom.par = NULL,
        safer_check = TRUE, 
        lib_path = NULL, 
        error_text = "" 

){
    # DEBUGGING
    # x.side = 0 ; x.log.scale = FALSE ; x.categ = NULL ; x.categ.pos = NULL ; x.lab = "" ; x.axis.size = 1.5 ; x.label.size = 1.5 ; x.dist.legend = 1 ; x.nb.inter.tick = 1 ; y.side = 0 ; y.log.scale = FALSE ; y.categ = NULL ; y.categ.pos = NULL ; y.lab = "" ; y.axis.size = 1.5 ; y.label.size = 1.5 ; y.dist.legend = 0.7 ; y.nb.inter.tick = 1 ; text.angle = 90 ; tick.length = 0.5 ; sec.tick.length = 0.3 ; bg.color = NULL ; grid.lwd = NULL ; grid.col = "white" ; corner.text = "" ; corner.text.size = 1 ; just.label.add = FALSE ; par.reset = FALSE ; custom.par = NULL ; safer_check = TRUE# for function debugging



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
    if(base::any(x = tempo_log, na.rm = TRUE)) {
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
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "x.side", 
        "x.log.scale", 
        # "x.categ", # inactivated because can be null
        # "x.categ.pos", # inactivated because can be null
        "x.lab", 
        "x.axis.size", 
        "x.label.size", 
        "x.dist.legend", 
        "x.nb.inter.tick", 
        "y.side", 
        "y.log.scale", 
        # "y.categ", # inactivated because can be null
        # "y.categ.pos", # inactivated because can be null
        "y.lab", 
        "y.axis.size", 
        "y.label.size", 
        "y.dist.legend", 
        "y.nb.inter.tick" , 
        "text.angle", 
        "tick.length", 
        "sec.tick.length", 
        # "bg.color", # inactivated because can be null
        # "grid.lwd", # inactivated because can be null
        "grid.col", 
        "corner.text", 
        "corner.text.size", 
        "just.label.add", 
        "par.reset",
        # "custom.par", # inactivated because can be null
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
        "x.side", 
        "x.log.scale", 
        "x.categ",  
        "x.categ.pos",  
        # "x.lab", 
        "x.axis.size", 
        "x.label.size", 
        "x.dist.legend", 
        "x.nb.inter.tick", 
        "y.side", 
        "y.log.scale", 
        "y.categ",  
        "y.categ.pos",  
        # "y.lab", 
        "y.axis.size", 
        "y.label.size", 
        "y.dist.legend", 
        "y.nb.inter.tick" , 
        "text.angle", 
        "tick.length", 
        "sec.tick.length", 
        "bg.color",  
        "grid.lwd",  
        "grid.col", 
        # "corner.text", 
        "corner.text.size", 
        "just.label.add", 
        "par.reset",
        "custom.par",  
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
    if(safer_check == TRUE){ # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
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
    }  # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = , envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check", # write each function preceeded by their package name
                # end functions required in this code
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

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = , envir = )
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
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
   tempo <- saferDev::arg_check(data = x.side, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c(0, 1, 3), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.log.scale, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    if( ! base::is.null(x.categ)){
        tempo <- saferDev::arg_check(data = x.categ, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    }
    if( ! base::is.null(x.categ.pos)){
        tempo <- saferDev::arg_check(data = x.categ.pos, class = "vector", typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = x.lab, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.axis.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.label.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.dist.legend, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.nb.inter.tick, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.side, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c(0, 2, 4), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.log.scale, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    if( ! base::is.null(y.categ)){
        tempo <- saferDev::arg_check(data = y.categ, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    }
    if( ! base::is.null(y.categ.pos)){
        tempo <- saferDev::arg_check(data = y.categ.pos, class = "vector", typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = y.lab, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.axis.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.label.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.dist.legend, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.nb.inter.tick, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = text.angle, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = tick.length, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = sec.tick.length, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    if( ! base::is.null(bg.color)){
        tempo <- saferDev::arg_check(data = bg.color, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
        if( ! (bg.color %in% grDevices::colors() | base::grepl(pattern = "^#", bg.color))){ # check color
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nbg.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY grDevices::colors()")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    if( ! base::is.null(grid.lwd)){
        tempo <- saferDev::arg_check(data = grid.lwd, class = "vector", typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    }
    if( ! base::is.null(grid.col)){
        tempo <- saferDev::arg_check(data = grid.col, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
        if( ! (grid.col %in% grDevices::colors() | base::grepl(pattern = "^#", grid.col))){ # check color
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\ngrid.col ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY grDevices::colors()")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = corner.text, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = corner.text.size, class = "vector", typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = just.label.add, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = par.reset, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    if( ! base::is.null(custom.par)){
        tempo <- saferDev::arg_check(data = custom.par, typeof = "list", mode = NULL, length = 1, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(ee)
    }
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
    # optional section: remove the code if you do not want to check if arguments of mode character of your own function cannot contain ""
    tempo_arg <- base::c(
        "x.categ",   
        # "x.lab", 
        "y.categ",   
        # "y.lab", 
        "bg.color",  
        "grid.col" 
        # "corner.text", 
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
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
        # nocov end
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
    text <- NULL
    graphics::par(tcl = -graphics::par()$mgp[2] * tick.length)
    if(x.log.scale == TRUE){
        grid.coord.x <- base::c(10^graphics::par("usr")[1], 10^graphics::par("usr")[2])
    }else{
        grid.coord.x <- base::c(graphics::par("usr")[1], graphics::par("usr")[2])
    }
    if(y.log.scale == TRUE){
        grid.coord.y <- base::c(10^graphics::par("usr")[3], 10^graphics::par("usr")[4])
    }else{
        grid.coord.y <- base::c(graphics::par("usr")[3], graphics::par("usr")[4])
    }
    if( ! base::is.null(bg.color)){
        graphics::rect(grid.coord.x[1], grid.coord.y[1], grid.coord.x[2], grid.coord.y[2], col = bg.color, border = NA)
    }
    if( ! base::is.null(grid.lwd)){
        graphics::grid(nx = NA, ny = NULL, col = grid.col, lty = 1, lwd = grid.lwd)
    }
    if(x.log.scale == TRUE){
        x.mid.left.dev.region <- 10^(graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * graphics::par("omd")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
        x.left.dev.region <- 10^(graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * graphics::par("omd")[1]) # in x coordinates
        x.mid.right.dev.region <- 10^(graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * (1 - graphics::par("omd")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
        x.right.dev.region <- 10^(graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * (1 - graphics::par("omd")[2])) # in x coordinates
        x.mid.left.fig.region <- 10^(graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
        x.left.fig.region <- 10^(graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1]) # in x coordinates
        x.mid.right.fig.region <- 10^(graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
        x.right.fig.region <- 10^(graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2])) # in x coordinates
        x.left.plot.region <- 10^graphics::par("usr")[1] # in x coordinates, left of the plot region (according to x scale)
        x.right.plot.region <- 10^graphics::par("usr")[2] # in x coordinates, right of the plot region (according to x scale)
        x.mid.plot.region <- 10^((graphics::par("usr")[2] + graphics::par("usr")[1]) / 2) # in x coordinates, right of the plot region (according to x scale)
    }else{
        x.mid.left.dev.region <- (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * graphics::par("omd")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
        x.left.dev.region <- (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * graphics::par("omd")[1]) # in x coordinates
        x.mid.right.dev.region <- (graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * (1 - graphics::par("omd")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
        x.right.dev.region <- (graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * (1 - graphics::par("omd")[2])) # in x coordinates
        x.mid.left.fig.region <- (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
        x.left.fig.region <- (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1]) # in x coordinates
        x.mid.right.fig.region <- (graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
        x.right.fig.region <- (graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2])) # in x coordinates
        x.left.plot.region <- graphics::par("usr")[1] # in x coordinates, left of the plot region (according to x scale)
        x.right.plot.region <- graphics::par("usr")[2] # in x coordinates, right of the plot region (according to x scale)
        x.mid.plot.region <- (graphics::par("usr")[2] + graphics::par("usr")[1]) / 2 # in x coordinates, right of the plot region (according to x scale)
    }
    if(y.log.scale == TRUE){
        y.mid.bottom.dev.region <- 10^(graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (graphics::par("omd")[3] / 2)) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
        y.bottom.dev.region <- 10^(graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * graphics::par("omd")[3]) # in y coordinates
        y.mid.top.dev.region <- 10^(graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (1 - graphics::par("omd")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
        y.top.dev.region <- 10^(graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (1 - graphics::par("omd")[4])) # in y coordinates
        y.mid.bottom.fig.region <- 10^(graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] / 2) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
        y.bottom.fig.region <- 10^(graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3]) # in y coordinates
        y.mid.top.fig.region <- 10^(graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
        y.top.fig.region <- 10^(graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4])) # in y coordinates
        y.top.plot.region <- 10^graphics::par("usr")[4] # in y coordinates, top of the plot region (according to y scale)
        y.bottom.plot.region <- 10^graphics::par("usr")[3] # in y coordinates, bottom of the plot region (according to y scale)
        y.mid.plot.region <- (graphics::par("usr")[3] + graphics::par("usr")[4]) / 2 # in x coordinates, right of the plot region (according to x scale)
    }else{
        y.mid.bottom.dev.region <- (graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (graphics::par("omd")[3] / 2)) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
        y.bottom.dev.region <- (graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * graphics::par("omd")[3]) # in y coordinates
        y.mid.top.dev.region <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (1 - graphics::par("omd")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
        y.top.dev.region <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (1 - graphics::par("omd")[4])) # in y coordinates
        y.mid.bottom.fig.region <- (graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] / 2) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
        y.bottom.fig.region <- (graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3]) # in y coordinates
        y.mid.top.fig.region <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
        y.top.fig.region <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4])) # in y coordinates
        y.top.plot.region <- graphics::par("usr")[4] # in y coordinates, top of the plot region (according to y scale)
        y.bottom.plot.region <- graphics::par("usr")[3] # in y coordinates, bottom of the plot region (according to y scale)
        y.mid.plot.region <- ((graphics::par("usr")[3] + graphics::par("usr")[4]) / 2) # in x coordinates, right of the plot region (according to x scale)
    }
    if(base::any(base::sapply(FUN = all.equal, base::c(1, 3), x.side) == TRUE, na.rm = TRUE)){
        graphics::par(xpd=FALSE, xaxt="s")
        if(base::is.null(x.categ) & x.log.scale == TRUE){
            if(base::any(graphics::par()$xaxp[1:2] == 0L, na.rm = TRUE)){ # any(sapply(FUN = all.equal, graphics::par()$xaxp[1:2], 0) == TRUE) not used because we strictly need zero as a result. Beware: write "== TRUE", because the result is otherwise character and a warning message appears using any()
                if(graphics::par()$xaxp[1] == 0L){ # isTRUE(all.equal(graphics::par()$xaxp[1], 0)) not used because we strictly need zero as a result
                    graphics::par(xaxp = base::c(10^-30, graphics::par()$xaxp[2:3])) # because log10(graphics::par()$xaxp[1] == 0) == -Inf
                }
                if(graphics::par()$xaxp[2] == 0L){ # isTRUE(all.equal(graphics::par()$xaxp[1], 0)) not used because we strictly need zero as a result
                    graphics::par(xaxp = base::c(graphics::par()$xaxp[1], 10^-30, graphics::par()$xaxp[3])) # because log10(graphics::par()$xaxp[2] == 0) == -Inf
                }
            }
            graphics::axis(side = x.side, at = base::c(10^graphics::par()$usr[1], 10^graphics::par()$usr[2]), labels=base::rep("", 2), lwd=1, lwd.ticks = 0) # draw the axis line
            graphics::mtext(side = x.side, text = x.lab, line = x.dist.legend / 0.2, las = 0, cex = x.label.size)
            graphics::par(tcl = -graphics::par()$mgp[2] * sec.tick.length) # length of the secondary ticks are reduced
            base::suppressWarnings(graphics::rug(10^base::outer(base::c((base::log10(graphics::par("xaxp")[1]) -1):base::log10(graphics::par("xaxp")[2])), base::log10(1:10), "+"), ticksize = NA, side = x.side)) # ticksize = NA to allow the use of graphics::par()$tcl value
            graphics::par(tcl = -graphics::par()$mgp[2] * tick.length) # back to main ticks
            graphics::axis(side = x.side, at = base::c(1e-15, 1e-14, 1e-13, 1e-12, 1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10), labels = base::expression(10^-15, 10^-14, 10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10), lwd = 0, lwd.ticks = 1, cex.axis = x.axis.size)
            x.text <- 10^graphics::par("usr")[2]
        }else if(base::is.null(x.categ) & x.log.scale == FALSE){
            graphics::axis(side=x.side, at=base::c(graphics::par()$usr[1], graphics::par()$usr[2]), labels=base::rep("", 2), lwd=1, lwd.ticks=0) # draw the axis line
            graphics::axis(side=x.side, at=base::round(base::seq(graphics::par()$xaxp[1], graphics::par()$xaxp[2], length.out=graphics::par()$xaxp[3]+1), 2), cex.axis = x.axis.size) # axis(side=x.side, at=round(seq(graphics::par()$xaxp[1], graphics::par()$xaxp[2], length.out=graphics::par()$xaxp[3]+1), 2), labels = format(round(seq(graphics::par()$xaxp[1], graphics::par()$xaxp[2], length.out=graphics::par()$xaxp[3]+1), 2), big.mark=','), cex.axis = x.axis.size) # to get the 1000 comma separator
            graphics::mtext(side = x.side, text = x.lab, line = x.dist.legend / 0.2, las = 0, cex = x.label.size)
            if(x.nb.inter.tick > 0){
                inter.tick.unit <- (graphics::par("xaxp")[2] - graphics::par("xaxp")[1]) / graphics::par("xaxp")[3]
                graphics::par(tcl = -graphics::par()$mgp[2] * sec.tick.length) # length of the ticks are reduced
                base::suppressWarnings(graphics::rug(base::seq(graphics::par("xaxp")[1] - 10 * inter.tick.unit, graphics::par("xaxp")[2] + 10 * inter.tick.unit, by = inter.tick.unit / (1 + x.nb.inter.tick)), ticksize = NA, x.side)) # ticksize = NA to allow the use of graphics::par()$tcl value
                graphics::par(tcl = -graphics::par()$mgp[2] * tick.length) # back to main ticks
            }
            x.text <- graphics::par("usr")[2]
        }else if(( ! base::is.null(x.categ)) & x.log.scale == FALSE){
            if(base::is.null(x.categ.pos)){
                x.categ.pos <- 1:base::length(x.categ)
            }else if(base::length(x.categ.pos) != base::length(x.categ)){
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nx.categ.pos MUST BE THE SAME LENGTH AS x.categ")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            graphics::par(xpd = TRUE)
            if(base::isTRUE(base::all.equal(x.side, 1))){ #isTRUE(all.equal(x.side, 1)) is similar to x.side == 1L but deals with float
                graphics::segments(x0 = x.left.plot.region, x1 = x.right.plot.region, y0 = y.bottom.plot.region, y1 = y.bottom.plot.region) # draw the line of the axis
                graphics::text(x = x.categ.pos, y = y.mid.bottom.fig.region, labels = x.categ, srt = text.angle, cex = x.axis.size)
            }else if(base::isTRUE(base::all.equal(x.side, 3))){ #isTRUE(all.equal(x.side, 1)) is similar to x.side == 3L but deals with float
                graphics::segments(x0 = x.left.plot.region, x1 = x.right.plot.region, y0 = y.top.plot.region, y1 = y.top.plot.region) # draw the line of the axis
                graphics::text(x = x.categ.pos, y = y.mid.top.fig.region, labels = x.categ, srt = text.angle, cex = x.axis.size)
            }else{
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT x.side CAN ONLY BE 1 OR 3")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            graphics::par(xpd = FALSE)
            x.text <- graphics::par("usr")[2]
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nPROBLEM WITH THE x.side (", x.side ,") OR x.log.scale (", x.log.scale,") ARGUMENTS")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else{
        x.text <- graphics::par("usr")[2]
    }
    if(base::any(base::sapply(FUN = all.equal, base::c(2, 4), y.side) == TRUE, na.rm = TRUE)){
        graphics::par(xpd=FALSE, yaxt="s")
        if(base::is.null(y.categ) & y.log.scale == TRUE){
            if(base::any(graphics::par()$yaxp[1:2] == 0L, na.rm = TRUE)){ # any(sapply(FUN = all.equal, graphics::par()$yaxp[1:2], 0) == TRUE) not used because we strictly need zero as a result. Beware: write "== TRUE", because the result is otherwise character and a warning message appears using any()
                if(graphics::par()$yaxp[1] == 0L){ # strict zero needed
                    graphics::par(yaxp = base::c(10^-30, graphics::par()$yaxp[2:3])) # because log10(graphics::par()$yaxp[1] == 0) == -Inf
                }
                if(graphics::par()$yaxp[2] == 0L){ # strict zero needed
                    graphics::par(yaxp = base::c(graphics::par()$yaxp[1], 10^-30, graphics::par()$yaxp[3])) # because log10(graphics::par()$yaxp[2] == 0) == -Inf
                }
            }
            graphics::axis(side=y.side, at=base::c(10^graphics::par()$usr[3], 10^graphics::par()$usr[4]), labels=base::rep("", 2), lwd=1, lwd.ticks=0) # draw the axis line
            graphics::par(tcl = -graphics::par()$mgp[2] * sec.tick.length) # length of the ticks are reduced
            base::suppressWarnings(graphics::rug(10^base::outer(base::c((base::log10(graphics::par("yaxp")[1])-1):base::log10(graphics::par("yaxp")[2])), base::log10(1:10), "+"), ticksize = NA, side = y.side)) # ticksize = NA to allow the use of graphics::par()$tcl value
            graphics::par(tcl = -graphics::par()$mgp[2] * tick.length) # back to main tick length
            graphics::axis(side = y.side, at = base::c(1e-15, 1e-14, 1e-13, 1e-12, 1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10), labels = base::expression(10^-15, 10^-14, 10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10), lwd = 0, lwd.ticks = 1, cex.axis = y.axis.size)
            y.text <- 10^(graphics::par("usr")[4] + (graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3]) * (1 - graphics::par("plt")[4]))
            graphics::mtext(side = y.side, text = y.lab, line = y.dist.legend / 0.2, las = 0, cex = y.label.size)
        }else if(base::is.null(y.categ) & y.log.scale == FALSE){
            graphics::axis(side=y.side, at=base::c(graphics::par()$usr[3], graphics::par()$usr[4]), labels=base::rep("", 2), lwd=1, lwd.ticks=0) # draw the axis line
            graphics::axis(side=y.side, at=base::round(base::seq(graphics::par()$yaxp[1], graphics::par()$yaxp[2], length.out=graphics::par()$yaxp[3]+1), 2), cex.axis = y.axis.size)
            graphics::mtext(side = y.side, text = y.lab, line = y.dist.legend / 0.2, las = 0, cex = y.label.size)
            if(y.nb.inter.tick > 0){
                inter.tick.unit <- (graphics::par("yaxp")[2] - graphics::par("yaxp")[1]) / graphics::par("yaxp")[3]
                graphics::par(tcl = -graphics::par()$mgp[2] * sec.tick.length) # length of the ticks are reduced
                base::suppressWarnings(graphics::rug(base::seq(graphics::par("yaxp")[1] - 10 * inter.tick.unit, graphics::par("yaxp")[2] + 10 * inter.tick.unit, by = inter.tick.unit / (1 + y.nb.inter.tick)), ticksize = NA, side=y.side)) # ticksize = NA to allow the use of graphics::par()$tcl value
                graphics::par(tcl = -graphics::par()$mgp[2] * tick.length) # back to main tick length
            }
            y.text <- (graphics::par("usr")[4] + (graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3]) * (1 - graphics::par("plt")[4]))
        }else if(( ! base::is.null(y.categ)) & y.log.scale == FALSE){
            if(base::is.null(y.categ.pos)){
                y.categ.pos <- 1:base::length(y.categ)
            }else if(base::length(y.categ.pos) != base::length(y.categ)){
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\ny.categ.pos MUST BE THE SAME LENGTH AS y.categ")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            graphics::axis(side = y.side, at = y.categ.pos, labels = base::rep("", base::length(y.categ)), lwd=0, lwd.ticks=1) # draw the line of the axis
            graphics::par(xpd = TRUE)
            if(base::isTRUE(base::all.equal(y.side, 2))){ #isTRUE(all.equal(y.side, 2)) is similar to y.side == 2L but deals with float
                graphics::text(x = x.mid.left.fig.region, y = y.categ.pos, labels = y.categ, srt = text.angle, cex = y.axis.size)
            }else if(base::isTRUE(base::all.equal(y.side, 4))){ # idem
                graphics::text(x = x.mid.right.fig.region, y = y.categ.pos, labels = y.categ, srt = text.angle, cex = y.axis.size)
            }else{
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT y.side CAN ONLY BE 2 OR 4")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            graphics::par(xpd = FALSE)
            y.text <- (graphics::par("usr")[4] + (graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3]) * (1 - graphics::par("plt")[4]))
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nPROBLEM WITH THE y.side (", y.side ,") OR y.log.scale (", y.log.scale,") ARGUMENTS")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else{
        y.text <- (graphics::par("usr")[4] + (graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3]) * (1 - graphics::par("plt")[4]))
    }
    graphics::par(xpd=NA)
    graphics::text(x = x.mid.right.fig.region, y = y.text, corner.text, adj=base::c(1, 1.1), cex = corner.text.size) # text at the topright corner. Replace x.right.fig.region by x.text if text at the right edge of the plot region
    if(just.label.add == TRUE & base::isTRUE(base::all.equal(x.side, 0)) & x.lab != ""){
        graphics::text(x = x.mid.plot.region, y = y.mid.bottom.fig.region, x.lab, adj=base::c(0.5, 0.5), cex = x.label.size) # x label
    }
    if(just.label.add == TRUE & base::isTRUE(base::all.equal(y.side, 0)) & y.lab != ""){
        graphics::text(x = y.mid.plot.region, y = x.mid.left.fig.region, y.lab, adj=base::c(0.5, 0.5), cex = y.label.size) # x label
    }
    graphics::par(xpd=FALSE)
    if(par.reset == TRUE){
        tempo.par <- base::open(pdf = FALSE, return.output = TRUE)
        base::invisible(grDevices::dev.off()) # close the new window
        if( ! base::is.null(custom.par)){
            if( ! base::names(custom.par) %in% base::names(tempo.par$ini.par)){
                tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\ncustom.par ARGUMENT SHOULD HAVE THE NAMES OF THE COMPARTMENT LIST COMING FROM THE graphics::par() LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            grDevices::colors(custom.par)
            text <- base::c(text, "\nGRAPH PARAMETERS SET TO VALUES DEFINED BY custom.par ARGUMENT\n")
        }else{
            grDevices::colors(tempo.par$ini.par)
            text <- base::c(text, "\nGRAPH PARAMETERS RESET TO graphics::par() DEFAULT VALUES\n")
        }
    }
    output <- base::list(x.mid.left.dev.region = x.mid.left.dev.region, x.left.dev.region = x.left.dev.region, x.mid.right.dev.region = x.mid.right.dev.region, x.right.dev.region = x.right.dev.region, x.mid.left.fig.region = x.mid.left.fig.region, x.left.fig.region = x.left.fig.region, x.mid.right.fig.region = x.mid.right.fig.region, x.right.fig.region = x.right.fig.region, x.left.plot.region = x.left.plot.region, x.right.plot.region = x.right.plot.region, x.mid.plot.region = x.mid.plot.region, y.mid.bottom.dev.region = y.mid.bottom.dev.region, y.bottom.dev.region = y.bottom.dev.region, y.mid.top.dev.region = y.mid.top.dev.region, y.top.dev.region = y.top.dev.region, y.mid.bottom.fig.region = y.mid.bottom.fig.region, y.bottom.fig.region = y.bottom.fig.region, y.mid.top.fig.region = y.mid.top.fig.region, y.top.fig.region = y.top.fig.region, y.top.plot.region = y.top.plot.region, y.bottom.plot.region = y.bottom.plot.region, y.mid.plot.region = y.mid.plot.region, text = text)
    # output
    # warning output
    # end warning output
    base::return(output)
    # end output
    # end main code
}
