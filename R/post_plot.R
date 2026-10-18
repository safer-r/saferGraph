#' @title post_plot
#' @description
#' Redesign axis. If x_side = 0, y_side = 0, the function just adds text at topright of the graph and reset par() for next graphics and provides outputs (see below).
#' 
#' Provide also positions for legend or additional text on the graph.
#' 
#' Use prior_plot() before this function for initial inactivation of the axis drawings.
#' @param x_side Single integer value indicating the axis at the bottom (1) or top (3) of the region figure. Write 0 for no change.
#' @param x_log_scale Single logical value. Log scale for the x-axis?
#' @param x_categ Vector of character indicating the categories when the x-axis is qualitative(stripchart, boxplot).
#' @param x_categ_pos Numeric vector of identical length than x_categ indicating the position of the categories names. If NULL, this will be 1:length(levels()).
#' @param x_lab Single character string of the label of the x-axis. If x_side == 0 and x_lab != "", then x_lab is printed.
#' @param x_axis_size Single positive numeric value to increase or decrease the size of the x axis numbers. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2. Also control the size of displayed categories.
#' @param x_label_size Single positive numeric value to increase or decrease the size of the x axis legend text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2.
#' @param x_dist_legend Single positive numeric value to move x-axis legends away (first number of mgp argument of par() but in inches).
#' @param x_nb_inter_tick Single positive integer value indicating the number of secondary ticks between main ticks on x-axis (only if not log scale). 0 means no secondary ticks.
#' @param y_side Single integer of either 0, 2 or 4 value to display the axis at the left (2) or right (4) of the region figure. Write 0 for no change.
#' @param y_log_scale Single logical value. Log scale for the y-axis?
#' @param y_categ Vector of character indicating the categories when the y-axis is qualitative(stripchart, boxplot).
#' @param y_categ_pos Numeric vector of identical length than y_categ indicating the position of the categories names. If NULL, this will be 1:length(levels()).
#' @param y_lab Label of the y-axis. If y_side == 0 and y_lab != "", then y_lab is printed.
#' @param y_axis_size Single positive numeric value to increase or decrease the size of the y axis numbers. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2. Also control the size of displayed categories.
#' @param y_label_size Single positive numeric value to increase or decrease the size of the y axis legend text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2.
#' @param y_dist_legend Single positive numeric value to move y-axis legends away (first number of mgp argument of par() but in inches).
#' @param y_nb_inter_tick Single positive integer value indicating the number of secondary ticks between main ticks on y-axis (only if not log scale). 0 means no secondary ticks.
#' @param text_angle Single numeric value for the angle of the text when axis is qualitative.
#' @param tick_length Single positive proportion value indicating the length of the main ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc., 0 for no ticks).
#' @param sec_tick_length Single positive proportion value indicating the length of the secondary ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc., 0 for no ticks).
#' @param bg_color Background color of the plot region. Either (1) NULL (no color), or (2) a single character string. Color can be a color name (see ?colors() in R), or an hexadecimal color code. BEWARE: cover/hide an existing plot !
#' @param grid_lwd Vector of positive numeric values. If non NULL, both activate the grid lines and specify the line widths.
#' @param grid_col Grid line colors (only if grid_lwd non NULL). A single character string. Color can be a color name (see ?colors() in R), or an hexadecimal color code.
#' @param corner_text Single character string adding a text at the top right corner of the window.
#' @param corner_text_size Single positive numeric value to increase or decrease the size of the text. Value 1 does not change it, 0.5 decreases by half, 2 increases by 2.
#' @param par_reset Single logical value that resets all the graphics parameters. BEWARE: TRUE can generate display problems, mainly in graphic devices with multiple figure regions.
#' @param just_label_add Single logical value that just add axis labels (legend). If TRUE, at least (x_side == 0 & x_lab != "") or (y_side == 0 & y_lab != "") must be set to display the corresponding x_lab or y_lab.
#' @param custom_par List that provides the parameters that reset all the graphics parameters. BEWARE: if NULL and par_reset == TRUE, the default par() parameters are used.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{c(lib_path, .libPaths())}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns 
#' A list containing: 
#' 
#' - $x_mid_left_dev_region: middle of the left margin of the device region, in coordinates of the x-axis.
#' 
#' - $x_left_dev_region: left side of the left margin (including the potential margin of the device region), in coordinates of the x-axis.
#' 
#' - $x_mid_right_dev_region: middle of the right margin of the device region, in coordinates of the x-axis.
#' 
#' - $x_right_dev_region: right side of the right margin (including the potential margin of the device region), in coordinates of the x-axis.
#' 
#' - $x_mid_left_fig_region: middle of the left margin of the figure region, in coordinates of the x-axis.
#' 
#' - $x_left_fig_region: left side of the left margin, in coordinates of the x-axis.
#' 
#' - $x_mid_right_fig_region: middle of the right margin of the figure region, in coordinates of the x-axis.
#' 
#' - $x_right_fig_region: right side of the right margin, in coordinates of the x-axis.
#' 
#' - $x_left_plot_region: left side of the plot region, in coordinates of the x-axis.
#' 
#' - $x_right_plot_region: right side of the plot region, in coordinates of the x-axis.
#' 
#' - $x_mid_plot_region: middle of the plot region, in coordinates of the x-axis.
#' 
#' - $y_mid_bottom_dev_region: middle of the bottom margin of the device region, in coordinates of the y-axis.
#' 
#' - $y_bottom_dev_region: bottom side of the bottom margin (including the potential margin of the device region), in coordinates of the y-axis.
#' 
#' - $y_mid_top_dev_region: middle of the top margin of the device region, in coordinates of the y-axis.
#' 
#' - $y_top_dev_region: top side of the top margin (including the potential margin of the device region), in coordinates of the y-axis.
#' 
#' - $y_mid_bottom_fig_region: middle of the bottom margin of the figure region, in coordinates of the y-axis.
#' 
#' - $y_bottom_fig_region: bottom of the bottom margin of the figure region, in coordinates of the y-axis.
#' 
#' - $y_mid_top_fig_region: middle of the top margin of the figure region, in coordinates of the y-axis.
#' 
#' - $y_top_fig_region: top of the top margin of the figure region, in coordinates of the y-axis.
#' 
#' - $y_top_plot_region: top of the plot region, in coordinates of the y-axis.
#' 
#' - $y_bottom_plot_region: bottom of the plot region, in coordinates of the y-axis.
#' 
#' - $y_mid_plot_region: middle of the plot region, in coordinates of the y-axis.
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
#' post_plot(x_side = 1, x_lab = "Values", y_side = 2, y_lab = "TEST", y_axis_size = 1, y_label_size = 2, y_dist_legend = 0.6)
#' }
#' @importFrom saferDev arg_check
#' @export
post_plot <- function(
        x_side = 0, 
        x_log_scale = FALSE, 
        x_categ = NULL, 
        x_categ_pos = NULL, 
        x_lab = "", 
        x_axis_size = 1.5, 
        x_label_size = 1.5, 
        x_dist_legend = 0.5, 
        x_nb_inter_tick = 1, 
        y_side = 0, 
        y_log_scale = FALSE, 
        y_categ = NULL, 
        y_categ_pos = NULL, 
        y_lab = "", 
        y_axis_size = 1.5, 
        y_label_size = 1.5, 
        y_dist_legend = 0.5, 
        y_nb_inter_tick = 1, 
        text_angle = 90, 
        tick_length = 0.5, 
        sec_tick_length = 0.3, 
        bg_color = NULL, 
        grid_lwd = NULL, 
        grid_col = "white", 
        corner_text = "", 
        corner_text_size = 1, 
        just_label_add = FALSE, 
        par_reset = FALSE, 
        custom_par = NULL,
        safer_check = TRUE, 
        lib_path = NULL, 
        error_text = "" 

){
    # DEBUGGING
    # x_side = 0 ; x_log_scale = FALSE ; x_categ = NULL ; x_categ_pos = NULL ; x_lab = "" ; x_axis_size = 1.5 ; x_label_size = 1.5 ; x_dist_legend = 1 ; x_nb_inter_tick = 1 ; y_side = 0 ; y_log_scale = FALSE ; y_categ = NULL ; y_categ_pos = NULL ; y_lab = "" ; y_axis_size = 1.5 ; y_label_size = 1.5 ; y_dist_legend = 0.7 ; y_nb_inter_tick = 1 ; text_angle = 90 ; tick_length = 0.5 ; sec_tick_length = 0.3 ; bg_color = NULL ; grid_lwd = NULL ; grid_col = "white" ; corner_text = "" ; corner_text_size = 1 ; just_label_add = FALSE ; par_reset = FALSE ; custom_par = NULL ; safer_check = TRUE# for function debugging



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
        "x_side", 
        "x_log_scale", 
        # "x_categ", # inactivated because can be null
        # "x_categ_pos", # inactivated because can be null
        "x_lab", 
        "x_axis_size", 
        "x_label_size", 
        "x_dist_legend", 
        "x_nb_inter_tick", 
        "y_side", 
        "y_log_scale", 
        # "y_categ", # inactivated because can be null
        # "y_categ_pos", # inactivated because can be null
        "y_lab", 
        "y_axis_size", 
        "y_label_size", 
        "y_dist_legend", 
        "y_nb_inter_tick" , 
        "text_angle", 
        "tick_length", 
        "sec_tick_length", 
        # "bg_color", # inactivated because can be null
        # "grid_lwd", # inactivated because can be null
        "grid_col", 
        "corner_text", 
        "corner_text_size", 
        "just_label_add", 
        "par_reset",
        # "custom_par", # inactivated because can be null
        "safer_check"
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.
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
        "x_side", 
        "x_log_scale", 
        "x_categ",  
        "x_categ_pos",  
        # "x_lab", 
        "x_axis_size", 
        "x_label_size", 
        "x_dist_legend", 
        "x_nb_inter_tick", 
        "y_side", 
        "y_log_scale", 
        "y_categ",  
        "y_categ_pos",  
        # "y_lab", 
        "y_axis_size", 
        "y_label_size", 
        "y_dist_legend", 
        "y_nb_inter_tick" , 
        "text_angle", 
        "tick_length", 
        "sec_tick_length", 
        "bg_color",  
        "grid_lwd",  
        "grid_col", 
        # "corner_text", 
        "corner_text_size", 
        "just_label_add", 
        "par_reset",
        "custom_par",  
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
    # Mandatory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
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
    tempo <- saferDev::arg_check(data = x_side, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c(0, 1, 3), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = x_log_scale, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x_categ)){
        tempo <- saferDev::arg_check(data = x_categ, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x_categ_pos)){
        tempo <- saferDev::arg_check(data = x_categ_pos, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = x_lab, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = x_axis_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = x_label_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = x_dist_legend, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = x_nb_inter_tick, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_side, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c(0, 2, 4), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_log_scale, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(y_categ)){
        tempo <- saferDev::arg_check(data = y_categ, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(y_categ_pos)){
        tempo <- saferDev::arg_check(data = y_categ_pos, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = y_lab, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_axis_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_label_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_dist_legend, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = y_nb_inter_tick, class = "integer", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = text_angle, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = tick_length, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = sec_tick_length, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(bg_color)){
        tempo <- saferDev::arg_check(data = bg_color, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if( ! (bg_color %in% grDevices::colors() | base::grepl(pattern = "^#", bg_color))){ # check color
            tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nbg_color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY grDevices::colors()")
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }
    if( ! base::is.null(grid_lwd)){
        tempo <- saferDev::arg_check(data = grid_lwd, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(grid_col)){
        tempo <- saferDev::arg_check(data = grid_col, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        if( ! (grid_col %in% grDevices::colors() | base::grepl(pattern = "^#", grid_col))){ # check color
            tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\ngrid_col ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY grDevices::colors()")
            text_check <- base::c(text_check, tempo_cat)
            argum_check <- base::c(argum_check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = corner_text, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = corner_text_size, class = NULL, typeof = NULL, mode = "numeric", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = just_label_add, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = par_reset, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(custom_par)){
        tempo <- saferDev::arg_check(data = custom_par, class = NULL, typeof = "list", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        "x_categ",   
        # "x_lab", 
        "y_categ",   
        # "y_lab", 
        "bg_color",  
        "grid_col" 
        # "corner_text", 
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            "\nIf saferMake::saferMake() HAS BEEN USED TO MODIFY YOUR FUNCTION, PLEASE RESTART WITH YOUR MAIN CODE AND SELECT PROPER CLASS, TYPE AND/OR MODE FOR YOUR ARGUMENTS\n.HERE, ARGUMENTS ARE TYPE:\n", 
            base::paste0(base::sapply(X = tempo_arg, FUN = function(x){base::paste0(x, ": \"", base::typeof(x = base::get(x = x, pos = -1L, envir = base::parent.frame(n = 3), mode = "any", inherits = FALSE)), "\"", collapse = NULL, recycle0 = FALSE)}), collapse = "\n", recycle0 = FALSE), # parent.frame(n = 3) because the get() is evaluated inside the FUN closure of sapply. But sapply(FUN) calls lapply(FUN) internally !! So parent.frame(n = 1) is the internal lapply() frame called by sapply, parent.frame(n = 2) is sapply's frame, parent.frame(n = 3) is BACKBONE's frame. NB: paste0() and typeof() are primitives and add no frame.
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame. # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
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
    graphics::par(tcl = -graphics::par()$mgp[2] * tick_length)
    if(x_log_scale == TRUE){
        grid_coord_x <- base::c(10^graphics::par("usr")[1], 10^graphics::par("usr")[2])
    }else{
        grid_coord_x <- base::c(graphics::par("usr")[1], graphics::par("usr")[2])
    }
    if(y_log_scale == TRUE){
        grid_coord_y <- base::c(10^graphics::par("usr")[3], 10^graphics::par("usr")[4])
    }else{
        grid_coord_y <- base::c(graphics::par("usr")[3], graphics::par("usr")[4])
    }
    if( ! base::is.null(bg_color)){
        graphics::rect(grid_coord_x[1], grid_coord_y[1], grid_coord_x[2], grid_coord_y[2], col = bg_color, border = NA)
    }
    if( ! base::is.null(grid_lwd)){
        graphics::grid(nx = NA, ny = NULL, col = grid_col, lty = 1, lwd = grid_lwd)
    }
    if(x_log_scale == TRUE){
        x_mid_left_dev_region <- 10^(graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * graphics::par("omd")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
        x_left_dev_region <- 10^(graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * graphics::par("omd")[1]) # in x coordinates
        x_mid_right_dev_region <- 10^(graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * (1 - graphics::par("omd")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
        x_right_dev_region <- 10^(graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * (1 - graphics::par("omd")[2])) # in x coordinates
        x_mid_left_fig_region <- 10^(graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
        x_left_fig_region <- 10^(graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1]) # in x coordinates
        x_mid_right_fig_region <- 10^(graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
        x_right_fig_region <- 10^(graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2])) # in x coordinates
        x_left_plot_region <- 10^graphics::par("usr")[1] # in x coordinates, left of the plot region (according to x scale)
        x_right_plot_region <- 10^graphics::par("usr")[2] # in x coordinates, right of the plot region (according to x scale)
        x_mid_plot_region <- 10^((graphics::par("usr")[2] + graphics::par("usr")[1]) / 2) # in x coordinates, right of the plot region (according to x scale)
    }else{
        x_mid_left_dev_region <- (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * graphics::par("omd")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
        x_left_dev_region <- (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * graphics::par("omd")[1]) # in x coordinates
        x_mid_right_dev_region <- (graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * (1 - graphics::par("omd")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
        x_right_dev_region <- (graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / ((graphics::par("omd")[2] - graphics::par("omd")[1]) * (graphics::par("plt")[2] - graphics::par("plt")[1]))) * (1 - graphics::par("omd")[2])) # in x coordinates
        x_mid_left_fig_region <- (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1] / 2) # in x coordinates, to position axis labeling at the bottom of the graph (according to x scale)
        x_left_fig_region <- (graphics::par("usr")[1] - ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * graphics::par("plt")[1]) # in x coordinates
        x_mid_right_fig_region <- (graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2]) / 2) # in x coordinates, to position axis labeling at the top of the graph (according to x scale)
        x_right_fig_region <- (graphics::par("usr")[2] + ((graphics::par("usr")[2] - graphics::par("usr")[1]) / (graphics::par("plt")[2] - graphics::par("plt")[1])) * (1 - graphics::par("plt")[2])) # in x coordinates
        x_left_plot_region <- graphics::par("usr")[1] # in x coordinates, left of the plot region (according to x scale)
        x_right_plot_region <- graphics::par("usr")[2] # in x coordinates, right of the plot region (according to x scale)
        x_mid_plot_region <- (graphics::par("usr")[2] + graphics::par("usr")[1]) / 2 # in x coordinates, right of the plot region (according to x scale)
    }
    if(y_log_scale == TRUE){
        y_mid_bottom_dev_region <- 10^(graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (graphics::par("omd")[3] / 2)) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
        y_bottom_dev_region <- 10^(graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * graphics::par("omd")[3]) # in y coordinates
        y_mid_top_dev_region <- 10^(graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (1 - graphics::par("omd")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
        y_top_dev_region <- 10^(graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (1 - graphics::par("omd")[4])) # in y coordinates
        y_mid_bottom_fig_region <- 10^(graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] / 2) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
        y_bottom_fig_region <- 10^(graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3]) # in y coordinates
        y_mid_top_fig_region <- 10^(graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
        y_top_fig_region <- 10^(graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4])) # in y coordinates
        y_top_plot_region <- 10^graphics::par("usr")[4] # in y coordinates, top of the plot region (according to y scale)
        y_bottom_plot_region <- 10^graphics::par("usr")[3] # in y coordinates, bottom of the plot region (according to y scale)
        y_mid_plot_region <- (graphics::par("usr")[3] + graphics::par("usr")[4]) / 2 # in x coordinates, right of the plot region (according to x scale)
    }else{
        y_mid_bottom_dev_region <- (graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (graphics::par("omd")[3] / 2)) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
        y_bottom_dev_region <- (graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * graphics::par("omd")[3]) # in y coordinates
        y_mid_top_dev_region <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (1 - graphics::par("omd")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
        y_top_dev_region <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / ((graphics::par("omd")[4] - graphics::par("omd")[3]) * (graphics::par("plt")[4] - graphics::par("plt")[3]))) * (1 - graphics::par("omd")[4])) # in y coordinates
        y_mid_bottom_fig_region <- (graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3] / 2) # in y coordinates, to position axis labeling at the bottom of the graph (according to y scale). Ex mid.bottom.space
        y_bottom_fig_region <- (graphics::par("usr")[3] - ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * graphics::par("plt")[3]) # in y coordinates
        y_mid_top_fig_region <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4]) / 2) # in y coordinates, to position axis labeling at the top of the graph (according to y scale). Ex mid.top.space
        y_top_fig_region <- (graphics::par("usr")[4] + ((graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3])) * (1 - graphics::par("plt")[4])) # in y coordinates
        y_top_plot_region <- graphics::par("usr")[4] # in y coordinates, top of the plot region (according to y scale)
        y_bottom_plot_region <- graphics::par("usr")[3] # in y coordinates, bottom of the plot region (according to y scale)
        y_mid_plot_region <- ((graphics::par("usr")[3] + graphics::par("usr")[4]) / 2) # in x coordinates, right of the plot region (according to x scale)
    }
    if(base::any(base::sapply(FUN = all.equal, base::c(1, 3), x_side) == TRUE, na.rm = TRUE)){
        graphics::par(xpd=FALSE, xaxt="s")
        if(base::is.null(x_categ) & x_log_scale == TRUE){
            if(base::any(graphics::par()$xaxp[1:2] == 0L, na.rm = TRUE)){ # any(sapply(FUN = all.equal, graphics::par()$xaxp[1:2], 0) == TRUE) not used because we strictly need zero as a result. Beware: write "== TRUE", because the result is otherwise character and a warning message appears using any()
                if(graphics::par()$xaxp[1] == 0L){ # isTRUE(all.equal(graphics::par()$xaxp[1], 0)) not used because we strictly need zero as a result
                    graphics::par(xaxp = base::c(10^-30, graphics::par()$xaxp[2:3])) # because log10(graphics::par()$xaxp[1] == 0) == -Inf
                }
                if(graphics::par()$xaxp[2] == 0L){ # isTRUE(all.equal(graphics::par()$xaxp[1], 0)) not used because we strictly need zero as a result # nocov start (unreachable: the plot.window() at the top of the function resets the par and no valid x_log_scale == TRUE plot can have xaxp[2] == 0, because xaxp[2] == 0 on a log axis means 10^0 == 1, which is always in the interior of the axis when xaxp[1] is already <= 0) ; kept for safety
                    graphics::par(xaxp = base::c(graphics::par()$xaxp[1], 10^-30, graphics::par()$xaxp[3])) # because log10(graphics::par()$xaxp[2] == 0) == -Inf
                } # nocov end
            }
            graphics::axis(side = x_side, at = base::c(10^graphics::par()$usr[1], 10^graphics::par()$usr[2]), labels=base::rep("", 2), lwd=1, lwd.ticks = 0) # draw the axis line
            graphics::mtext(side = x_side, text = x_lab, line = x_dist_legend / 0.2, las = 0, cex = x_label_size)
            graphics::par(tcl = -graphics::par()$mgp[2] * sec_tick_length) # length of the secondary ticks are reduced
            base::suppressWarnings(graphics::rug(10^base::outer(base::c((base::log10(graphics::par("xaxp")[1]) -1):base::log10(graphics::par("xaxp")[2])), base::log10(1:10), "+"), ticksize = NA, side = x_side)) # ticksize = NA to allow the use of graphics::par()$tcl value
            graphics::par(tcl = -graphics::par()$mgp[2] * tick_length) # back to main ticks
            graphics::axis(side = x_side, at = base::c(1e-15, 1e-14, 1e-13, 1e-12, 1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10), labels = base::expression(10^-15, 10^-14, 10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10), lwd = 0, lwd.ticks = 1, cex.axis = x_axis_size)
            x_text <- 10^graphics::par("usr")[2]
        }else if(base::is.null(x_categ) & x_log_scale == FALSE){
            graphics::axis(side=x_side, at=base::c(graphics::par()$usr[1], graphics::par()$usr[2]), labels=base::rep("", 2), lwd=1, lwd.ticks=0) # draw the axis line
            graphics::axis(side=x_side, at=base::round(base::seq(graphics::par()$xaxp[1], graphics::par()$xaxp[2], length.out=graphics::par()$xaxp[3]+1), 2), cex.axis = x_axis_size) # axis(side=x_side, at=round(seq(graphics::par()$xaxp[1], graphics::par()$xaxp[2], length.out=graphics::par()$xaxp[3]+1), 2), labels = format(round(seq(graphics::par()$xaxp[1], graphics::par()$xaxp[2], length.out=graphics::par()$xaxp[3]+1), 2), big.mark=','), cex.axis = x_axis_size) # to get the 1000 comma separator
            graphics::mtext(side = x_side, text = x_lab, line = x_dist_legend / 0.2, las = 0, cex = x_label_size)
            if(x_nb_inter_tick > 0){
                inter_tick_unit <- (graphics::par("xaxp")[2] - graphics::par("xaxp")[1]) / graphics::par("xaxp")[3]
                graphics::par(tcl = -graphics::par()$mgp[2] * sec_tick_length) # length of the ticks are reduced
                base::suppressWarnings(graphics::rug(base::seq(graphics::par("xaxp")[1] - 10 * inter_tick_unit, graphics::par("xaxp")[2] + 10 * inter_tick_unit, by = inter_tick_unit / (1 + x_nb_inter_tick)), ticksize = NA, x_side)) # ticksize = NA to allow the use of graphics::par()$tcl value
                graphics::par(tcl = -graphics::par()$mgp[2] * tick_length) # back to main ticks
            }
            x_text <- graphics::par("usr")[2]
        }else if(( ! base::is.null(x_categ)) & x_log_scale == FALSE){
            if(base::is.null(x_categ_pos)){
                x_categ_pos <- 1:base::length(x_categ)
            }else if(base::length(x_categ_pos) != base::length(x_categ)){
                tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nx_categ_pos MUST BE THE SAME LENGTH AS x_categ")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            graphics::par(xpd = TRUE)
            if(base::isTRUE(base::all.equal(x_side, 1))){ #isTRUE(all.equal(x_side, 1)) is similar to x_side == 1L but deals with float
                graphics::segments(x0 = x_left_plot_region, x1 = x_right_plot_region, y0 = y_bottom_plot_region, y1 = y_bottom_plot_region) # draw the line of the axis
                graphics::text(x = x_categ_pos, y = y_mid_bottom_fig_region, labels = x_categ, srt = text_angle, cex = x_axis_size)
            }else if(base::isTRUE(base::all.equal(x_side, 3))){ #isTRUE(all.equal(x_side, 1)) is similar to x_side == 3L but deals with float
                graphics::segments(x0 = x_left_plot_region, x1 = x_right_plot_region, y0 = y_top_plot_region, y1 = y_top_plot_region) # draw the line of the axis
                graphics::text(x = x_categ_pos, y = y_mid_top_fig_region, labels = x_categ, srt = text_angle, cex = x_axis_size)
            }else{ # nocov start (unreachable: arg_check() above restricts x_side to 0, 1 and 3 when x_categ is not NULL)
                tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT x_side CAN ONLY BE 1 OR 3")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            } # nocov end
            graphics::par(xpd = FALSE)
            x_text <- graphics::par("usr")[2]
        }else{ # nocov start (unreachable: the three branches above cover all combinations of x_categ NULL/non-NULL and x_log_scale TRUE/FALSE allowed by arg_check())
            tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nPROBLEM WITH THE x_side (", x_side ,") OR x_log_scale (", x_log_scale,") ARGUMENTS")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        } # nocov end
    }else{
        x_text <- graphics::par("usr")[2]
    }
    if(base::any(base::sapply(FUN = all.equal, base::c(2, 4), y_side) == TRUE, na.rm = TRUE)){
        graphics::par(xpd=FALSE, yaxt="s")
        if(base::is.null(y_categ) & y_log_scale == TRUE){
            if(base::any(graphics::par()$yaxp[1:2] == 0L, na.rm = TRUE)){ # any(sapply(FUN = all.equal, graphics::par()$yaxp[1:2], 0) == TRUE) not used because we strictly need zero as a result. Beware: write "== TRUE", because the result is otherwise character and a warning message appears using any()
                if(graphics::par()$yaxp[1] == 0L){ # strict zero needed
                    graphics::par(yaxp = base::c(10^-30, graphics::par()$yaxp[2:3])) # because log10(graphics::par()$yaxp[1] == 0) == -Inf
                }
                if(graphics::par()$yaxp[2] == 0L){ # strict zero needed # nocov start (unreachable: same reason as for xaxp[2], on a log y axis) ; kept for safety
                    graphics::par(yaxp = base::c(graphics::par()$yaxp[1], 10^-30, graphics::par()$yaxp[3])) # because log10(graphics::par()$yaxp[2] == 0) == -Inf
                } # nocov end
            }
            graphics::axis(side=y_side, at=base::c(10^graphics::par()$usr[3], 10^graphics::par()$usr[4]), labels=base::rep("", 2), lwd=1, lwd.ticks=0) # draw the axis line
            graphics::par(tcl = -graphics::par()$mgp[2] * sec_tick_length) # length of the ticks are reduced
            base::suppressWarnings(graphics::rug(10^base::outer(base::c((base::log10(graphics::par("yaxp")[1])-1):base::log10(graphics::par("yaxp")[2])), base::log10(1:10), "+"), ticksize = NA, side = y_side)) # ticksize = NA to allow the use of graphics::par()$tcl value
            graphics::par(tcl = -graphics::par()$mgp[2] * tick_length) # back to main tick length
            graphics::axis(side = y_side, at = base::c(1e-15, 1e-14, 1e-13, 1e-12, 1e-11, 1e-10, 1e-9, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10), labels = base::expression(10^-15, 10^-14, 10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7, 10^8, 10^9, 10^10), lwd = 0, lwd.ticks = 1, cex.axis = y_axis_size)
            y_text <- 10^(graphics::par("usr")[4] + (graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3]) * (1 - graphics::par("plt")[4]))
            graphics::mtext(side = y_side, text = y_lab, line = y_dist_legend / 0.2, las = 0, cex = y_label_size)
        }else if(base::is.null(y_categ) & y_log_scale == FALSE){
            graphics::axis(side=y_side, at=base::c(graphics::par()$usr[3], graphics::par()$usr[4]), labels=base::rep("", 2), lwd=1, lwd.ticks=0) # draw the axis line
            graphics::axis(side=y_side, at=base::round(base::seq(graphics::par()$yaxp[1], graphics::par()$yaxp[2], length.out=graphics::par()$yaxp[3]+1), 2), cex.axis = y_axis_size)
            graphics::mtext(side = y_side, text = y_lab, line = y_dist_legend / 0.2, las = 0, cex = y_label_size)
            if(y_nb_inter_tick > 0){
                inter_tick_unit <- (graphics::par("yaxp")[2] - graphics::par("yaxp")[1]) / graphics::par("yaxp")[3]
                graphics::par(tcl = -graphics::par()$mgp[2] * sec_tick_length) # length of the ticks are reduced
                base::suppressWarnings(graphics::rug(base::seq(graphics::par("yaxp")[1] - 10 * inter_tick_unit, graphics::par("yaxp")[2] + 10 * inter_tick_unit, by = inter_tick_unit / (1 + y_nb_inter_tick)), ticksize = NA, side=y_side)) # ticksize = NA to allow the use of graphics::par()$tcl value
                graphics::par(tcl = -graphics::par()$mgp[2] * tick_length) # back to main tick length
            }
            y_text <- (graphics::par("usr")[4] + (graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3]) * (1 - graphics::par("plt")[4]))
        }else if(( ! base::is.null(y_categ)) & y_log_scale == FALSE){
            if(base::is.null(y_categ_pos)){
                y_categ_pos <- 1:base::length(y_categ)
            }else if(base::length(y_categ_pos) != base::length(y_categ)){
                tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\ny_categ_pos MUST BE THE SAME LENGTH AS y_categ")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            graphics::axis(side = y_side, at = y_categ_pos, labels = base::rep("", base::length(y_categ)), lwd=0, lwd.ticks=1) # draw the line of the axis
            graphics::par(xpd = TRUE)
            if(base::isTRUE(base::all.equal(y_side, 2))){ #isTRUE(all.equal(y_side, 2)) is similar to y_side == 2L but deals with float
                graphics::text(x = x_mid_left_fig_region, y = y_categ_pos, labels = y_categ, srt = text_angle, cex = y_axis_size)
            }else if(base::isTRUE(base::all.equal(y_side, 4))){ # idem
                graphics::text(x = x_mid_right_fig_region, y = y_categ_pos, labels = y_categ, srt = text_angle, cex = y_axis_size)
            }else{ # nocov start (unreachable: arg_check() above restricts y_side to 0, 2 and 4 when y_categ is not NULL)
                tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT y_side CAN ONLY BE 2 OR 4")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            } # nocov end
            graphics::par(xpd = FALSE)
            y_text <- (graphics::par("usr")[4] + (graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3]) * (1 - graphics::par("plt")[4]))
        }else{ # nocov start (unreachable: the three branches above cover all combinations of y_categ NULL/non-NULL and y_log_scale TRUE/FALSE allowed by arg_check())
            tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nPROBLEM WITH THE y_side (", y_side ,") OR y_log_scale (", y_log_scale,") ARGUMENTS")
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        } # nocov end
    }else{
        y_text <- (graphics::par("usr")[4] + (graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3]) * (1 - graphics::par("plt")[4]))
    }
    graphics::par(xpd=NA)
    graphics::text(x = x_mid_right_fig_region, y = y_text, corner_text, adj=base::c(1, 1.1), cex = corner_text_size) # text at the topright corner. Replace x_right_fig_region by x_text if text at the right edge of the plot region
    if(just_label_add == TRUE & base::isTRUE(base::all.equal(x_side, 0)) & x_lab != ""){
        graphics::text(x = x_mid_plot_region, y = y_mid_bottom_fig_region, x_lab, adj=base::c(0.5, 0.5), cex = x_label_size) # x label
    }
    if(just_label_add == TRUE & base::isTRUE(base::all.equal(y_side, 0)) & y_lab != ""){
        graphics::text(x = y_mid_plot_region, y = x_mid_left_fig_region, y_lab, adj=base::c(0.5, 0.5), cex = y_label_size) # x label
    }
    graphics::par(xpd=FALSE)
    if(par_reset == TRUE){
        tempo_par <- saferGraph::open2(pdf = FALSE, pdf_path = "working.dir", pdf_name = "graph", width = 7, height = 7, paper = "special", pdf_overwrite = FALSE, rescale = "R", remove_read_only = FALSE, return_output = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        base::invisible(grDevices::dev.off()) # close the new window
        if( ! base::is.null(custom_par)){
            if( ! base::names(x = custom_par) %in% base::names(x = tempo_par$ini_par)){
                tempo_cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\ncustom_par ARGUMENT SHOULD HAVE THE NAMES OF THE COMPARTMENT LIST COMING FROM THE graphics::par() LIST")
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            base::suppressWarnings(expr = graphics::par(custom_par), classes = "warning") # apply the custom parameters on the current device
            text <- base::c(text, "\nGRAPH PARAMETERS SET TO VALUES DEFINED BY custom_par ARGUMENT\n")
        }else{
            base::suppressWarnings(expr = graphics::par(tempo_par$ini_par), classes = "warning") # reset to the default parameters of a fresh device
            text <- base::c(text, "\nGRAPH PARAMETERS RESET TO graphics::par() DEFAULT VALUES\n")
        }
    }
    output <- base::list(x_mid_left_dev_region = x_mid_left_dev_region, x_left_dev_region = x_left_dev_region, x_mid_right_dev_region = x_mid_right_dev_region, x_right_dev_region = x_right_dev_region, x_mid_left_fig_region = x_mid_left_fig_region, x_left_fig_region = x_left_fig_region, x_mid_right_fig_region = x_mid_right_fig_region, x_right_fig_region = x_right_fig_region, x_left_plot_region = x_left_plot_region, x_right_plot_region = x_right_plot_region, x_mid_plot_region = x_mid_plot_region, y_mid_bottom_dev_region = y_mid_bottom_dev_region, y_bottom_dev_region = y_bottom_dev_region, y_mid_top_dev_region = y_mid_top_dev_region, y_top_dev_region = y_top_dev_region, y_mid_bottom_fig_region = y_mid_bottom_fig_region, y_bottom_fig_region = y_bottom_fig_region, y_mid_top_fig_region = y_mid_top_fig_region, y_top_fig_region = y_top_fig_region, y_top_plot_region = y_top_plot_region, y_bottom_plot_region = y_bottom_plot_region, y_mid_plot_region = y_mid_plot_region, text = text)
    # output
    # warning output
    # end warning output
    base::return(output)
    # end output
    # end main code
}
