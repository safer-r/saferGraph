#' @title width
#' @description
#' Rescale the width of a window to open depending on the number of categories of qualitative variable to plot on the x-axis.
#' Can be used for height, considering that it is as if it was a width.
#' 
#' This order can be used:
#' 
#' width()
#' 
#' open()
#' 
#' prior_plot() # not for ggplot2
#' 
#' plot() or any other plotting
#' 
#' post_plot() if prior_plot() has been used # not for ggplot2
#' 
#' close()
#' @param categ.nb Single positive numeric value of number of categories to plot.
#' @param inches.per.categ.nb Single positive numeric value of inches per unit of categ.nb. 2 means 2 inches for each boxplot for instance.
#' @param ini.window.width Single positive numeric value indicating the initial Window width (in inches).
#' @param inch.left.space Single positive numeric value indicating the left horizontal margin of the figure region (in inches).
#' @param inch.right.space Single positive numeric value indicating the right horizontal margin of the figure region (in inches).
#' @param boundarie.space Single positive numeric value indicating the space between the right and left limits of the plotting region and the plot (0.5 means half a category width). It is as if empty categories were added on the right and left of the plot.
#' @returns The new window width in inches.
#' @details 
#' REQUIRED PACKAGES
#' 
#' cuteDev
#' 
#' 
#' REQUIRED FUNCTIONS FROM THE cute PACKAGE
#' 
#' arg_check()
#' @examples
#' width(categ.nb = 10, inches.per.categ.nb = 0.2, ini.window.width = 7, inch.left.space = 1, inch.right.space = 1, boundarie.space = 0.5)
#' @importFrom cuteDev arg_check
#' @export
width <- function(
        categ.nb, 
        inches.per.categ.nb = 1, 
        ini.window.width = 7, 
        inch.left.space, 
        inch.right.space, 
        boundarie.space = 0.5
){
    # DEBUGGING
    # categ.nb = 10 ; inches.per.categ.nb = 0.2 ; ini.window.width = 7 ; inch.left.space = 1 ; inch.right.space = 1 ; boundarie.space = 0.5 # for function debugging
    # package name
    package.name <- "cuteGraph"
    # end package name
    # function name
    ini <- match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # check of lib.path
    # end check of lib.path

    # check of the required function from the required packages
    .pack_and_function_check(
        fun = c(
            "cuteDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "categ.nb", 
        "inch.left.space", 
        "inch.right.space" 
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", package.name, " PACKAGE:\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with cuteDev::arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check = c(argum.check, tempo$problem) , text.check = c(text.check, tempo$text) , checked.arg.names = c(checked.arg.names, tempo$object.name))
    tempo <- cuteDev::arg_check(data = categ.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = inches.per.categ.nb, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = ini.window.width, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = inch.left.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = inch.right.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = boundarie.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with cuteDev::arg_check()
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
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", package.name, " PACKAGE:\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-c(
        "categ.nb", 
        "inches.per.categ.nb",
        "ini.window.width",
        "inch.left.space", 
        "inch.right.space", 
        "boundarie.space"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE", package.name, " PACKAGE:\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
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
    range.max <- categ.nb + boundarie.space # the max range of the future plot
    range.min <- boundarie.space # the min range of the future plot
    window.width <- inch.left.space + inch.right.space + inches.per.categ.nb * (range.max - range.min)
    # output
    # warning output
    # end warning output
    return(window.width)
    # end output
    # end main code
}
