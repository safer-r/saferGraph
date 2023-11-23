#' @title empty_graph
#' @description
#' Display an empty plot with a text in the middle of the window (for instance to specify that no plot can be drawn).
#' @param text Single character string of the message to display.
#' @param text.size Single numeric value of the text size.
#' @param title Single character string of the graph title.
#' @param title.size Single numeric value of the title size (in points).
#' @returns An empty plot.
#' @details 
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#' @examples
#' # simple example
#' 
#' fun_empty_graph(text = "NO GRAPH")
#' 
#' 
#' # white page
#' 
#' fun_empty_graph() # white page
#' 
#' 
#' # all the arguments
#' 
#' fun_empty_graph(text = "NO GRAPH", text.size = 2, title = "GRAPH1", title.size = 1)
#' @export
fun_empty_graph <- function(
        text = NULL, 
        text.size = 1, 
        title = NULL, 
        title.size = 1.5
){
    # DEBUGGING
    # text = "NO GRAPH" ; title = "GRAPH1" ; text.size = 1
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # required function checking
    req.function <- c(
        "fun_check"
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
    # end arg with no default values
    
    # argument primary checking
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    if( ! is.null(text)){
        tempo <- fun_check(data = text, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    tempo <- fun_check(data = text.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(title)){
        tempo <- fun_check(data = title, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    tempo <- fun_check(data = title.size, class = "vector", mode = "numeric", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
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
        # "text", # inactivated because can be null
        "text.size",
        # "title", # inactivated because can be null
        "title.size"
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
    
    # package checking
    # end package checking
    
    # main code
    ini.par <- par(no.readonly = TRUE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
    par(ann=FALSE, xaxt="n", yaxt="n", mar = rep(1, 4), bty = "n", xpd = NA)
    plot(1, 1, type = "n") # no display with type = "n"
    x.left.dev.region <- (par("usr")[1] - ((par("usr")[2] - par("usr")[1]) / (par("plt")[2] - par("plt")[1])) * par("plt")[1] - ((par("usr")[2] - par("usr")[1]) / ((par("omd")[2] - par("omd")[1]) * (par("plt")[2] - par("plt")[1]))) * par("omd")[1])
    y.top.dev.region <- (par("usr")[4] + ((par("usr")[4] - par("usr")[3]) / (par("plt")[4] - par("plt")[3])) * (1 - par("plt")[4]) + ((par("usr")[4] - par("usr")[3]) / ((par("omd")[4] - par("omd")[3]) * (par("plt")[4] - par("plt")[3]))) * (1 - par("omd")[4]))
    if( ! is.null(text)){
        text(x = 1, y = 1, labels = text, cex = text.size)
    }
    if( ! is.null(title)){
        text(x = x.left.dev.region, y = y.top.dev.region, labels = title, adj=c(0, 1), cex = title.size)
    }
    # output
    par(ini.par)
    # end output
    # end main code
}