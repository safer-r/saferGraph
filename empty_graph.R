######## empty_graph() #### text to display for empty graphs

#' @title empty_graph
#' @description
#' Display an empty plot with a text in the middle of the window (for instance to specify that no plot can be drawn).
#' @param text Character string of the message to display.
#' @param text.size Numeric value of the text size.
#' @param title Character string of the graph title.
#' @param title.size Numeric value of the title size (in points).
#' @returns An empty plot.
#' @details 
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#'
#' @examples
#' # simple example
#' 
#' fun_empty_graph(text = "NO GRAPH")
#' 
#' # white page
#' 
#' fun_empty_graph() # white page
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
    # end function name
    # required function checking
    if(length(utils::find("fun_check", mode = "function")) == 0L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end required function checking
    # argument checking
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
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument checking
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
    par(ini.par)
}