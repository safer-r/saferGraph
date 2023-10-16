######## close() #### close specific graphic windows

#' @title width
#' @description
#' Close only specific graphic windows (devices).
#' @param kind Vector, among c("windows", "quartz", "x11", "X11", "pdf", "bmp", "png", "tiff"), indicating the kind of graphic windows (devices) to close. BEWARE: either "windows", "quartz", "x11" or "X11" means that all the X11 GUI graphics devices will be closed, whatever the OS used.
#' @param return.text Print text regarding the kind parameter and the devices that were finally closed?
#' @returns Text regarding the kind parameter and the devices that were finally closed.
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
#' windows() ; 
#' windows() ; 
#' pdf() ; 
#' dev.list() ; 
#' fun_close(kind = c("pdf", "x11"), return.text = TRUE) ; 
#' dev.list()
#' @export
fun_close <- function(kind = "pdf", return.text = FALSE){
    
    # DEBUGGING
    # kind = c("windows", "pdf") ; return.text = FALSE # for function debugging
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
    tempo <- fun_check(data = kind, options = c("windows", "quartz", "x11", "X11", "pdf", "bmp", "png", "tiff"), fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = return.text, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument checking
    # main code
    text <- paste0("THE REQUIRED KIND OF GRAPHIC DEVICES TO CLOSE ARE ", paste(kind, collapse = " "))
    if(Sys.info()["sysname"] == "Windows"){ # Note that .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
        if(any(kind %in% c("windows", "quartz", "x11", "X11"))){
            tempo <- kind %in% c("windows", "quartz", "x11", "X11")
            kind[tempo] <- "windows" # term are replaced by what is displayed when using a <- dev.list() ; names(a)
        }
    }else if(Sys.info()["sysname"] == "Linux"){
        if(any(kind %in% c("windows", "quartz", "x11", "X11"))){
            tempo.device <- suppressWarnings(try(X11(), silent = TRUE))[] # open a X11 window to try to recover the X11 system used
            if( ! is.null(tempo.device)){
                text <- paste0(text, "\nCANNOT CLOSE GUI GRAPHIC DEVICES AS REQUIRED BECAUSE THIS LINUX SYSTEM DOES NOT HAVE IT")
            }else{
                tempo <- kind %in% c("windows", "quartz", "x11", "X11")
                kind[tempo] <- names(dev.list()[length(dev.list())]) # term are replaced by what is displayed when using a <- dev.list() ; names(a)
                invisible(dev.off()) # close the X11 opened by tempo
            }
        }
    }else{ # for macOS
        if(any(kind %in% c("windows", "quartz", "x11", "X11"))){
            tempo <- kind %in% c("windows", "quartz", "x11", "X11")
            kind[tempo] <- "quartz" # term are replaced by what is displayed when using a <- dev.list() ; names(a)
        }
    }
    kind <- unique(kind)
    if(length(dev.list()) != 0){
        for(i in length(names(dev.list())):1){
            if(names(dev.list())[i] %in% kind){
                text <- paste0(text, "\n", names(dev.list())[i], " DEVICE NUMBER ", dev.list()[i], " HAS BEEN CLOSED")
                invisible(dev.off(dev.list()[i]))
            }
        }
    }
    if(return.text == TRUE){
        return(text)
    }
}

