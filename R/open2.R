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
#' @param pdf.path Single character string. Where the pdf is saved (do not terminate by / or \\). Write "working.dir" if working directory is required (default). Ignored if pdf == FALSE.
#' @param pdf.name Single character string. Name of the pdf file containing the graphs (the .pdf extension is added by the function, if not detected in the name end). Ignored if pdf == FALSE.
#' @param width Single positive numeric value indicating the width of the window (in inches).
#' @param height Single positive numeric value indicating the height of the window (in inches).
#' @param paper Single character string. Paper argument of the pdf function (paper format). Only used for pdf(). Either "a4", "letter", "legal", "us", "executive", "a4r", "USr" or "special". If "special", means that the paper dimension will be width and height. With another paper format, if width or height is over the size of the paper, width or height will be modified such that the plot is adjusted to the paper dimension (see $dim in the returned list below to see the modified dimensions). Ignored if pdf == FALSE.
#' @param pdf.overwrite Single logical value. Existing pdf can be overwritten? . Ignored if pdf == FALSE.
#' @param rescale Kind of GUI. Either "R", "fit", or "fixed". Ignored on Mac and Linux OS. See ?windows for details.
#' @param remove.read.only Single logical value. Remove the read only (R.O.) graphical parameters? If TRUE, the graphical parameters are returned without the R.O. parameters. The returned $ini.par list can be used to set the par() of a new graphical device. If FALSE, graphical parameters are returned with the R.O. parameters, which provides information like text dimension (see ?par() ). The returned $ini.par list can be used to set the par() of a new graphical device, but generate a warning message. Ignored if return.output == FALSE. 
#' @param return.output Single logical value. Return output ? If TRUE the output list is displayed.
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
#' @examples
#' \dontrun{
#' # Screen devices should not be used in examples
#' open2(pdf = FALSE, pdf.path = ".", pdf.name = "graph", width = 7, height = 7, paper = "special", pdf.overwrite = FALSE, return.output = TRUE)
#' }
#' @importFrom saferDev arg_check
#' @importFrom grDevices dev.off
#' @importFrom grDevices dev.list
#' @importFrom grDevices dev.size
#' @export
open2 <- function(
        pdf = TRUE, 
        pdf.path = "working.dir", 
        pdf.name = "graph", 
        width = 7, 
        height = 7, 
        paper = "special", 
        pdf.overwrite = FALSE, 
        rescale = "fixed", 
        remove.read.only = TRUE, 
        return.output = FALSE
){
    # DEBUGGING
    # pdf = TRUE ; pdf.path = "C:/Users/Gael/Desktop" ; pdf.name = "graphs" ; width = 7 ; height = 7 ; paper = "special" ; pdf.overwrite = FALSE ; rescale = "fixed" ; remove.read.only = TRUE ; return.output = TRUE # for function debugging
    # package name
    package.name <- "saferGraph"
    # end package name
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # package checking
    # check of lib.path
    # end check of lib.path
    .pack_and_function_check(
        fun = c(
            "saferDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check <- c(argum.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = pdf, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = pdf.path, class = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = pdf.name, class = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = width, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = height, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = paper, options = c("a4", "letter", "legal", "us", "executive", "a4r", "USr", "special", "A4", "LETTER", "LEGAL", "US"), length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data =pdf.overwrite, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = rescale, options = c("R", "fit", "fixed"), length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = remove.read.only, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = return.output, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with saferDev::arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-c(
        "pdf", 
        "pdf.path", 
        "pdf.name", 
        "width", 
        "height", 
        "paper", 
        "pdf.overwrite", 
        "rescale", 
        "remove.read.only", 
        "return.output"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
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
    if(pdf.path == "working.dir"){
        pdf.path <- getwd()
    }else{
        if(grepl(x = pdf.path, pattern = ".+/$")){
            pdf.path <- sub(x = pdf.path, pattern = "/$", replacement = "") # remove the last /
        }else if(grepl(x = pdf.path, pattern = ".+[\\]$")){ # or ".+\\\\$" # cannot be ".+\$" because \$ does not exist contrary to \n
            pdf.path <- sub(x = pdf.path, pattern = "[\\]$", replacement = "") # remove the last /
        }
        if(dir.exists(pdf.path) == FALSE){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\npdf.path ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY\n", pdf.path)
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # par.ini recovery
    # cannot use pdf(file = NULL), because some small differences between pdf() and other devices. For instance, differences with windows() for par()$fin, par()$pin and par()$plt
    if(Sys.info()["sysname"] == "Windows"){ # Note that .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
        open.fail <- NULL
        grDevices::windows()
        ini.par <- par(no.readonly = remove.read.only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
        invisible(grDevices::dev.off()) # close the new window
    }else if(Sys.info()["sysname"] == "Linux"){
        if(pdf == TRUE){# cannot use pdf(file = NULL), because some small differences between pdf() and other devices. For instance, differences with windows() for par()$fin, par()$pin and par()$plt
            if(exists(".Random.seed", envir = .GlobalEnv)){ # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment
                tempo.random.seed <- .Random.seed
                on.exit(assign(".Random.seed", tempo.random.seed, envir = .GlobalEnv))
            }else{
                on.exit(set.seed(NULL)) # inactivate seeding -> return to complete randomness
            }
            set.seed(NULL)
            tempo.code <- sample(x = 1:1e7, size = 1)
            while(file.exists(paste0(pdf.path, "/recover_ini_par", tempo.code, ".pdf")) == TRUE){
                tempo.code <- tempo.code + 1
            }
            grDevices::pdf(width = width, height = height, file=paste0(pdf.path, "/recover_ini_par", tempo.code, ".pdf"), paper = paper)
            ini.par <- par(no.readonly = remove.read.only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
            invisible(grDevices::dev.off()) # close the pdf window
            file.remove(paste0(pdf.path, "/recover_ini_par", tempo.code, ".pdf")) # remove the pdf file
        }else{
            # test if X11 can be opened
            if(file.exists(paste0(getwd(), "/Rplots.pdf"))){
                tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\nTHIS FUNCTION CANNOT BE USED ON LINUX IF A Rplots.pdf FILE ALREADY EXISTS HERE\n", getwd())
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                open.fail <- suppressWarnings(try(grDevices::X11(), silent = TRUE))[] # try to open a X11 window. If open.fail == NULL, no problem, meaning that the X11 window is opened. If open.fail != NULL, a pdf can be opened here paste0(getwd(), "/Rplots.pdf")
                if(is.null(open.fail)){
                    ini.par <- par(no.readonly = remove.read.only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
                    invisible(grDevices::dev.off()) # close the new window
                }else if(file.exists(paste0(getwd(), "/Rplots.pdf"))){
                    file.remove(paste0(getwd(), "/Rplots.pdf")) # remove the pdf file
                    tempo.cat <- ("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\nTHIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM\nTO OVERCOME THIS, EITHER SET THE X GRAPHIC INTERFACE OF THE SYSTEM OR SET THE pdf ARGUMENT OF THE ", function.name, " FUNCTION TO TRUE AND RERUN")
                    stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
            }
        }
    }else{
        open.fail <- NULL
        grDevices::quartz()
        ini.par <- par(no.readonly = remove.read.only) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
        invisible(grDevices::dev.off()) # close the new window
    }
    # end par.ini recovery 
    zone.ini <- matrix(1, ncol=1) # to recover the initial parameters for next figure region when device region split into several figure regions
    if(pdf == TRUE){
        if(grepl(x = pdf.name, pattern = "\\.pdf$")){
            pdf.name <- sub(x = pdf.name, pattern = "\\.pdf$", replacement = "") # remove the last .pdf
        }
        pdf.loc <- paste0(pdf.path, "/", pdf.name, ".pdf")
        if(file.exists(pdf.loc) == TRUE & pdf.overwrite == FALSE){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", pdf.loc, " FILE ALREADY EXISTS AND CANNOT BE OVERWRITTEN DUE TO pdf.overwrite ARGUMENT SET TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else{
            grDevices::pdf(width = width, height = height, file=pdf.loc, paper = paper)
        }
    }else if(pdf == FALSE){
        pdf.loc <- NULL
        if(Sys.info()["sysname"] == "Windows"){ # .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
            grDevices::windows(width = width, height = height, rescale = rescale)
        }else if(Sys.info()["sysname"] == "Linux"){
            if( ! is.null(open.fail)){
                tempo.cat <- "ERROR IN function.name\nTHIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM\nTO OVERCOME THIS, EITHER SET THE X GRAPHIC INTERFACE OF THE SYSTEM OR SET THE pdf ARGUMENT OF THE function.name FUNCTION TO TRUE AND RERUN"
                stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                grDevices::X11(width = width, height = height)
            }
        }else{
            grDevices::quartz(width = width, height = height)
        }
    }
    # output
    # warning output
    # end warning output
    if(return.output == TRUE){
        output <- list(pdf.loc = pdf.loc, ini.par = ini.par, zone.ini = zone.ini, dim = grDevices::dev.size())
        return(output)
    }
    # output
    # end main code
}