#' @title prior_plot
#' @description
#' Very convenient to erase the axes for post plot axis redrawing using post_plot().
#' 
#' Reinitialize and set the graphic parameters before plotting.
#' 
#' CANNOT be used if no graphic device already opened.
#' 
#' @param param.reinitial Single logical value. Reinitialize graphic parameters before applying the new ones, as defined by the other arguments? Either TRUE or FALSE.
#' @param xlog.scale Single logical value. Log scale for the x-axis? Either TRUE or FALSE. If TRUE, erases the x-axis, except legend, for further drawing by post_plot()(xlog argument of par()).
#' @param ylog.scale Single logical value. Log scale for the y-axis? Either TRUE or FALSE. If TRUE, erases the y-axis, except legend, for further drawing by post_plot()(ylog argument of par()).
#' @param remove.label Single logical value. Remove labels (axis legend) of the two axes? Either TRUE or FALSE (ann argument of par()).
#' @param remove.x.axis Single logical value. Remove x-axis except legend? Either TRUE or FALSE (control the xaxt argument of par()). Automately set to TRUE if xlog.scale == TRUE.
#' @param remove.y.axis Single logical value. Remove y-axis except legend? Either TRUE or FALSE (control the yaxt argument of par()). Automately set to TRUE if ylog.scale == TRUE.
#' @param std.x.range Single logical value. Standard range on the x-axis? TRUE (no range extend) or FALSE (4\% range extend). Controls xaxs argument of par() (TRUE is xaxs = "i", FALSE is xaxs = "r").
#' @param std.y.range Single logical value. Standard range on the y-axis? TRUE (no range extend) or FALSE (4\% range extend). Controls yaxs argument of par() (TRUE is yaxs = "i", FALSE is yaxs = "r").
#' @param down.space Single positive numeric value indicating the lower vertical margin (in inches, mai argument of par()).
#' @param left.space Single positive numeric value indicating the left horizontal margin (in inches, mai argument of par()).
#' @param up.space Single positive numeric value indicating the upper vertical margin between plot region and grapical window (in inches, mai argument of par()).
#' @param right.space Single positive numeric value indicating the right horizontal margin (in inches, mai argument of par()).
#' @param orient Single positive numeric value indicating the scale number orientation (las argument of par()). 0, always parallel to the axis; 1, always horizontal; 2, always perpendicular to the axis; 3, always vertical.
#' @param dist.legend Single positive numeric value that moves axis legends away in inches (first number of mgp argument of par() but in inches thus / 0.2).
#' @param tick.length Single positive numeric value indicating the length of the ticks (1 means complete the distance between the plot region and the axis numbers, 0.5 means half the length, etc. 0 means no tick.
#' @param box.type The bty argument of par(). Either "o", "l", "7", "c", "u", "]", the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box.
#' @param amplif.label Single positive numeric value to increase or decrease the size of the text in legends.
#' @param amplif.axis Single positive numeric value to increase or decrease the size of the scale numbers in axis.
#' @param display.extend Single logical value. Extend display beyond plotting region? Either TRUE or FALSE (xpd argument of par() without NA).
#' @param return.par Single logical value. Return graphic parameter modification?
#' @returns 
#' Graphic parameter modification.
#' @examples
#' \dontrun{
#' # Screen devices should not be used in examples
#' prior_plot(param.reinitial = FALSE, xlog.scale = FALSE, ylog.scale = FALSE, remove.label = TRUE, remove.x.axis = TRUE, remove.y.axis = TRUE, std.x.range = TRUE, std.y.range = TRUE, down.space = 1, left.space = 1, up.space = 1, right.space = 1, orient = 1, dist.legend = 4.5, tick.length = 0.5, box.type = "n", amplif.label = 1, amplif.axis = 1, display.extend = FALSE, return.par = FALSE)
#' }
#' @importFrom saferDev arg_check
#' @importFrom grDevices dev.list
#' @importFrom grDevices dev.cur
#' @importFrom graphics par
#' @importFrom grDevices dev.off
#' @importFrom grDevices dev.set
#' @export
prior_plot <- function(
        param.reinitial = FALSE, 
        xlog.scale = FALSE, 
        ylog.scale = FALSE, 
        remove.label = TRUE, 
        remove.x.axis = TRUE, 
        remove.y.axis = TRUE, 
        std.x.range = TRUE, 
        std.y.range = TRUE, 
        down.space = 1, 
        left.space = 1, 
        up.space = 1, 
        right.space = 1, 
        orient = 1, 
        dist.legend = 3.5, 
        tick.length = 0.5, 
        box.type = "n", 
        amplif.label = 1, 
        amplif.axis = 1, 
        display.extend = FALSE, 
        return.par = FALSE
){
    # DEBUGGING
    # param.reinitial = FALSE ; xlog.scale = FALSE ; ylog.scale = FALSE ; remove.label = TRUE ; remove.x.axis = TRUE ; remove.y.axis = TRUE ; std.x.range = TRUE ; std.y.range = TRUE ; down.space = 1 ; left.space = 1 ; up.space = 1 ; right.space = 1 ; orient = 1 ; dist.legend = 4.5 ; tick.length = 0.5 ; box.type = "n" ; amplif.label = 1 ; amplif.axis = 1 ; display.extend = FALSE ; return.par = FALSE # for function debugging
    # package name
    package.name <- "saferGraph"
    # end package name
    # function name
    ini <- base::match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    
    # package checking
    # check of lib.path
    # end check of lib.path
    # check of the required function from the required packages
    .pack_and_function_check(
        fun = base::c(
            "graphics::par",
            "grDevices::dev.cur",
            "grDevices::dev.list",
            "grDevices::dev.off",
            "grDevices::dev.set",
            "grDevices::quartz",
            "grDevices::windows",
            "grDevices::X11",
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
    # argument checking with saferDev::arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = param.reinitial, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = xlog.scale, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = ylog.scale, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = remove.label, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = remove.x.axis, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = remove.y.axis, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = std.x.range, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = std.y.range, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = down.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = left.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = up.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = right.space, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = orient, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = dist.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = box.type, options = base::c("o", "l", "7", "c", "u", "]", "n"), length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = amplif.label, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = amplif.axis, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = display.extend, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = return.par, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # argument checking with saferDev::arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) == "list", na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-base::c(
        "param.reinitial", 
        "xlog.scale", 
        "ylog.scale", 
        "remove.label", 
        "remove.x.axis", 
        "remove.y.axis", 
        "std.x.range", 
        "std.y.range", 
        "down.space", 
        "left.space", 
        "up.space", 
        "right.space", 
        "orient", 
        "dist.legend", 
        "tick.length", 
        "box.type", 
        "amplif.label", 
        "amplif.axis", 
        "display.extend", 
        "return.par"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
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
    if(base::is.null(grDevices::dev.list())){ 
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THIS FUNCTION CANNOT BE USED IF NO GRAPHIC DEVICE ALREADY OPENED (grDevices::dev.list() IS CURRENTLY NULL)")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # par.ini recovery
    # cannot use pdf(file = NULL), because some small differences between pdf() and other devices. For instance, differences with windows() for par()$fin, par()$pin and par()$plt
    if(param.reinitial == TRUE){
        if( ! base::all(base::names(grDevices::dev.cur()) == "null device", na.rm = TRUE)){
            active.wind.nb <- grDevices::dev.cur()
        }else{
            active.wind.nb <- 0
        }
        if(base::Sys.info()["sysname"] == "Windows"){ # Note that .Platform$OS.type() only says "unix" for macOS and Linux and "Windows" for Windows
            grDevices::windows()
            ini.par <- graphics::par(no.readonly = FALSE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
            base::invisible(grDevices::dev.off()) # close the new window
        }else if(base::Sys.info()["sysname"] == "Linux"){
            if(base::file.exists(base::paste0(base::getwd(), "/Rplots.pdf"))){
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THIS FUNCTION CANNOT BE USED ON LINUX WITH param.reinitial SET TO TRUE IF A Rplots.pdf FILE ALREADY EXISTS HERE: ", base::getwd())
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }else{
                open.fail <- base::suppressWarnings(base::try(grDevices::X11(), silent = TRUE))[] # try to open a X11 window. If open.fail == NULL, no problem, meaning that the X11 window is opened. If open.fail != NULL, a pdf can be opened here paste0(getwd(), "/Rplots.pdf")
                if(base::is.null(open.fail)){
                    ini.par <- graphics::par(no.readonly = FALSE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
                    base::invisible(grDevices::dev.off()) # close the new window
                }else if(base::file.exists(base::paste0(base::getwd(), "/Rplots.pdf"))){
                    ini.par <- graphics::par(no.readonly = FALSE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened
                    base::invisible(grDevices::dev.off()) # close the new window
                    base::file.remove(base::paste0(base::getwd(), "/Rplots.pdf")) # remove the pdf file
                }else{
                    tempo.cat <- base::paste0("ERROR IN ", function.name, " \nTHIS FUNCTION CANNOT OPEN GUI ON LINUX OR NON MACOS UNIX SYSTEM\nTO OVERCOME THIS, PLEASE USE A PDF GRAPHIC INTERFACE AND RERUN")
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
                }
            }
        }else{ # macOS
            grDevices::quartz()
            ini.par <- graphics::par(no.readonly = FALSE) # to recover the initial graphical parameters if required (reset). BEWARE: this command alone opens a pdf of GUI window if no window already opened. But here, protected with the code because always a tempo window opened)
            base::invisible(grDevices::dev.off()) # close the new window
        }
        if( ! base::all(base::names(grDevices::dev.cur()) == "null device", na.rm = TRUE)){
            base::invisible(grDevices::dev.set(active.wind.nb)) # go back to the active window if exists
            graphics::par(ini.par) # apply the initial par to current window
        }
    }
    # end par.ini recovery
    if(remove.x.axis == TRUE){
        graphics::par(xaxt = "n") # suppress the y-axis label
    }else{
        graphics::par(xaxt = "s")
    }
    if(remove.y.axis == TRUE){
        graphics::par(yaxt = "n") # suppress the y-axis label
    }else{
        graphics::par(yaxt = "s")
    }
    if(std.x.range == TRUE){
        graphics::par(xaxs = "i")
    }else{
        graphics::par(xaxs = "r")
    }
    if(std.y.range == TRUE){
        graphics::par(yaxs = "i")
    }else{
        graphics::par(yaxs = "r")
    }
    graphics::par(mai = base::c(down.space, left.space, up.space, right.space), ann = ! remove.label, las = orient, mgp = base::c(dist.legend/0.2, 1, 0), xpd = display.extend, bty= box.type, cex.lab = amplif.label, cex.axis = amplif.axis)
    graphics::par(tcl = -graphics::par()$mgp[2] * tick.length) # tcl gives the length of the ticks as proportion of line text, knowing that mgp is in text lines. So the main ticks are a 0.5 of the distance of the axis numbers by default. The sign provides the side of the tick (negative for outside of the plot region)
    if(xlog.scale == TRUE){
        graphics::par(xaxt = "n", xlog = TRUE) # suppress the x-axis label
    }else{
        graphics::par(xlog = FALSE)
    }
    if(ylog.scale == TRUE){
        graphics::par(yaxt = "n", ylog = TRUE) # suppress the y-axis label
    }else{
        graphics::par(ylog = FALSE)
    }
    # output
    # warning output
    # end warning output
    if(return.par == TRUE){
        tempo.par <- graphics::par()
        base::return(tempo.par)
    }
    # end output
    # end main code
}
