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
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
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
        safer_check = TRUE
){
    # DEBUGGING
    # x.side = 0 ; x.log.scale = FALSE ; x.categ = NULL ; x.categ.pos = NULL ; x.lab = "" ; x.axis.size = 1.5 ; x.label.size = 1.5 ; x.dist.legend = 1 ; x.nb.inter.tick = 1 ; y.side = 0 ; y.log.scale = FALSE ; y.categ = NULL ; y.categ.pos = NULL ; y.lab = "" ; y.axis.size = 1.5 ; y.label.size = 1.5 ; y.dist.legend = 0.7 ; y.nb.inter.tick = 1 ; text.angle = 90 ; tick.length = 0.5 ; sec.tick.length = 0.3 ; bg.color = NULL ; grid.lwd = NULL ; grid.col = "white" ; corner.text = "" ; corner.text.size = 1 ; just.label.add = FALSE ; par.reset = FALSE ; custom.par = NULL ; safer_check = TRUE# for function debugging
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
    # critical operator checking
    if(safer_check == TRUE){
        saferGraph:::.base_op_check(
            external.function.name = function.name,
            external.package.name = package.name
    )
    }
    # end critical operator checking
    # package checking
    # check of lib.path
    # end check of lib.path

    # check of the required function from the required packages
    if(safer_check == TRUE){
        saferGraph:::.pack_and_function_check(
        fun = base::c(
            "saferDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name,
        external.package.name = package.name
    )
    }
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
    tempo <- saferDev::arg_check(data = x.side, options = base::c(0, 1, 3), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.log.scale, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(x.categ)){
        tempo <- saferDev::arg_check(data = x.categ, class = "character", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(x.categ.pos)){
        tempo <- saferDev::arg_check(data = x.categ.pos, class = "vector", mode = "numeric", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = x.lab, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.axis.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.label.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.dist.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = x.nb.inter.tick, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.side, options = base::c(0, 2, 4), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.log.scale, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(y.categ)){
        tempo <- saferDev::arg_check(data = y.categ, class = "character", na.contain = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(y.categ.pos)){
        tempo <- saferDev::arg_check(data = y.categ.pos, class = "vector", mode = "numeric", fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    tempo <- saferDev::arg_check(data = y.lab, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.axis.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.label.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.dist.legend, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = y.nb.inter.tick, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = text.angle, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = sec.tick.length, class = "vector", mode = "numeric", length = 1, prop = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(bg.color)){
        tempo <- saferDev::arg_check(data = bg.color, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if( ! (bg.color %in% grDevices::colors() | base::grepl(pattern = "^#", bg.color))){ # check color
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nbg.color ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY grDevices::colors()")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    if( ! base::is.null(grid.lwd)){
        tempo <- saferDev::arg_check(data = grid.lwd, class = "vector", mode = "numeric", neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(grid.col)){
        tempo <- saferDev::arg_check(data = grid.col, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
        if( ! (grid.col %in% grDevices::colors() | base::grepl(pattern = "^#", grid.col))){ # check color
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\ngrid.col ARGUMENT MUST BE A HEXADECIMAL COLOR VECTOR STARTING BY # OR A COLOR NAME GIVEN BY grDevices::colors()")
            text.check <- base::c(text.check, tempo.cat)
            argum.check <- base::c(argum.check, TRUE)
        }
    }
    tempo <- saferDev::arg_check(data = corner.text, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = corner.text.size, class = "vector", mode = "numeric", length = 1, neg.values = FALSE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = just.label.add, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = par.reset, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(custom.par)){
        tempo <- saferDev::arg_check(data = custom.par, typeof = "list", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with saferDev::arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <- base::c(
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
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
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
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nx.categ.pos MUST BE THE SAME LENGTH AS x.categ")
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
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nARGUMENT x.side CAN ONLY BE 1 OR 3")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            graphics::par(xpd = FALSE)
            x.text <- graphics::par("usr")[2]
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nPROBLEM WITH THE x.side (", x.side ,") OR x.log.scale (", x.log.scale,") ARGUMENTS")
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
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\ny.categ.pos MUST BE THE SAME LENGTH AS y.categ")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            graphics::axis(side = y.side, at = y.categ.pos, labels = base::rep("", base::length(y.categ)), lwd=0, lwd.ticks=1) # draw the line of the axis
            graphics::par(xpd = TRUE)
            if(base::isTRUE(base::all.equal(y.side, 2))){ #isTRUE(all.equal(y.side, 2)) is similar to y.side == 2L but deals with float
                graphics::text(x = x.mid.left.fig.region, y = y.categ.pos, labels = y.categ, srt = text.angle, cex = y.axis.size)
            }else if(base::isTRUE(base::all.equal(y.side, 4))){ # idem
                graphics::text(x = x.mid.right.fig.region, y = y.categ.pos, labels = y.categ, srt = text.angle, cex = y.axis.size)
            }else{
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nARGUMENT y.side CAN ONLY BE 2 OR 4")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            graphics::par(xpd = FALSE)
            y.text <- (graphics::par("usr")[4] + (graphics::par("usr")[4] - graphics::par("usr")[3]) / (graphics::par("plt")[4] - graphics::par("plt")[3]) * (1 - graphics::par("plt")[4]))
        }else{
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nPROBLEM WITH THE y.side (", y.side ,") OR y.log.scale (", y.log.scale,") ARGUMENTS")
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
                tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\ncustom.par ARGUMENT SHOULD HAVE THE NAMES OF THE COMPARTMENT LIST COMING FROM THE graphics::par() LIST")
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
