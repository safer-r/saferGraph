testthat::test_that("post_plot()", {

    ## data argument values
    int1 <- 1L
    num1 <- 1.5
    num2 <- 0.5
    str1 <- "a"
    vec1 <- base::c("A", "B", "C")
    list1 <- base::list(mar = base::c(5, 4, 4, 2) + 0.1)
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(post_plot(caca = 1)) # to test for the absence of ...
    pdf_ini <- grDevices::pdf(file = base::file.path(base::tempdir(), "post_plot_ini.pdf"), width = 7, height = 7, paper = "special", title = "post_plot_ini")
    graphics::plot(x = 1:3, y = 1:3)
    testthat::expect_no_error(post_plot(x.side = int1, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 2, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "y", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "test", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_no_error(saferGraph:::post_plot()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # runs with all defaults on the open pdf device: also covers the arg_user_setting_eval <- NULL branch
    testthat::expect_no_error(post_plot()) # to cover the arg_user_setting_eval <- NULL branch (all args have defaults, so this runs the whole function with defaults)
    #### end function name

    ########  arg with no default values
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(post_plot(x.side = NULL))
    testthat::expect_error(post_plot(x.log.scale = NULL))
    testthat::expect_no_error(post_plot(x.categ = NULL))
    testthat::expect_no_error(post_plot(x.categ.pos = NULL))
    testthat::expect_error(post_plot(x.lab = NULL))
    testthat::expect_error(post_plot(x.axis.size = NULL))
    testthat::expect_error(post_plot(x.label.size = NULL))
    testthat::expect_error(post_plot(x.dist.legend = NULL))
    testthat::expect_error(post_plot(x.nb.inter.tick = NULL))
    testthat::expect_error(post_plot(y.side = NULL))
    testthat::expect_error(post_plot(y.log.scale = NULL))
    testthat::expect_no_error(post_plot(y.categ = NULL))
    testthat::expect_no_error(post_plot(y.categ.pos = NULL))
    testthat::expect_error(post_plot(y.lab = NULL))
    testthat::expect_error(post_plot(y.axis.size = NULL))
    testthat::expect_error(post_plot(y.label.size = NULL))
    testthat::expect_error(post_plot(y.dist.legend = NULL))
    testthat::expect_error(post_plot(y.nb.inter.tick = NULL))
    testthat::expect_error(post_plot(text.angle = NULL))
    testthat::expect_error(post_plot(tick.length = NULL))
    testthat::expect_error(post_plot(sec.tick.length = NULL))
    testthat::expect_no_error(post_plot(bg.color = NULL))
    testthat::expect_no_error(post_plot(grid.lwd = NULL))
    testthat::expect_error(post_plot(grid.col = NULL))
    testthat::expect_error(post_plot(corner.text = NULL))
    testthat::expect_error(post_plot(corner.text.size = NULL))
    testthat::expect_error(post_plot(just.label.add = NULL))
    testthat::expect_error(post_plot(par.reset = NULL))
    testthat::expect_no_error(post_plot(custom.par = NULL))
    testthat::expect_error(post_plot(safer_check = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(post_plot(x.categ = character()))
    testthat::expect_error(post_plot(x.categ.pos = double()))
    testthat::expect_error(post_plot(x.lab = character()))
    testthat::expect_error(post_plot(x.axis.size = double()))
    testthat::expect_error(post_plot(x.label.size = double()))
    testthat::expect_error(post_plot(x.dist.legend = double()))
    testthat::expect_error(post_plot(x.nb.inter.tick = integer()))
    testthat::expect_error(post_plot(y.categ = character()))
    testthat::expect_error(post_plot(y.categ.pos = double()))
    testthat::expect_error(post_plot(y.lab = character()))
    testthat::expect_error(post_plot(y.axis.size = double()))
    testthat::expect_error(post_plot(y.label.size = double()))
    testthat::expect_error(post_plot(y.dist.legend = double()))
    testthat::expect_error(post_plot(y.nb.inter.tick = integer()))
    testthat::expect_error(post_plot(text.angle = double()))
    testthat::expect_error(post_plot(tick.length = double()))
    testthat::expect_error(post_plot(sec.tick.length = double()))
    testthat::expect_error(post_plot(bg.color = character()))
    testthat::expect_error(post_plot(grid.lwd = double()))
    testthat::expect_error(post_plot(grid.col = character()))
    testthat::expect_error(post_plot(corner.text = character()))
    testthat::expect_error(post_plot(corner.text.size = double()))
    testthat::expect_error(post_plot(just.label.add = logical()))
    testthat::expect_error(post_plot(par.reset = logical()))
    testthat::expect_error(post_plot(custom.par = list()))
    testthat::expect_error(post_plot(safer_check = logical()))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(post_plot(x.side = NA))
    testthat::expect_error(post_plot(x.log.scale = NA))
    testthat::expect_error(post_plot(x.categ = NA))
    testthat::expect_error(post_plot(x.categ.pos = NA))
    testthat::expect_error(post_plot(x.lab = NA))
    testthat::expect_error(post_plot(x.axis.size = NA))
    testthat::expect_error(post_plot(x.label.size = NA))
    testthat::expect_error(post_plot(x.dist.legend = NA))
    testthat::expect_error(post_plot(x.nb.inter.tick = NA))
    testthat::expect_error(post_plot(y.side = NA))
    testthat::expect_error(post_plot(y.log.scale = NA))
    testthat::expect_error(post_plot(y.categ = NA))
    testthat::expect_error(post_plot(y.categ.pos = NA))
    testthat::expect_error(post_plot(y.lab = NA))
    testthat::expect_error(post_plot(y.axis.size = NA))
    testthat::expect_error(post_plot(y.label.size = NA))
    testthat::expect_error(post_plot(y.dist.legend = NA))
    testthat::expect_error(post_plot(y.nb.inter.tick = NA))
    testthat::expect_error(post_plot(text.angle = NA))
    testthat::expect_error(post_plot(tick.length = NA))
    testthat::expect_error(post_plot(sec.tick.length = NA))
    testthat::expect_error(post_plot(bg.color = NA))
    testthat::expect_error(post_plot(grid.lwd = NA))
    testthat::expect_error(post_plot(grid.col = NA))
    testthat::expect_error(post_plot(corner.text = NA))
    testthat::expect_error(post_plot(corner.text.size = NA))
    testthat::expect_error(post_plot(just.label.add = NA))
    testthat::expect_error(post_plot(par.reset = NA))
    testthat::expect_error(post_plot(custom.par = NA))
    testthat::expect_error(post_plot(safer_check = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(post_plot(safer_check = 1))
    testthat::expect_error(post_plot(safer_check = c(TRUE, FALSE)))
    testthat::expect_error(post_plot(safer_check = mat1))
    testthat::expect_error(post_plot(safer_check = factor1))
    testthat::expect_error(post_plot(safer_check = expr1))
    testthat::expect_error(post_plot(safer_check = fun1))
    testthat::expect_no_error(post_plot(safer_check = TRUE))
    testthat::expect_no_error(post_plot(safer_check = FALSE))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(post_plot(lib_path = NULL))
    testthat::expect_error(post_plot(lib_path = 1))
    testthat::expect_error(post_plot(lib_path = c(TRUE, FALSE)))
    testthat::expect_error(post_plot(lib_path = mat1))
    testthat::expect_error(post_plot(lib_path = factor1))
    testthat::expect_error(post_plot(lib_path = expr1))
    testthat::expect_error(post_plot(lib_path = fun1))
    testthat::expect_error(post_plot(lib_path = "PATH_NOT_GOOD"))
    testthat::expect_no_error(post_plot(lib_path = base:::.libPaths(new = , include.site = TRUE)))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(post_plot(safer_check = TRUE)) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(post_plot(safer_check = TRUE)) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x.side
    testthat::expect_error(post_plot(x.side = NULL))
    testthat::expect_error(post_plot(x.side = NA))
    testthat::expect_error(post_plot(x.side = "a"))
    testthat::expect_error(post_plot(x.side = c(1, 3)))
    testthat::expect_error(post_plot(x.side = mat1))
    testthat::expect_error(post_plot(x.side = factor1))
    testthat::expect_error(post_plot(x.side = expr1))
    testthat::expect_error(post_plot(x.side = fun1))
    testthat::expect_error(post_plot(x.side = 2)) # not in the allowed options c(0, 1, 3)
    testthat::expect_error(post_plot(x.side = -1)) # negative not allowed
    testthat::expect_error(post_plot(x.side = 1.5)) # not integer
    # end x.side
    # x.log.scale
    testthat::expect_error(post_plot(x.log.scale = NULL))
    testthat::expect_error(post_plot(x.log.scale = NA))
    testthat::expect_error(post_plot(x.log.scale = 1))
    testthat::expect_error(post_plot(x.log.scale = "TRUE"))
    testthat::expect_error(post_plot(x.log.scale = c(TRUE, FALSE)))
    testthat::expect_error(post_plot(x.log.scale = mat1))
    testthat::expect_error(post_plot(x.log.scale = factor1))
    testthat::expect_error(post_plot(x.log.scale = expr1))
    testthat::expect_error(post_plot(x.log.scale = fun1))
    # end x.log.scale
    # x.categ (checked only if non NULL)
    testthat::expect_no_error(post_plot(x.categ = vec1))
    testthat::expect_error(post_plot(x.categ = NA))
    testthat::expect_error(post_plot(x.categ = 1))
    testthat::expect_error(post_plot(x.categ = mat1))
    testthat::expect_error(post_plot(x.categ = factor1))
    testthat::expect_error(post_plot(x.categ = expr1))
    testthat::expect_error(post_plot(x.categ = fun1))
    # end x.categ
    # x.categ.pos (checked only if non NULL)
    testthat::expect_no_error(post_plot(x.categ = vec1, x.categ.pos = base::c(1, 2, 3)))
    testthat::expect_error(post_plot(x.categ.pos = NA))
    testthat::expect_error(post_plot(x.categ.pos = "a"))
    testthat::expect_error(post_plot(x.categ.pos = c(TRUE, FALSE)))
    testthat::expect_no_error(post_plot(x.categ.pos = mat1)) # matrix has mode "numeric", allowed by arg_check()
    testthat::expect_no_error(post_plot(x.categ.pos = factor1)) # factor has mode "numeric", allowed by arg_check()
    testthat::expect_error(post_plot(x.categ.pos = expr1))
    testthat::expect_error(post_plot(x.categ.pos = fun1))
    # end x.categ.pos
    # x.lab
    testthat::expect_error(post_plot(x.lab = NULL))
    testthat::expect_error(post_plot(x.lab = NA))
    testthat::expect_error(post_plot(x.lab = 1))
    testthat::expect_error(post_plot(x.lab = c("a", "b")))
    testthat::expect_error(post_plot(x.lab = mat1))
    testthat::expect_error(post_plot(x.lab = factor1))
    testthat::expect_error(post_plot(x.lab = expr1))
    testthat::expect_error(post_plot(x.lab = fun1))
    testthat::expect_no_error(post_plot(x.lab = "")) # "" allowed (can be empty)
    # end x.lab
    # x.axis.size
    testthat::expect_error(post_plot(x.axis.size = NULL))
    testthat::expect_error(post_plot(x.axis.size = NA))
    testthat::expect_error(post_plot(x.axis.size = "a"))
    testthat::expect_error(post_plot(x.axis.size = TRUE))
    testthat::expect_error(post_plot(x.axis.size = c(1.5, 1.5)))
    testthat::expect_error(post_plot(x.axis.size = mat1))
    testthat::expect_error(post_plot(x.axis.size = factor1))
    testthat::expect_error(post_plot(x.axis.size = expr1))
    testthat::expect_error(post_plot(x.axis.size = fun1))
    testthat::expect_error(post_plot(x.axis.size = -1)) # negative not allowed
    # end x.axis.size
    # x.label.size: same checks as x.axis.size
    testthat::expect_error(post_plot(x.label.size = -1))
    testthat::expect_error(post_plot(x.label.size = c(1.5, 1.5)))
    testthat::expect_error(post_plot(x.label.size = "a"))
    # end x.label.size
    # x.dist.legend
    testthat::expect_error(post_plot(x.dist.legend = -1)) # negative not allowed
    testthat::expect_error(post_plot(x.dist.legend = c(0.5, 0.5)))
    testthat::expect_error(post_plot(x.dist.legend = "a"))
    # end x.dist.legend
    # x.nb.inter.tick
    testthat::expect_error(post_plot(x.nb.inter.tick = 1.5)) # not integer
    testthat::expect_error(post_plot(x.nb.inter.tick = "a"))
    testthat::expect_error(post_plot(x.nb.inter.tick = -1)) # negative not allowed
    testthat::expect_error(post_plot(x.nb.inter.tick = c(1L, 2L)))
    testthat::expect_error(post_plot(x.nb.inter.tick = TRUE))
    testthat::expect_no_error(post_plot(x.nb.inter.tick = 0L))
    # end x.nb.inter.tick
    # y.side
    testthat::expect_error(post_plot(y.side = 1)) # not in the allowed options c(0, 2, 4)
    testthat::expect_error(post_plot(y.side = 3)) # not in the allowed options c(0, 2, 4)
    testthat::expect_error(post_plot(y.side = -2)) # negative not allowed
    testthat::expect_error(post_plot(y.side = 2.5)) # not integer
    testthat::expect_error(post_plot(y.side = "a"))
    testthat::expect_error(post_plot(y.side = c(2, 4)))
    # end y.side
    # y.log.scale: same as x.log.scale
    testthat::expect_error(post_plot(y.log.scale = 1))
    testthat::expect_error(post_plot(y.log.scale = "TRUE"))
    testthat::expect_error(post_plot(y.log.scale = c(TRUE, FALSE)))
    # end y.log.scale
    # y.categ (checked only if non NULL)
    testthat::expect_no_error(post_plot(y.categ = vec1, y.side = 2))
    testthat::expect_error(post_plot(y.categ = 1))
    testthat::expect_error(post_plot(y.categ = mat1))
    testthat::expect_error(post_plot(y.categ = factor1))
    testthat::expect_error(post_plot(y.categ = expr1))
    testthat::expect_error(post_plot(y.categ = fun1))
    # end y.categ
    # y.categ.pos (checked only if non NULL)
    testthat::expect_no_error(post_plot(y.categ = vec1, y.categ.pos = base::c(1, 2, 3), y.side = 2))
    testthat::expect_error(post_plot(y.categ.pos = "a"))
    testthat::expect_error(post_plot(y.categ.pos = c(TRUE, FALSE)))
    testthat::expect_no_error(post_plot(y.categ.pos = mat1)) # matrix has mode "numeric", allowed by arg_check()
    # end y.categ.pos
    # y.lab
    testthat::expect_error(post_plot(y.lab = NULL))
    testthat::expect_error(post_plot(y.lab = 1))
    testthat::expect_error(post_plot(y.lab = c("a", "b")))
    testthat::expect_no_error(post_plot(y.lab = "")) # "" allowed (can be empty)
    # end y.lab
    # y.axis.size
    testthat::expect_error(post_plot(y.axis.size = -1)) # negative not allowed
    testthat::expect_error(post_plot(y.axis.size = c(1.5, 1.5)))
    testthat::expect_error(post_plot(y.axis.size = "a"))
    # end y.axis.size
    # y.label.size: same as y.axis.size
    testthat::expect_error(post_plot(y.label.size = -1))
    testthat::expect_error(post_plot(y.label.size = c(1.5, 1.5)))
    # end y.label.size
    # y.dist.legend
    testthat::expect_error(post_plot(y.dist.legend = -1))
    testthat::expect_error(post_plot(y.dist.legend = c(0.5, 0.5)))
    # end y.dist.legend
    # y.nb.inter.tick: same as x.nb.inter.tick
    testthat::expect_error(post_plot(y.nb.inter.tick = 1.5))
    testthat::expect_error(post_plot(y.nb.inter.tick = -1))
    testthat::expect_no_error(post_plot(y.nb.inter.tick = 0L))
    # end y.nb.inter.tick
    # text.angle
    testthat::expect_error(post_plot(text.angle = "a"))
    testthat::expect_error(post_plot(text.angle = c(90, 90)))
    testthat::expect_error(post_plot(text.angle = mat1))
    testthat::expect_error(post_plot(text.angle = factor1))
    testthat::expect_error(post_plot(text.angle = expr1))
    testthat::expect_error(post_plot(text.angle = fun1))
    testthat::expect_error(post_plot(text.angle = -90)) # negative not allowed
    # end text.angle
    # tick.length
    testthat::expect_error(post_plot(tick.length = "a"))
    testthat::expect_error(post_plot(tick.length = c(0.5, 0.5)))
    testthat::expect_error(post_plot(tick.length = mat1))
    testthat::expect_error(post_plot(tick.length = -1)) # negative not allowed
    # end tick.length
    # sec.tick.length
    testthat::expect_error(post_plot(sec.tick.length = "a"))
    testthat::expect_error(post_plot(sec.tick.length = c(0.3, 0.3)))
    testthat::expect_error(post_plot(sec.tick.length = -1)) # negative not allowed
    # end sec.tick.length
    # bg.color (checked only if non NULL)
    testthat::expect_no_error(post_plot(bg.color = "white"))
    testthat::expect_no_error(post_plot(bg.color = "#FFFFFF"))
    testthat::expect_error(post_plot(bg.color = NA))
    testthat::expect_error(post_plot(bg.color = 1))
    testthat::expect_error(post_plot(bg.color = c("white", "black")))
    testthat::expect_error(post_plot(bg.color = "NOTACOLOR")) # not in grDevices::colors() and not hexadecimal
    testthat::expect_error(post_plot(bg.color = mat1))
    testthat::expect_error(post_plot(bg.color = factor1))
    testthat::expect_error(post_plot(bg.color = expr1))
    testthat::expect_error(post_plot(bg.color = fun1))
    # end bg.color
    # grid.lwd (checked only if non NULL)
    testthat::expect_no_error(post_plot(grid.lwd = 1))
    testthat::expect_error(post_plot(grid.lwd = NA))
    testthat::expect_error(post_plot(grid.lwd = "a"))
    testthat::expect_error(post_plot(grid.lwd = TRUE))
    testthat::expect_error(post_plot(grid.lwd = factor1))
    testthat::expect_error(post_plot(grid.lwd = expr1))
    testthat::expect_error(post_plot(grid.lwd = fun1))
    testthat::expect_error(post_plot(grid.lwd = -1)) # negative not allowed
    testthat::expect_no_error(post_plot(grid.lwd = base::c(1, 2)))
    # end grid.lwd
    # grid.col (checked only if non NULL)
    testthat::expect_no_error(post_plot(grid.lwd = 1, grid.col = "grey"))
    testthat::expect_error(post_plot(grid.lwd = 1, grid.col = NA))
    testthat::expect_error(post_plot(grid.lwd = 1, grid.col = 1))
    testthat::expect_error(post_plot(grid.lwd = 1, grid.col = c("grey", "white")))
    testthat::expect_error(post_plot(grid.lwd = 1, grid.col = "NOTACOLOR"))
    testthat::expect_error(post_plot(grid.lwd = 1, grid.col = mat1))
    testthat::expect_error(post_plot(grid.lwd = 1, grid.col = factor1))
    testthat::expect_error(post_plot(grid.lwd = 1, grid.col = expr1))
    testthat::expect_error(post_plot(grid.lwd = 1, grid.col = fun1))
    # end grid.col
    # corner.text
    testthat::expect_error(post_plot(corner.text = NULL))
    testthat::expect_error(post_plot(corner.text = NA))
    testthat::expect_error(post_plot(corner.text = 1))
    testthat::expect_error(post_plot(corner.text = c("a", "b")))
    testthat::expect_error(post_plot(corner.text = mat1))
    testthat::expect_error(post_plot(corner.text = factor1))
    testthat::expect_error(post_plot(corner.text = expr1))
    testthat::expect_error(post_plot(corner.text = fun1))
    testthat::expect_no_error(post_plot(corner.text = "")) # "" allowed (can be empty)
    # end corner.text
    # corner.text.size
    testthat::expect_error(post_plot(corner.text.size = -1))
    testthat::expect_error(post_plot(corner.text.size = c(1, 1)))
    testthat::expect_error(post_plot(corner.text.size = "a"))
    # end corner.text.size
    # just.label.add
    testthat::expect_error(post_plot(just.label.add = NULL))
    testthat::expect_error(post_plot(just.label.add = NA))
    testthat::expect_error(post_plot(just.label.add = 1))
    testthat::expect_error(post_plot(just.label.add = "TRUE"))
    testthat::expect_error(post_plot(just.label.add = c(TRUE, FALSE)))
    testthat::expect_error(post_plot(just.label.add = mat1))
    testthat::expect_error(post_plot(just.label.add = factor1))
    testthat::expect_error(post_plot(just.label.add = expr1))
    testthat::expect_error(post_plot(just.label.add = fun1))
    # end just.label.add
    # par.reset
    testthat::expect_error(post_plot(par.reset = NULL))
    testthat::expect_error(post_plot(par.reset = NA))
    testthat::expect_error(post_plot(par.reset = 1))
    testthat::expect_error(post_plot(par.reset = "TRUE"))
    testthat::expect_error(post_plot(par.reset = c(TRUE, FALSE)))
    testthat::expect_error(post_plot(par.reset = mat1))
    testthat::expect_error(post_plot(par.reset = factor1))
    testthat::expect_error(post_plot(par.reset = expr1))
    testthat::expect_error(post_plot(par.reset = fun1))
    # end par.reset
    # custom.par (checked only if non NULL)
    testthat::expect_error(post_plot(custom.par = NA))
    testthat::expect_error(post_plot(custom.par = "a"))
    testthat::expect_error(post_plot(custom.par = c(1, 2)))
    testthat::expect_error(post_plot(custom.par = mat1))
    testthat::expect_error(post_plot(custom.par = factor1))
    testthat::expect_error(post_plot(custom.par = expr1))
    testthat::expect_error(post_plot(custom.par = fun1))
    # end custom.par
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(post_plot(x.categ = c("", "B")))
    testthat::expect_error(post_plot(y.categ = c("", "B")))
    testthat::expect_error(post_plot(bg.color = ""))
    testthat::expect_error(post_plot(grid.lwd = 1, grid.col = ""))
    # x.lab, y.lab and corner.text can contain "" (inactivated in tempo_arg)
    testthat::expect_no_error(post_plot(x.lab = "", y.lab = "", corner.text = ""))
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
    testthat::expect_error(post_plot(x.side = 1, x.categ = vec1, x.categ.pos = base::c(1, 2))) # x.categ.pos length != x.categ length (checked in main code, only when x.side != 0)
    testthat::expect_error(post_plot(y.categ = vec1, y.categ.pos = base::c(1, 2), y.side = 2)) # y.categ.pos length != y.categ length (checked in main code)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # numeric axis, bottom
    graphics::plot(x = 1:3, y = 1:3)
    result <- post_plot(x.side = 1, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result))
    testthat::expect_true(object = base::length(x = result) > 20)
    testthat::expect_true(object = base::is.numeric(x = result$x.mid.left.dev.region))
    testthat::expect_null(object = result$text)
    # numeric axis, top with inter ticks
    result2 <- post_plot(x.side = 3, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = 2L, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result2))
    # log scale x
    graphics::plot(x = 1:3, y = 1:3, log = "x")
    result3 <- post_plot(x.side = 1, x.log.scale = TRUE, x.categ = NULL, x.categ.pos = NULL, x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result3))
    # numeric y axis, left, with inter ticks
    result4 <- post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 2, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "y", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = 2L, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result4))
    # log scale y, right side
    graphics::plot(x = 1:3, y = 1:3, log = "y")
    result5 <- post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 4, y.log.scale = TRUE, y.categ = NULL, y.categ.pos = NULL, y.lab = "y", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result5))
    # x.categ
    graphics::plot(x = 1:3, y = 1:3)
    result6 <- post_plot(x.side = 1, x.log.scale = FALSE, x.categ = vec1, x.categ.pos = NULL, x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result6))
    # x.categ with x.side = 3 (top axis)
    graphics::plot(x = 1:3, y = 1:3)
    result6b <- post_plot(x.side = 3, x.log.scale = FALSE, x.categ = vec1, x.categ.pos = base::c(1, 2, 3), x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result6b))
    # y.categ, side 4
    graphics::plot(x = 1:3, y = 1:3)
    result7 <- post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 4, y.log.scale = FALSE, y.categ = vec1, y.categ.pos = base::c(1, 2, 3), y.lab = "y", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result7))
    # bg.color, grid.lwd, grid.col
    graphics::plot(x = 1:3, y = 1:3)
    result8 <- post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = "grey", grid.lwd = 1, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result8))
    # just.label.add
    result9 <- post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "y", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = TRUE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result9))
    # par.reset = TRUE and custom.par non NULL
    graphics::plot(x = 1:3, y = 1:3)
    result10 <- post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = TRUE, custom.par = list1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::grepl(x = result10$text[1], pattern = "GRAPH PARAMETERS SET TO VALUES DEFINED BY custom.par ARGUMENT", fixed = TRUE))
    # par.reset = TRUE and custom.par = NULL
    graphics::plot(x = 1:3, y = 1:3)
    result11 <- post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = TRUE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::grepl(x = result11$text[1], pattern = "GRAPH PARAMETERS RESET TO graphics::par() DEFAULT VALUES", fixed = TRUE))
    # wrong x.categ.pos length
    testthat::expect_error(post_plot(x.side = 1, x.log.scale = FALSE, x.categ = vec1, x.categ.pos = base::c(1, 2), x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # wrong y.categ.pos length
    testthat::expect_error(post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 2, y.log.scale = FALSE, y.categ = vec1, y.categ.pos = base::c(1, 2), y.lab = "y", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "c", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # log scale with xaxp or yaxp containing 0 (main code lines replacing 0 by 10^-30): forced with graphics::par(xaxp = c(0, 1, 3)) on a log window, because R never sets 0 alone when plotting (the 4% extension makes usr[1] < 0, and xaxp limits are then 10^usr, which is 0.1 and not 0)
    graphics::plot.new()
    graphics::plot.window(xlim = c(1, 10), ylim = c(1, 10), log = "x")
    graphics::par(xaxp = c(0, 1, 3))
    result14 <- post_plot(x.side = 1, x.log.scale = TRUE, x.categ = NULL, x.categ.pos = NULL, x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result14))
    graphics::plot.new()
    graphics::plot.window(xlim = c(1, 10), ylim = c(1, 10), log = "y")
    graphics::par(yaxp = c(0, 1, 3))
    result15 <- post_plot(x.side = 0, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 2, y.log.scale = TRUE, y.categ = NULL, y.categ.pos = NULL, y.lab = "y", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "", corner.text.size = 1, just.label.add = FALSE, par.reset = FALSE, custom.par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result15))
    # custom.par with wrong names and par.reset = TRUE (main code error)
    tempo_par_err <- base::tryCatch(
        expr = post_plot(x.side = 1, x.log.scale = FALSE, x.categ = NULL, x.categ.pos = NULL, x.lab = "x", x.axis.size = num1, x.label.size = num1, x.dist.legend = num2, x.nb.inter.tick = int1, y.side = 0, y.log.scale = FALSE, y.categ = NULL, y.categ.pos = NULL, y.lab = "", y.axis.size = num1, y.label.size = num1, y.dist.legend = num2, y.nb.inter.tick = int1, text.angle = 90, tick.length = num2, sec.tick.length = 0.3, bg.color = NULL, grid.lwd = NULL, grid.col = "white", corner.text = "", corner.text.size = 1, just.label.add = FALSE, par.reset = TRUE, custom.par = base::list(NOTGOOD = 1), safer_check = TRUE, lib_path = NULL, error_text = ""),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = tempo_par_err, pattern = "custom.par ARGUMENT SHOULD HAVE THE NAMES OF THE COMPARTMENT LIST COMING FROM THE graphics::par() LIST", fixed = TRUE))
    ## end other tests
    grDevices::dev.off(which = grDevices::dev.cur()) # close the pdf device opened at the beginning
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
