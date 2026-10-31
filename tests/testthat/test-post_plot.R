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
    testthat::expect_no_error(post_plot(x_side = int1, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 2, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "y", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "test", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_no_error(saferGraph:::post_plot()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # runs with all defaults on the open pdf device: also covers the arg_user_setting_eval <- NULL branch
    testthat::expect_no_error(post_plot()) # to cover the arg_user_setting_eval <- NULL branch (all args have defaults, so this runs the whole function with defaults)
    #### end function name

    ########  arg with no default values
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(post_plot(x_side = NULL))
    testthat::expect_error(post_plot(x_log_scale = NULL))
    testthat::expect_no_error(post_plot(x_categ = NULL))
    testthat::expect_no_error(post_plot(x_categ_pos = NULL))
    testthat::expect_error(post_plot(x_lab = NULL))
    testthat::expect_error(post_plot(x_axis_size = NULL))
    testthat::expect_error(post_plot(x_label_size = NULL))
    testthat::expect_error(post_plot(x_dist_legend = NULL))
    testthat::expect_error(post_plot(x_nb_inter_tick = NULL))
    testthat::expect_error(post_plot(y_side = NULL))
    testthat::expect_error(post_plot(y_log_scale = NULL))
    testthat::expect_no_error(post_plot(y_categ = NULL))
    testthat::expect_no_error(post_plot(y_categ_pos = NULL))
    testthat::expect_error(post_plot(y_lab = NULL))
    testthat::expect_error(post_plot(y_axis_size = NULL))
    testthat::expect_error(post_plot(y_label_size = NULL))
    testthat::expect_error(post_plot(y_dist_legend = NULL))
    testthat::expect_error(post_plot(y_nb_inter_tick = NULL))
    testthat::expect_error(post_plot(text_angle = NULL))
    testthat::expect_error(post_plot(tick_length = NULL))
    testthat::expect_error(post_plot(sec_tick_length = NULL))
    testthat::expect_no_error(post_plot(bg_color = NULL))
    testthat::expect_no_error(post_plot(grid_lwd = NULL))
    testthat::expect_error(post_plot(grid_col = NULL))
    testthat::expect_error(post_plot(corner_text = NULL))
    testthat::expect_error(post_plot(corner_text_size = NULL))
    testthat::expect_error(post_plot(just_label_add = NULL))
    testthat::expect_error(post_plot(par_reset = NULL))
    testthat::expect_no_error(post_plot(custom_par = NULL))
    testthat::expect_error(post_plot(safer_check = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(post_plot(x_categ = character()))
    testthat::expect_error(post_plot(x_categ_pos = double()))
    testthat::expect_error(post_plot(x_lab = character()))
    testthat::expect_error(post_plot(x_axis_size = double()))
    testthat::expect_error(post_plot(x_label_size = double()))
    testthat::expect_error(post_plot(x_dist_legend = double()))
    testthat::expect_error(post_plot(x_nb_inter_tick = integer()))
    testthat::expect_error(post_plot(y_categ = character()))
    testthat::expect_error(post_plot(y_categ_pos = double()))
    testthat::expect_error(post_plot(y_lab = character()))
    testthat::expect_error(post_plot(y_axis_size = double()))
    testthat::expect_error(post_plot(y_label_size = double()))
    testthat::expect_error(post_plot(y_dist_legend = double()))
    testthat::expect_error(post_plot(y_nb_inter_tick = integer()))
    testthat::expect_error(post_plot(text_angle = double()))
    testthat::expect_error(post_plot(tick_length = double()))
    testthat::expect_error(post_plot(sec_tick_length = double()))
    testthat::expect_error(post_plot(bg_color = character()))
    testthat::expect_error(post_plot(grid_lwd = double()))
    testthat::expect_error(post_plot(grid_col = character()))
    testthat::expect_error(post_plot(corner_text = character()))
    testthat::expect_error(post_plot(corner_text_size = double()))
    testthat::expect_error(post_plot(just_label_add = logical()))
    testthat::expect_error(post_plot(par_reset = logical()))
    testthat::expect_error(post_plot(custom_par = list()))
    testthat::expect_error(post_plot(safer_check = logical()))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(post_plot(x_side = NA))
    testthat::expect_error(post_plot(x_log_scale = NA))
    testthat::expect_error(post_plot(x_categ = NA))
    testthat::expect_error(post_plot(x_categ_pos = NA))
    testthat::expect_error(post_plot(x_lab = NA))
    testthat::expect_error(post_plot(x_axis_size = NA))
    testthat::expect_error(post_plot(x_label_size = NA))
    testthat::expect_error(post_plot(x_dist_legend = NA))
    testthat::expect_error(post_plot(x_nb_inter_tick = NA))
    testthat::expect_error(post_plot(y_side = NA))
    testthat::expect_error(post_plot(y_log_scale = NA))
    testthat::expect_error(post_plot(y_categ = NA))
    testthat::expect_error(post_plot(y_categ_pos = NA))
    testthat::expect_error(post_plot(y_lab = NA))
    testthat::expect_error(post_plot(y_axis_size = NA))
    testthat::expect_error(post_plot(y_label_size = NA))
    testthat::expect_error(post_plot(y_dist_legend = NA))
    testthat::expect_error(post_plot(y_nb_inter_tick = NA))
    testthat::expect_error(post_plot(text_angle = NA))
    testthat::expect_error(post_plot(tick_length = NA))
    testthat::expect_error(post_plot(sec_tick_length = NA))
    testthat::expect_error(post_plot(bg_color = NA))
    testthat::expect_error(post_plot(grid_lwd = NA))
    testthat::expect_error(post_plot(grid_col = NA))
    testthat::expect_error(post_plot(corner_text = NA))
    testthat::expect_error(post_plot(corner_text_size = NA))
    testthat::expect_error(post_plot(just_label_add = NA))
    testthat::expect_error(post_plot(par_reset = NA))
    testthat::expect_error(post_plot(custom_par = NA))
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
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
    testthat::expect_error(post_plot(lib_path = "PATH_NOT_GOOD", safer_check = FALSE))
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
    # x_side
    testthat::expect_error(post_plot(x_side = NULL))
    testthat::expect_error(post_plot(x_side = NA))
    testthat::expect_error(post_plot(x_side = "a"))
    testthat::expect_error(post_plot(x_side = c(1, 3)))
    testthat::expect_error(post_plot(x_side = mat1))
    testthat::expect_error(post_plot(x_side = factor1))
    testthat::expect_error(post_plot(x_side = expr1))
    testthat::expect_error(post_plot(x_side = fun1))
    testthat::expect_error(post_plot(x_side = 2)) # not in the allowed options c(0, 1, 3)
    testthat::expect_error(post_plot(x_side = -1)) # negative not allowed
    testthat::expect_error(post_plot(x_side = 1.5)) # not integer
    # end x_side
    # x_log_scale
    testthat::expect_error(post_plot(x_log_scale = NULL))
    testthat::expect_error(post_plot(x_log_scale = NA))
    testthat::expect_error(post_plot(x_log_scale = 1))
    testthat::expect_error(post_plot(x_log_scale = "TRUE"))
    testthat::expect_error(post_plot(x_log_scale = c(TRUE, FALSE)))
    testthat::expect_error(post_plot(x_log_scale = mat1))
    testthat::expect_error(post_plot(x_log_scale = factor1))
    testthat::expect_error(post_plot(x_log_scale = expr1))
    testthat::expect_error(post_plot(x_log_scale = fun1))
    # end x_log_scale
    # x_categ (checked only if non NULL)
    testthat::expect_no_error(post_plot(x_categ = vec1))
    testthat::expect_error(post_plot(x_categ = NA))
    testthat::expect_error(post_plot(x_categ = 1))
    testthat::expect_error(post_plot(x_categ = mat1))
    testthat::expect_error(post_plot(x_categ = factor1))
    testthat::expect_error(post_plot(x_categ = expr1))
    testthat::expect_error(post_plot(x_categ = fun1))
    # end x_categ
    # x_categ_pos (checked only if non NULL)
    testthat::expect_no_error(post_plot(x_categ = vec1, x_categ_pos = base::c(1, 2, 3)))
    testthat::expect_error(post_plot(x_categ_pos = NA))
    testthat::expect_error(post_plot(x_categ_pos = "a"))
    testthat::expect_error(post_plot(x_categ_pos = c(TRUE, FALSE)))
    testthat::expect_no_error(post_plot(x_categ_pos = mat1)) # matrix has mode "numeric", allowed by arg_check()
    testthat::expect_no_error(post_plot(x_categ_pos = factor1)) # factor has mode "numeric", allowed by arg_check()
    testthat::expect_error(post_plot(x_categ_pos = expr1))
    testthat::expect_error(post_plot(x_categ_pos = fun1))
    # end x_categ_pos
    # x_lab
    testthat::expect_error(post_plot(x_lab = NULL))
    testthat::expect_error(post_plot(x_lab = NA))
    testthat::expect_error(post_plot(x_lab = 1))
    testthat::expect_error(post_plot(x_lab = c("a", "b")))
    testthat::expect_error(post_plot(x_lab = mat1))
    testthat::expect_error(post_plot(x_lab = factor1))
    testthat::expect_error(post_plot(x_lab = expr1))
    testthat::expect_error(post_plot(x_lab = fun1))
    testthat::expect_no_error(post_plot(x_lab = "")) # "" allowed (can be empty)
    # end x_lab
    # x_axis_size
    testthat::expect_error(post_plot(x_axis_size = NULL))
    testthat::expect_error(post_plot(x_axis_size = NA))
    testthat::expect_error(post_plot(x_axis_size = "a"))
    testthat::expect_error(post_plot(x_axis_size = TRUE))
    testthat::expect_error(post_plot(x_axis_size = c(1.5, 1.5)))
    testthat::expect_error(post_plot(x_axis_size = mat1))
    testthat::expect_error(post_plot(x_axis_size = factor1))
    testthat::expect_error(post_plot(x_axis_size = expr1))
    testthat::expect_error(post_plot(x_axis_size = fun1))
    testthat::expect_error(post_plot(x_axis_size = -1)) # negative not allowed
    # end x_axis_size
    # x_label_size: same checks as x_axis_size
    testthat::expect_error(post_plot(x_label_size = -1))
    testthat::expect_error(post_plot(x_label_size = c(1.5, 1.5)))
    testthat::expect_error(post_plot(x_label_size = "a"))
    # end x_label_size
    # x_dist_legend
    testthat::expect_error(post_plot(x_dist_legend = -1)) # negative not allowed
    testthat::expect_error(post_plot(x_dist_legend = c(0.5, 0.5)))
    testthat::expect_error(post_plot(x_dist_legend = "a"))
    # end x_dist_legend
    # x_nb_inter_tick
    testthat::expect_error(post_plot(x_nb_inter_tick = 1.5)) # not integer
    testthat::expect_error(post_plot(x_nb_inter_tick = "a"))
    testthat::expect_error(post_plot(x_nb_inter_tick = -1)) # negative not allowed
    testthat::expect_error(post_plot(x_nb_inter_tick = c(1L, 2L)))
    testthat::expect_error(post_plot(x_nb_inter_tick = TRUE))
    testthat::expect_no_error(post_plot(x_nb_inter_tick = 0L))
    # end x_nb_inter_tick
    # y_side
    testthat::expect_error(post_plot(y_side = 1)) # not in the allowed options c(0, 2, 4)
    testthat::expect_error(post_plot(y_side = 3)) # not in the allowed options c(0, 2, 4)
    testthat::expect_error(post_plot(y_side = -2)) # negative not allowed
    testthat::expect_error(post_plot(y_side = 2.5)) # not integer
    testthat::expect_error(post_plot(y_side = "a"))
    testthat::expect_error(post_plot(y_side = c(2, 4)))
    # end y_side
    # y_log_scale: same as x_log_scale
    testthat::expect_error(post_plot(y_log_scale = 1))
    testthat::expect_error(post_plot(y_log_scale = "TRUE"))
    testthat::expect_error(post_plot(y_log_scale = c(TRUE, FALSE)))
    # end y_log_scale
    # y_categ (checked only if non NULL)
    testthat::expect_no_error(post_plot(y_categ = vec1, y_side = 2))
    testthat::expect_error(post_plot(y_categ = 1))
    testthat::expect_error(post_plot(y_categ = mat1))
    testthat::expect_error(post_plot(y_categ = factor1))
    testthat::expect_error(post_plot(y_categ = expr1))
    testthat::expect_error(post_plot(y_categ = fun1))
    # end y_categ
    # y_categ_pos (checked only if non NULL)
    testthat::expect_no_error(post_plot(y_categ = vec1, y_categ_pos = base::c(1, 2, 3), y_side = 2))
    testthat::expect_error(post_plot(y_categ_pos = "a"))
    testthat::expect_error(post_plot(y_categ_pos = c(TRUE, FALSE)))
    testthat::expect_no_error(post_plot(y_categ_pos = mat1)) # matrix has mode "numeric", allowed by arg_check()
    # end y_categ_pos
    # y_lab
    testthat::expect_error(post_plot(y_lab = NULL))
    testthat::expect_error(post_plot(y_lab = 1))
    testthat::expect_error(post_plot(y_lab = c("a", "b")))
    testthat::expect_no_error(post_plot(y_lab = "")) # "" allowed (can be empty)
    # end y_lab
    # y_axis_size
    testthat::expect_error(post_plot(y_axis_size = -1)) # negative not allowed
    testthat::expect_error(post_plot(y_axis_size = c(1.5, 1.5)))
    testthat::expect_error(post_plot(y_axis_size = "a"))
    # end y_axis_size
    # y_label_size: same as y_axis_size
    testthat::expect_error(post_plot(y_label_size = -1))
    testthat::expect_error(post_plot(y_label_size = c(1.5, 1.5)))
    # end y_label_size
    # y_dist_legend
    testthat::expect_error(post_plot(y_dist_legend = -1))
    testthat::expect_error(post_plot(y_dist_legend = c(0.5, 0.5)))
    # end y_dist_legend
    # y_nb_inter_tick: same as x_nb_inter_tick
    testthat::expect_error(post_plot(y_nb_inter_tick = 1.5))
    testthat::expect_error(post_plot(y_nb_inter_tick = -1))
    testthat::expect_no_error(post_plot(y_nb_inter_tick = 0L))
    # end y_nb_inter_tick
    # text_angle
    testthat::expect_error(post_plot(text_angle = "a"))
    testthat::expect_error(post_plot(text_angle = c(90, 90)))
    testthat::expect_error(post_plot(text_angle = mat1))
    testthat::expect_error(post_plot(text_angle = factor1))
    testthat::expect_error(post_plot(text_angle = expr1))
    testthat::expect_error(post_plot(text_angle = fun1))
    testthat::expect_error(post_plot(text_angle = -90)) # negative not allowed
    # end text_angle
    # tick_length
    testthat::expect_error(post_plot(tick_length = "a"))
    testthat::expect_error(post_plot(tick_length = c(0.5, 0.5)))
    testthat::expect_error(post_plot(tick_length = mat1))
    testthat::expect_error(post_plot(tick_length = -1)) # negative not allowed
    # end tick_length
    # sec_tick_length
    testthat::expect_error(post_plot(sec_tick_length = "a"))
    testthat::expect_error(post_plot(sec_tick_length = c(0.3, 0.3)))
    testthat::expect_error(post_plot(sec_tick_length = -1)) # negative not allowed
    # end sec_tick_length
    # bg_color (checked only if non NULL)
    testthat::expect_no_error(post_plot(bg_color = "white"))
    testthat::expect_no_error(post_plot(bg_color = "#FFFFFF"))
    testthat::expect_error(post_plot(bg_color = NA))
    testthat::expect_error(post_plot(bg_color = 1))
    testthat::expect_error(post_plot(bg_color = c("white", "black")))
    testthat::expect_error(post_plot(bg_color = "NOTACOLOR")) # not in grDevices::colors() and not hexadecimal
    testthat::expect_error(post_plot(bg_color = mat1))
    testthat::expect_error(post_plot(bg_color = factor1))
    testthat::expect_error(post_plot(bg_color = expr1))
    testthat::expect_error(post_plot(bg_color = fun1))
    # end bg_color
    # grid_lwd (checked only if non NULL)
    testthat::expect_no_error(post_plot(grid_lwd = 1))
    testthat::expect_error(post_plot(grid_lwd = NA))
    testthat::expect_error(post_plot(grid_lwd = "a"))
    testthat::expect_error(post_plot(grid_lwd = TRUE))
    testthat::expect_error(post_plot(grid_lwd = factor1))
    testthat::expect_error(post_plot(grid_lwd = expr1))
    testthat::expect_error(post_plot(grid_lwd = fun1))
    testthat::expect_error(post_plot(grid_lwd = -1)) # negative not allowed
    testthat::expect_no_error(post_plot(grid_lwd = base::c(1, 2)))
    # end grid_lwd
    # grid_col (checked only if non NULL)
    testthat::expect_no_error(post_plot(grid_lwd = 1, grid_col = "grey"))
    testthat::expect_error(post_plot(grid_lwd = 1, grid_col = NA))
    testthat::expect_error(post_plot(grid_lwd = 1, grid_col = 1))
    testthat::expect_error(post_plot(grid_lwd = 1, grid_col = c("grey", "white")))
    testthat::expect_error(post_plot(grid_lwd = 1, grid_col = "NOTACOLOR"))
    testthat::expect_error(post_plot(grid_lwd = 1, grid_col = mat1))
    testthat::expect_error(post_plot(grid_lwd = 1, grid_col = factor1))
    testthat::expect_error(post_plot(grid_lwd = 1, grid_col = expr1))
    testthat::expect_error(post_plot(grid_lwd = 1, grid_col = fun1))
    # end grid_col
    # corner_text
    testthat::expect_error(post_plot(corner_text = NULL))
    testthat::expect_error(post_plot(corner_text = NA))
    testthat::expect_error(post_plot(corner_text = 1))
    testthat::expect_error(post_plot(corner_text = c("a", "b")))
    testthat::expect_error(post_plot(corner_text = mat1))
    testthat::expect_error(post_plot(corner_text = factor1))
    testthat::expect_error(post_plot(corner_text = expr1))
    testthat::expect_error(post_plot(corner_text = fun1))
    testthat::expect_no_error(post_plot(corner_text = "")) # "" allowed (can be empty)
    # end corner_text
    # corner_text_size
    testthat::expect_error(post_plot(corner_text_size = -1))
    testthat::expect_error(post_plot(corner_text_size = c(1, 1)))
    testthat::expect_error(post_plot(corner_text_size = "a"))
    # end corner_text_size
    # just_label_add
    testthat::expect_error(post_plot(just_label_add = NULL))
    testthat::expect_error(post_plot(just_label_add = NA))
    testthat::expect_error(post_plot(just_label_add = 1))
    testthat::expect_error(post_plot(just_label_add = "TRUE"))
    testthat::expect_error(post_plot(just_label_add = c(TRUE, FALSE)))
    testthat::expect_error(post_plot(just_label_add = mat1))
    testthat::expect_error(post_plot(just_label_add = factor1))
    testthat::expect_error(post_plot(just_label_add = expr1))
    testthat::expect_error(post_plot(just_label_add = fun1))
    # end just_label_add
    # par_reset
    testthat::expect_error(post_plot(par_reset = NULL))
    testthat::expect_error(post_plot(par_reset = NA))
    testthat::expect_error(post_plot(par_reset = 1))
    testthat::expect_error(post_plot(par_reset = "TRUE"))
    testthat::expect_error(post_plot(par_reset = c(TRUE, FALSE)))
    testthat::expect_error(post_plot(par_reset = mat1))
    testthat::expect_error(post_plot(par_reset = factor1))
    testthat::expect_error(post_plot(par_reset = expr1))
    testthat::expect_error(post_plot(par_reset = fun1))
    # end par_reset
    # custom_par (checked only if non NULL)
    testthat::expect_error(post_plot(custom_par = NA))
    testthat::expect_error(post_plot(custom_par = "a"))
    testthat::expect_error(post_plot(custom_par = c(1, 2)))
    testthat::expect_error(post_plot(custom_par = mat1))
    testthat::expect_error(post_plot(custom_par = factor1))
    testthat::expect_error(post_plot(custom_par = expr1))
    testthat::expect_error(post_plot(custom_par = fun1))
    # end custom_par
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(post_plot(x_categ = c("", "B")))
    testthat::expect_error(post_plot(y_categ = c("", "B")))
    testthat::expect_error(post_plot(bg_color = ""))
    testthat::expect_error(post_plot(grid_lwd = 1, grid_col = ""))
    # x_lab, y_lab and corner_text can contain "" (inactivated in tempo_arg)
    testthat::expect_no_error(post_plot(x_lab = "", y_lab = "", corner_text = ""))
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
    testthat::expect_error(post_plot(x_side = 1, x_categ = vec1, x_categ_pos = base::c(1, 2))) # x_categ_pos length != x_categ length (checked in main code, only when x_side != 0)
    testthat::expect_error(post_plot(y_categ = vec1, y_categ_pos = base::c(1, 2), y_side = 2)) # y_categ_pos length != y_categ length (checked in main code)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # numeric axis, bottom
    graphics::plot(x = 1:3, y = 1:3)
    result <- post_plot(x_side = 1, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result))
    testthat::expect_true(object = base::length(x = result) > 20)
    testthat::expect_true(object = base::is.numeric(x = result$x_mid_left_dev_region))
    testthat::expect_null(object = result$text)
    # numeric axis, top with inter ticks
    result2 <- post_plot(x_side = 3, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = 2L, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result2))
    # log scale x
    graphics::plot(x = 1:3, y = 1:3, log = "x")
    result3 <- post_plot(x_side = 1, x_log_scale = TRUE, x_categ = NULL, x_categ_pos = NULL, x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result3))
    # numeric y axis, left, with inter ticks
    result4 <- post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 2, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "y", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = 2L, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result4))
    # log scale y, right side
    graphics::plot(x = 1:3, y = 1:3, log = "y")
    result5 <- post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 4, y_log_scale = TRUE, y_categ = NULL, y_categ_pos = NULL, y_lab = "y", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result5))
    # x_categ
    graphics::plot(x = 1:3, y = 1:3)
    result6 <- post_plot(x_side = 1, x_log_scale = FALSE, x_categ = vec1, x_categ_pos = NULL, x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result6))
    # x_categ with x_side = 3 (top axis)
    graphics::plot(x = 1:3, y = 1:3)
    result6b <- post_plot(x_side = 3, x_log_scale = FALSE, x_categ = vec1, x_categ_pos = base::c(1, 2, 3), x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result6b))
    # y_categ, side 4
    graphics::plot(x = 1:3, y = 1:3)
    result7 <- post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 4, y_log_scale = FALSE, y_categ = vec1, y_categ_pos = base::c(1, 2, 3), y_lab = "y", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result7))
    # bg_color, grid_lwd, grid_col
    graphics::plot(x = 1:3, y = 1:3)
    result8 <- post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = "grey", grid_lwd = 1, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result8))
    # just_label_add
    result9 <- post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "y", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = TRUE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result9))
    # par_reset = TRUE and custom_par non NULL
    graphics::plot(x = 1:3, y = 1:3)
    result10 <- post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = TRUE, custom_par = list1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::grepl(x = result10$text[1], pattern = "GRAPH PARAMETERS SET TO VALUES DEFINED BY custom_par ARGUMENT", fixed = TRUE))
    # par_reset = TRUE and custom_par = NULL
    graphics::plot(x = 1:3, y = 1:3)
    result11 <- post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = TRUE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::grepl(x = result11$text[1], pattern = "GRAPH PARAMETERS RESET TO graphics::par() DEFAULT VALUES", fixed = TRUE))
    # wrong x_categ_pos length
    testthat::expect_error(post_plot(x_side = 1, x_log_scale = FALSE, x_categ = vec1, x_categ_pos = base::c(1, 2), x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # wrong y_categ_pos length
    testthat::expect_error(post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 2, y_log_scale = FALSE, y_categ = vec1, y_categ_pos = base::c(1, 2), y_lab = "y", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "c", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # log scale with xaxp or yaxp containing 0 (main code lines replacing 0 by 10^-30): forced with graphics::par(xaxp = c(0, 1, 3)) on a log window, because R never sets 0 alone when plotting (the 4% extension makes usr[1] < 0, and xaxp limits are then 10^usr, which is 0.1 and not 0)
    graphics::plot.new()
    graphics::plot.window(xlim = c(1, 10), ylim = c(1, 10), log = "x")
    graphics::par(xaxp = c(0, 1, 3))
    result14 <- post_plot(x_side = 1, x_log_scale = TRUE, x_categ = NULL, x_categ_pos = NULL, x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result14))
    graphics::plot.new()
    graphics::plot.window(xlim = c(1, 10), ylim = c(1, 10), log = "y")
    graphics::par(yaxp = c(0, 1, 3))
    result15 <- post_plot(x_side = 0, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 2, y_log_scale = TRUE, y_categ = NULL, y_categ_pos = NULL, y_lab = "y", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "", corner_text_size = 1, just_label_add = FALSE, par_reset = FALSE, custom_par = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.list(x = result15))
    # custom_par with wrong names and par_reset = TRUE (main code error)
    tempo_par_err <- base::tryCatch(
        expr = post_plot(x_side = 1, x_log_scale = FALSE, x_categ = NULL, x_categ_pos = NULL, x_lab = "x", x_axis_size = num1, x_label_size = num1, x_dist_legend = num2, x_nb_inter_tick = int1, y_side = 0, y_log_scale = FALSE, y_categ = NULL, y_categ_pos = NULL, y_lab = "", y_axis_size = num1, y_label_size = num1, y_dist_legend = num2, y_nb_inter_tick = int1, text_angle = 90, tick_length = num2, sec_tick_length = 0.3, bg_color = NULL, grid_lwd = NULL, grid_col = "white", corner_text = "", corner_text_size = 1, just_label_add = FALSE, par_reset = TRUE, custom_par = base::list(NOTGOOD = 1), safer_check = TRUE, lib_path = NULL, error_text = ""),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = tempo_par_err, pattern = "custom_par ARGUMENT SHOULD HAVE THE NAMES OF THE COMPARTMENT LIST COMING FROM THE graphics::par() LIST", fixed = TRUE))
    ## end other tests
    grDevices::dev.off(which = grDevices::dev.cur()) # close the pdf device opened at the beginning
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
