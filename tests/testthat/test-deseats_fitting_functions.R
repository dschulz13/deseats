test_that("deseats fitting functions return correct output", {
  
  expect_error(deseats(as.character(1:300)))
  expect_no_error(deseats(c(log(EXPENDITURES))))
  
  expect_error(deseats(log(EXPENDITURES), set_options(order_poly = 2)), "@order_poly must be of length 1 and set to either 1 or 3")
  expect_error(deseats(log(EXPENDITURES), set_options(order_poly = 5)), "@order_poly must be of length 1 and set to either 1 or 3") 
  
  expect_error(deseats(log(EXPENDITURES), set_options(kernel_fun = "else")), '@kernel_fun must be of length 1 and set to either "uniform", "epanechnikov", "bisquare" or "triweight') 
  expect_error(deseats(log(EXPENDITURES), set_options(kernel_fun = c("uniform", "epanechnikov"))), '@kernel_fun must be of length 1 and set to either "uniform", "epanechnikov", "bisquare" or "triweight') 
  expect_error(deseats(log(EXPENDITURES), set_options(kernel_fun = c("uniform", "epanechnikov"))), '@kernel_fun must be of length 1 and set to either "uniform", "epanechnikov", "bisquare" or "triweight') 
  expect_error(deseats(log(EXPENDITURES), set_options(kernel_fun = c("uniform", "epanechnikov"))), '@kernel_fun must be of length 1 and set to either "uniform", "epanechnikov", "bisquare" or "triweight') 
  
  expect_error(deseats(log(EXPENDITURES), set_options(season = c("1"))), 'invalid object for slot "season" in class "smoothing_options": got class "character", should be or extend class "numeric"') 
  expect_error(deseats(log(EXPENDITURES), set_options(season = c(4, 7))), '@season must be of length 1 and either NA or a numeric value') 
  
  expect_error(deseats(log(EXPENDITURES), set_options(bwidth = "1")), 'invalid object for slot "bwidth" in class "smoothing_options": got class "character", should be or extend class "numeric"') 
  expect_error(deseats(log(EXPENDITURES), set_options(bwidth = c(0.2, 0.3))), '@bwidth must be of length 1 and must be either set to NA or to a numeric value between 0 and 0.5')  
  expect_error(deseats(log(EXPENDITURES), set_options(bwidth = c(0.6))), '@bwidth must be of length 1 and must be either set to NA or to a numeric value between 0 and 0.5')  
  expect_error(deseats(log(EXPENDITURES), set_options(bwidth = c(-0.1))), '@bwidth must be of length 1 and must be either set to NA or to a numeric value between 0 and 0.5')    
  expect_error(deseats(log(EXPENDITURES), set_options(bwidth = 0.45)), "the use of correction_factor = TRUE inflates the bandwidth too strongly; try correction_factor = FALSE or use a smaller bandwidth")
  
  expect_error(deseats(log(EXPENDITURES), set_options(boundary_method = 1)), 'invalid object for slot "boundary_method" in class "smoothing_options": got class "numeric", should be or extend class "character"')
  expect_error(deseats(log(EXPENDITURES), set_options(boundary_method = c("shorten", "extend"))), "@boundary_method must be of length 1")
  
  expect_error(deseats(log(EXPENDITURES), inflation_rate = 2), 'inflation_rate must be either "optimal" or "naive"')
  expect_error(deseats(log(EXPENDITURES), inflation_rate = c("naive", "optimal")), 'inflation_rate must be either "optimal" or "naive"')
  
  expect_error(deseats(log(EXPENDITURES), correction_factor = 3), 'correction_factor must be either TRUE or FALSE')
  expect_error(deseats(log(EXPENDITURES), correction_factor = c(TRUE, FALSE)), 'correction_factor must be either TRUE or FALSE')
  
  expect_error(deseats(log(EXPENDITURES), autocor = 3), 'autocor must be either TRUE or FALSE')
  expect_error(deseats(log(EXPENDITURES), autocor = c(FALSE, TRUE)), 'autocor must be either TRUE or FALSE')  
  
  expect_error(deseats(log(EXPENDITURES), drop = 0.6), 'drop must be a single numeric value between 0 and 0.25 or NULL')  
  expect_error(deseats(log(EXPENDITURES), drop = c(0.1, 0.15)), 'drop must be a single numeric value between 0 and 0.25 or NULL')  
  
  expect_error(deseats(log(EXPENDITURES), error_model = 1), 'error_model must be either "free" or "ARMA"')
  expect_error(deseats(log(EXPENDITURES), error_model = c("ARMA", "free")), 'error_model must be either "free" or "ARMA"')  
  
  
  expect_no_error(deseats(log(EXPENDITURES), set_options(order_poly = 1)))   
  expect_no_error(deseats(log(EXPENDITURES), set_options(order_poly = 3)))
  
  expect_no_error(deseats(log(EXPENDITURES), set_options(season = 1))) 
  expect_no_error(deseats(log(EXPENDITURES), set_options(season = 4))) 
  expect_no_error(deseats(log(EXPENDITURES), set_options(season = NA_real_))) 
  
  expect_no_error(deseats(log(EXPENDITURES), set_options(bwidth = 0.01))) 
  expect_no_error(deseats(log(EXPENDITURES), set_options(bwidth = 0.3))) 
  expect_no_error(deseats(log(EXPENDITURES), set_options(bwidth = 0.49), correction_factor = FALSE))  
  expect_no_error(deseats(log(EXPENDITURES), set_options(bwidth = NA_real_)))
  
  expect_no_error(deseats(log(EXPENDITURES), set_options(boundary_method = "extend")))
  expect_no_error(deseats(log(EXPENDITURES), set_options(boundary_method = "shorten")))  

  expect_no_error(deseats(log(EXPENDITURES)))
  expect_equal(deseats(log(EXPENDITURES)), deseats(log(EXPENDITURES), inflation_rate = "optimal"))
  expect_equal(deseats(log(EXPENDITURES), set_options(order_poly = 3)), deseats(log(EXPENDITURES), set_options(order_poly = 3), inflation_rate = "naive"))
  expect_no_error(deseats(log(EXPENDITURES), inflation_rate = "naive"))
  expect_no_error(deseats(log(EXPENDITURES), set_options(order_poly = 3), inflation_rate = "optimal"))   
  
  expect_no_error(deseats(log(EXPENDITURES), correction_factor = TRUE))
  expect_no_error(deseats(log(EXPENDITURES), correction_factor = FALSE))
  expect_equal(deseats(log(EXPENDITURES), correction_factor = TRUE, error_model = "ARMA"), deseats(log(EXPENDITURES), correction_factor = FALSE, error_model = "ARMA"))
  
  expect_no_error(deseats(log(EXPENDITURES), autocor = TRUE))
  expect_no_error(deseats(log(EXPENDITURES), autocor = FALSE))
  expect_no_error(deseats(log(EXPENDITURES), autocor = FALSE, correction_factor = FALSE))
  expect_no_match(as.character(deseats(log(EXPENDITURES), autocor = FALSE)@bwidth), as.character(deseats(log(EXPENDITURES), autocor = FALSE, correction_factor = FALSE)@bwidth))  
  
  expect_no_error(deseats(log(EXPENDITURES), drop = 0.1))
  expect_no_error(deseats(log(EXPENDITURES), drop = 0.25))
  expect_no_error(deseats(log(EXPENDITURES), set_options(order_poly = 3), drop = 0.1))
  expect_no_error(deseats(log(EXPENDITURES), set_options(order_poly = 3), drop = 0.25))
  expect_equal(deseats(log(EXPENDITURES)),  deseats(log(EXPENDITURES), drop = 0.05))
  expect_equal(deseats(log(EXPENDITURES), set_options(order_poly = 3)),  deseats(log(EXPENDITURES), set_options(order_poly = 3), drop = 0.1))  
  
  expect_no_error(deseats(log(EXPENDITURES), error_model = c("free", "ARMA")))  
  expect_no_error(deseats(log(EXPENDITURES), error_model = "ARMA"))
  expect_no_error(deseats(log(EXPENDITURES), error_model = "free"))
  expect_no_match(as.character(deseats(log(EXPENDITURES), error_model = "free")@bwidth), as.character(deseats(log(EXPENDITURES), error_model = "ARMA")@bwidth))
  
  expect_s4_class(deseats(log(EXPENDITURES)), "deseats")
  expect_s3_class(deseats(log(EXPENDITURES))@decomp, "mts")
  
  
})

test_that("select_bwidth function returns correct output", {
  expect_error(select_bwidth(as.character(1:300)))
  expect_no_error(select_bwidth(c(log(EXPENDITURES))))
  
  expect_error(select_bwidth(log(EXPENDITURES), set_options(order_poly = 2)), "@order_poly must be of length 1 and set to either 1 or 3")
  expect_error(select_bwidth(log(EXPENDITURES), set_options(order_poly = 5)), "@order_poly must be of length 1 and set to either 1 or 3") 
  
  expect_error(select_bwidth(log(EXPENDITURES), set_options(kernel_fun = "else")), '@kernel_fun must be of length 1 and set to either "uniform", "epanechnikov", "bisquare" or "triweight') 
  expect_error(select_bwidth(log(EXPENDITURES), set_options(kernel_fun = c("uniform", "epanechnikov"))), '@kernel_fun must be of length 1 and set to either "uniform", "epanechnikov", "bisquare" or "triweight') 
  expect_error(select_bwidth(log(EXPENDITURES), set_options(kernel_fun = c("uniform", "epanechnikov"))), '@kernel_fun must be of length 1 and set to either "uniform", "epanechnikov", "bisquare" or "triweight') 
  expect_error(select_bwidth(log(EXPENDITURES), set_options(kernel_fun = c("uniform", "epanechnikov"))), '@kernel_fun must be of length 1 and set to either "uniform", "epanechnikov", "bisquare" or "triweight') 
  
  expect_error(select_bwidth(log(EXPENDITURES), set_options(season = c("1"))), 'invalid object for slot "season" in class "smoothing_options": got class "character", should be or extend class "numeric"') 
  expect_error(select_bwidth(log(EXPENDITURES), set_options(season = c(4, 7))), '@season must be of length 1 and either NA or a numeric value') 
  
  expect_error(select_bwidth(log(EXPENDITURES), set_options(bwidth = "1")), 'invalid object for slot "bwidth" in class "smoothing_options": got class "character", should be or extend class "numeric"') 
  expect_error(select_bwidth(log(EXPENDITURES), set_options(bwidth = c(0.2, 0.3))), '@bwidth must be of length 1 and must be either set to NA or to a numeric value between 0 and 0.5')  
  expect_error(select_bwidth(log(EXPENDITURES), set_options(bwidth = c(0.6))), '@bwidth must be of length 1 and must be either set to NA or to a numeric value between 0 and 0.5')  
  expect_error(select_bwidth(log(EXPENDITURES), set_options(bwidth = c(-0.1))), '@bwidth must be of length 1 and must be either set to NA or to a numeric value between 0 and 0.5')    

  expect_error(select_bwidth(log(EXPENDITURES), set_options(boundary_method = 1)), 'invalid object for slot "boundary_method" in class "smoothing_options": got class "numeric", should be or extend class "character"')
  expect_error(select_bwidth(log(EXPENDITURES), set_options(boundary_method = c("shorten", "extend"))), "@boundary_method must be of length 1")
  
  expect_error(select_bwidth(log(EXPENDITURES), inflation_rate = 2), 'inflation_rate must be either "optimal" or "naive"')
  expect_error(select_bwidth(log(EXPENDITURES), inflation_rate = c("naive", "optimal")), 'inflation_rate must be either "optimal" or "naive"')
  
  expect_error(select_bwidth(log(EXPENDITURES), correction_factor = 3), 'correction_factor must be either TRUE or FALSE')
  expect_error(select_bwidth(log(EXPENDITURES), correction_factor = c(TRUE, FALSE)), 'correction_factor must be either TRUE or FALSE')
  
  expect_error(select_bwidth(log(EXPENDITURES), autocor = 3), 'autocor must be either TRUE or FALSE')
  expect_error(select_bwidth(log(EXPENDITURES), autocor = c(FALSE, TRUE)), 'autocor must be either TRUE or FALSE')  
  
  expect_error(select_bwidth(log(EXPENDITURES), drop = 0.6), 'drop must be a single numeric value between 0 and 0.25 or NULL')  
  expect_error(select_bwidth(log(EXPENDITURES), drop = c(0.1, 0.15)), 'drop must be a single numeric value between 0 and 0.25 or NULL')  
  
  expect_error(select_bwidth(log(EXPENDITURES), error_model = 1), 'error_model must be either "free" or "ARMA"')
  expect_error(select_bwidth(log(EXPENDITURES), error_model = c("ARMA", "free")), 'error_model must be either "free" or "ARMA"')  
  
  
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(order_poly = 1)))   
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(order_poly = 3)))
  
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(season = 1))) 
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(season = 4))) 
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(season = NA_real_))) 
  
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(bwidth = 0.01))) 
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(bwidth = 0.3))) 
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(bwidth = 0.49), correction_factor = FALSE))  
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(bwidth = NA_real_)))
  
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(boundary_method = "extend")))
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(boundary_method = "shorten")))  

  expect_no_error(select_bwidth(log(EXPENDITURES)))
  expect_equal(select_bwidth(log(EXPENDITURES)), select_bwidth(log(EXPENDITURES), inflation_rate = "optimal"))
  expect_equal(select_bwidth(log(EXPENDITURES), set_options(order_poly = 3)), select_bwidth(log(EXPENDITURES), set_options(order_poly = 3), inflation_rate = "naive"))
  expect_no_error(select_bwidth(log(EXPENDITURES), inflation_rate = "naive"))
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(order_poly = 3), inflation_rate = "optimal"))   
  
  expect_no_error(select_bwidth(log(EXPENDITURES), correction_factor = TRUE))
  expect_no_error(select_bwidth(log(EXPENDITURES), correction_factor = FALSE))
  expect_equal(select_bwidth(log(EXPENDITURES), correction_factor = TRUE, error_model = "ARMA"), select_bwidth(log(EXPENDITURES), correction_factor = FALSE, error_model = "ARMA"))
  
  expect_no_error(select_bwidth(log(EXPENDITURES), autocor = TRUE))
  expect_no_error(select_bwidth(log(EXPENDITURES), autocor = FALSE))
  expect_no_error(select_bwidth(log(EXPENDITURES), autocor = FALSE, correction_factor = FALSE))
  expect_no_match(as.character(select_bwidth(log(EXPENDITURES), autocor = FALSE)$bopt), as.character(select_bwidth(log(EXPENDITURES), autocor = FALSE, correction_factor = FALSE)$bopt))  
  
  expect_no_error(select_bwidth(log(EXPENDITURES), drop = 0.1))
  expect_no_error(select_bwidth(log(EXPENDITURES), drop = 0.25))
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(order_poly = 3), drop = 0.1))
  expect_no_error(select_bwidth(log(EXPENDITURES), set_options(order_poly = 3), drop = 0.25))
  expect_equal(select_bwidth(log(EXPENDITURES)),  select_bwidth(log(EXPENDITURES), drop = 0.05))
  expect_equal(select_bwidth(log(EXPENDITURES), set_options(order_poly = 3)),  select_bwidth(log(EXPENDITURES), set_options(order_poly = 3), drop = 0.1))  
  
  expect_no_error(select_bwidth(log(EXPENDITURES), error_model = c("free", "ARMA")))  
  expect_no_error(select_bwidth(log(EXPENDITURES), error_model = "ARMA"))
  expect_no_error(select_bwidth(log(EXPENDITURES), error_model = "free"))
  expect_no_match(as.character(select_bwidth(log(EXPENDITURES), error_model = "free")$bopt), as.character(select_bwidth(log(EXPENDITURES), error_model = "ARMA")$bopt))
  
})

test_that("BV4.1 fitting functions return correct output", {
  expect_error(BV4.1(log(EXPENDITURES), type = 1), 'type muste be either NULL, "quarterly" or "monthly"')
  expect_error(BV4.1(log(EXPENDITURES), type = c("quarterly", "monthly")), 'type muste be either NULL, "quarterly" or "monthly"')  
  expect_error(BV4.1(c(log(EXPENDITURES)), type = NULL), 'The procedure cannot be applied to data with frequency 1.')  
  expect_no_error(BV4.1(c(log(EXPENDITURES)), type = "quarterly"))
})
