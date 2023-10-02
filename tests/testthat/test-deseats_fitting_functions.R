test_that("deseats fitting functions return correct output", {
  expect_error(deseats(as.character(1:300)))
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
  
  expect_no_error(deseats(log(EXPENDITURES), set_options(order_poly = 1)))   
  expect_no_error(deseats(log(EXPENDITURES), set_options(order_poly = 3)))
  expect_no_error(deseats(log(EXPENDITURES), set_options(season = 1))) 
  expect_no_error(deseats(log(EXPENDITURES), set_options(season = 4))) 
  expect_no_error(deseats(log(EXPENDITURES), set_options(season = NA_real_))) 
  expect_no_error(deseats(log(EXPENDITURES), set_options(bwidth = 0.05))) 
  expect_no_error(deseats(log(EXPENDITURES), set_options(bwidth = 0.45))) 
  expect_no_error(deseats(log(EXPENDITURES), set_options(bwidth = NA_real_)))   
  
  expect_s4_class(deseats(log(EXPENDITURES)), "deseats")
  expect_s3_class(deseats(log(EXPENDITURES))@decomp, "mts")
  
  
})


