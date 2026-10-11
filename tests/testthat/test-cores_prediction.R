test_that("Ensure that number of cores is handled correctly in bootstrap", {
  est <- s_semiarma(log(GDP))
  # Sets cores automatically to 1 if entered as < 1 and returns message
  expect_message({    
    fc <- predict(est, method = "boot", ncores = 0, progress_bar = FALSE)
  }, '"ncores" was < 1. Automatically set the value to 1.')
  # Should not produce an error for ncores < 1
  testthat::expect_no_error({   
    fc <- predict(est, method = "boot", ncores = 0, progress_bar = FALSE)
  })
  # No printing of message when ncores is in acceptable range
  testthat::expect_no_message({   
    fc <- predict(est, method = "boot", ncores = 1, progress_bar = FALSE)
  })  
  # No printing of message when ncores is in acceptable range (check default)
  testthat::expect_no_message({   
    fc <- predict(est, method = "boot", progress_bar = FALSE)
  })
  # Check that there are no errors for ncores in acceptable range
  testthat::expect_no_error({   
    fc <- predict(est, method = "boot", ncores = 1, progress_bar = FALSE)
  })  
  testthat::expect_no_error({   
    fc <- predict(est, method = "boot", progress_bar = FALSE)
  })      
})
