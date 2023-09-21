test_that("'bwidthAbsCpp' calculates the absolute bandwidth correctly based on a given relative bandwidth", {
  expect_equal(bwidthAbsCpp(10, 0.5111), trunc(10 * 0.5111 + 0.5)) 
  expect_equal(bwidthAbsCpp(11, 0.5111), trunc(11 * 0.5111 + 0.5))  
  expect_equal(bwidthAbsCpp(12, 0.5111), trunc(12 * 0.5111 + 0.5))  
  expect_equal(bwidthAbsCpp(13, 0.5111), trunc(13 * 0.5111 + 0.5))  
  expect_equal(bwidthAbsCpp(14, 0.5111), trunc(14 * 0.5111 + 0.5))    
})

test_that("'bwidthTotalCpp' calculates the total absolute bandwidth correctly", {
  expect_equal(bwidthTotalCpp(10), 2L * 10L + 1L)
  expect_equal(bwidthTotalCpp(11), 2L * 11L + 1L)
  expect_equal(bwidthTotalCpp(12), 2L * 12L + 1L)
  expect_equal(bwidthTotalCpp(13), 2L * 13L + 1L)
  expect_equal(bwidthTotalCpp(14), 2L * 14L + 1L)  
})

test_that("'setup_wsMat_single_Cpp' initializes the weighting system matrices correctly", {
  expect_equal(setup_wsMat_single_Cpp(10), matrix(0, ncol = 10, nrow = 10))
  expect_equal(setup_wsMat_single_Cpp(11), matrix(0, ncol = 11, nrow = 11))
  expect_equal(setup_wsMat_single_Cpp(12), matrix(0, ncol = 12, nrow = 12))
  expect_equal(setup_wsMat_single_Cpp(13), matrix(0, ncol = 13, nrow = 13))
  expect_equal(setup_wsMat_single_Cpp(14), matrix(0, ncol = 14, nrow = 14))  
})

test_that("'setup_wk_Cpp' initializes the weighting row vector correctly", {
  expect_equal(setup_wk_Cpp(10), matrix(0, nrow = 1, ncol = 10))
  expect_equal(setup_wk_Cpp(11), matrix(0, nrow = 1, ncol = 11))
  expect_equal(setup_wk_Cpp(12), matrix(0, nrow = 1, ncol = 12))
  expect_equal(setup_wk_Cpp(13), matrix(0, nrow = 1, ncol = 13))
  expect_equal(setup_wk_Cpp(14), matrix(0, nrow = 1, ncol = 14))  
})

test_that("'setup_xt_Cpp' initializes the x-data matrix correctly", {
  expect_equal(setup_xt_Cpp(1, 4, 11), matrix(0, nrow = 1 + 4, ncol = 11))
  expect_equal(setup_xt_Cpp(1, 12, 11), matrix(0, nrow = 1 + 12, ncol = 11)) 
  
  expect_equal(setup_xt_Cpp(3, 4, 11), matrix(0, nrow = 3 + 4, ncol = 11))
  expect_equal(setup_xt_Cpp(3, 12, 11), matrix(0, nrow = 3 + 12, ncol = 11))  
  
  expect_equal(setup_xt_Cpp(1, 4, 101), matrix(0, nrow = 1 + 4, ncol = 101))
  expect_equal(setup_xt_Cpp(1, 12, 101), matrix(0, nrow = 1 + 12, ncol = 101)) 
  
  expect_equal(setup_xt_Cpp(3, 4, 101), matrix(0, nrow = 3 + 4, ncol = 101))
  expect_equal(setup_xt_Cpp(3, 12, 101), matrix(0, nrow = 3 + 12, ncol = 101))   
  
})

test_that("'lowIndexCpp' creates the collection of lower indexes correctly", {
  expect_equal(lowIndexCpp(10, 0:10), matrix(10 - (0:10), ncol = 1))
  expect_equal(lowIndexCpp(11, 0:11), matrix(11 - (0:11), ncol = 1))   
  expect_equal(lowIndexCpp(12, 0:12), matrix(12 - (0:12), ncol = 1))   
  expect_equal(lowIndexCpp(13, 0:13), matrix(13 - (0:13), ncol = 1))   
  expect_equal(lowIndexCpp(14, 0:14), matrix(14 - (0:14), ncol = 1))     
})

test_that("'upIndexCpp' creates the collection of upper indexes correctly", {
  expect_equal(upIndexCpp(10, 20:10), matrix(10 + (20:10), ncol = 1))
  expect_equal(upIndexCpp(11, 19:9), matrix(11 + (19:9), ncol = 1))   
  expect_equal(upIndexCpp(12, 18:8), matrix(12 + (18:8), ncol = 1))   
  expect_equal(upIndexCpp(13, 17:7), matrix(13 + (17:7), ncol = 1))   
  expect_equal(upIndexCpp(14, 16:6), matrix(14 + (16:6), ncol = 1))     
})

test_that("'bwidthRightCpp' creates the collection of right-hand side absolute bandwidths correctly", {
  expect_equal(bwidthRightCpp(10, 1, 10:0), matrix(10 + 1 * (10:0), ncol = 1))
  expect_equal(bwidthRightCpp(10, 0, 10:0), matrix(10 + 0 * (10:0), ncol = 1)) 
  
  expect_equal(bwidthRightCpp(101, 1, 101:0), matrix(101 + 1 * (101:0), ncol = 1))
  expect_equal(bwidthRightCpp(101, 0, 101:0), matrix(101 + 0 * (101:0), ncol = 1))   
})

test_that("'bwidthAllTotalCpp' creates the collection of all total absolute bandwidths correctly", {
  expect_equal(bwidthAllTotalCpp(0:10, bwidthRightCpp(10, 0, lowIndexCpp(10, 0:10))), matrix(10:20, ncol = 1, nrow = 11))
  expect_equal(bwidthAllTotalCpp(0:10, bwidthRightCpp(10, 1, lowIndexCpp(10, 0:10))), matrix(20, ncol = 1, nrow = 11))  
})

#====================================================#
p <- 1
s <- 4
bwidthAbs <- 12
bwidthRight_1st <- 24 # for bb = 1
all_tp <- (-bwidthAbs):bwidthRight_1st
ntpoints <- length(all_tp)

outCpp_a1 <- dataMatCpp(p, s, ntpoints, bwidthAbs, all_tp)

tp_scaled <- all_tp / bwidthAbs
testMat <- matrix(1, nrow = p + s, ncol = ntpoints)
for (i in 1:p) {
  testMat[i + 1, ] <- tp_scaled^i
}

l1 <- 2 * pi / s

for (i in 1:trunc((s - 1) / 2)) {
  testMat[p + 2 * i, ] <- cos(i * l1 * all_tp)
  testMat[p + 2 * i + 1, ] <- sin(i * l1 * all_tp)  
}
if((s / 2) == trunc(s / 2)) {
  testMat[p + s, ] <- cos(pi * all_tp)
}

testMat_b1 <- testMat

#===================================================#

p <- 3
s <- 4
bwidthAbs <- 12
bwidthRight_1st <- 24 # for bb = 1
all_tp <- (-bwidthAbs):bwidthRight_1st
ntpoints <- length(all_tp)

outCpp_a2 <- dataMatCpp(p, s, ntpoints, bwidthAbs, all_tp)

tp_scaled <- all_tp / bwidthAbs
testMat <- matrix(1, nrow = p + s, ncol = ntpoints)
for (i in 1:p) {
  testMat[i + 1, ] <- tp_scaled^i
}

l1 <- 2 * pi / s

for (i in 1:trunc((s - 1) / 2)) {
  testMat[p + 2 * i, ] <- cos(i * l1 * all_tp)
  testMat[p + 2 * i + 1, ] <- sin(i * l1 * all_tp)  
}
if((s / 2) == trunc(s / 2)) {
  testMat[p + s, ] <- cos(pi * all_tp)
}

testMat_b2 <- testMat

#============================================#

p <- 1
s <- 12
bwidthAbs <- 36
bwidthRight_1st <- 72 # for bb = 1
all_tp <- (-bwidthAbs):bwidthRight_1st
ntpoints <- length(all_tp)

outCpp_a3 <- dataMatCpp(p, s, ntpoints, bwidthAbs, all_tp)

tp_scaled <- all_tp / bwidthAbs
testMat <- matrix(1, nrow = p + s, ncol = ntpoints)
for (i in 1:p) {
  testMat[i + 1, ] <- tp_scaled^i
}

l1 <- 2 * pi / s

for (i in 1:trunc((s - 1) / 2)) {
  testMat[p + 2 * i, ] <- cos(i * l1 * all_tp)
  testMat[p + 2 * i + 1, ] <- sin(i * l1 * all_tp)  
}
if((s / 2) == trunc(s / 2)) {
  testMat[p + s, ] <- cos(pi * all_tp)
}

testMat_b3 <- testMat

#===========================================#

p <- 3
s <- 12
bwidthAbs <- 36
bwidthRight_1st <- 72 # for bb = 1
all_tp <- (-bwidthAbs):bwidthRight_1st
ntpoints <- length(all_tp)

outCpp_a4 <- dataMatCpp(p, s, ntpoints, bwidthAbs, all_tp)

tp_scaled <- all_tp / bwidthAbs
testMat <- matrix(1, nrow = p + s, ncol = ntpoints)
for (i in 1:p) {
  testMat[i + 1, ] <- tp_scaled^i
}

l1 <- 2 * pi / s

for (i in 1:trunc((s - 1) / 2)) {
  testMat[p + 2 * i, ] <- cos(i * l1 * all_tp)
  testMat[p + 2 * i + 1, ] <- sin(i * l1 * all_tp)  
}
if((s / 2) == trunc(s / 2)) {
  testMat[p + s, ] <- cos(pi * all_tp)
}

testMat_b4 <- testMat

#==========================================#

test_that("The x-matrix is being set up correctly with the polynomial and harmonic function values", {
  expect_equal(outCpp_a1, testMat_b1)
  expect_equal(outCpp_a2, testMat_b2)
  expect_equal(outCpp_a3, testMat_b3)
  expect_equal(outCpp_a4, testMat_b4)  
  
})

test_that("'derivSelectorCpp' has the correct element set to 1", {
  
  expect_equal(derivSelectorCpp(1, 4, 0), matrix(c(1, 0, 0, 0, 0), nrow = 1))
  expect_equal(derivSelectorCpp(1, 4, 1), matrix(c(0, 1, 0, 0, 0), nrow = 1))
  expect_equal(derivSelectorCpp(1, 4, 3), matrix(c(0, 0, 0, 1, 0), nrow = 1))
  
  expect_equal(derivSelectorCpp(1, 12, 0), matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1))
  expect_equal(derivSelectorCpp(1, 12, 1), matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1))
  expect_equal(derivSelectorCpp(1, 12, 3), matrix(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1))  
  
  expect_equal(derivSelectorCpp(3, 4, 0), matrix(c(1, 0, 0, 0, 0, 0, 0), nrow = 1))
  expect_equal(derivSelectorCpp(3, 4, 1), matrix(c(0, 1, 0, 0, 0, 0, 0), nrow = 1))
  expect_equal(derivSelectorCpp(3, 4, 3), matrix(c(0, 0, 0, 1, 0, 0, 0), nrow = 1))  
  
  expect_equal(derivSelectorCpp(3, 12, 0), matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1))
  expect_equal(derivSelectorCpp(3, 12, 1), matrix(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1))
  expect_equal(derivSelectorCpp(3, 12, 3), matrix(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1))   
  
})

test_that("'seasonSelectorCpp' has the correct elements set to 1", {
  
  expect_equal(seasonSelectorCpp(1, 4), matrix(c(0, 0, 1, 0, 1), nrow = 1))
  expect_equal(seasonSelectorCpp(3, 4), matrix(c(0, 0, 0, 0, 1, 0, 1), nrow = 1))
  
  expect_equal(seasonSelectorCpp(1, 12), matrix(c(0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1), nrow = 1))
  expect_equal(seasonSelectorCpp(3, 12), matrix(c(0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1), nrow = 1))  
  
})

test_that("The weighting function weights are obtained correctly", {
  
  expect_equal(wk_mu0_Cpp(5), matrix(1, nrow = 1, ncol = 5))
  expect_equal(wk_mu0_Cpp(10), matrix(1, nrow = 1, ncol = 10)) 
  
  expect_equal(wk_mu1_Cpp(-5:5, 6), matrix(1 - ((-5:5) / 6), nrow = 1))
  expect_equal(wk_mu1_Cpp(-10:10, 11), matrix(1 - ((-10:10) / 11), nrow = 1))  

  expect_equal(wk_mu23_Cpp(-5:5, 6, 2), matrix((1 - ((-5:5) / 6))^2, nrow = 1))
  expect_equal(wk_mu23_Cpp(-10:10, 11, 2), matrix((1 - ((-10:10) / 11))^2, nrow = 1)) 
  expect_equal(wk_mu23_Cpp(-5:5, 6, 3), matrix((1 - ((-5:5) / 6))^3, nrow = 1))
  expect_equal(wk_mu23_Cpp(-10:10, 11, 3), matrix((1 - ((-10:10) / 11))^3, nrow = 1))   

  expect_equal(wkAdjCpp(-5:5, 6, 0), wk_mu0_Cpp(11))
  expect_equal(wkAdjCpp(-10:10, 11, 0), wk_mu0_Cpp(21)) 
  
  expect_equal(wkAdjCpp(-5:5, 6, 1), wk_mu1_Cpp(-5:5, 6))
  expect_equal(wkAdjCpp(-10:10, 11, 1), wk_mu1_Cpp(-10:10, 11))   
  
  expect_equal(wkAdjCpp(-5:5, 6, 2), wk_mu23_Cpp(-5:5, 6, 2))
  expect_equal(wkAdjCpp(-10:10, 11, 2), wk_mu23_Cpp(-10:10, 11, 2)) 
  
  expect_equal(wkAdjCpp(-5:5, 6, 3), wk_mu23_Cpp(-5:5, 6, 3))
  expect_equal(wkAdjCpp(-10:10, 11, 3), wk_mu23_Cpp(-10:10, 11, 3))   
  
})


