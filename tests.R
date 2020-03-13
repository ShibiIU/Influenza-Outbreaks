
### Test file ###

library(testthat)
# import::here(data_generating_function, ABC_SMC_sample, 
#              generate_abc_sample, generate_abc_sample2, .from = 'S610FinalProject.R')

source('ABC_FluOutbreak.R')
context("Check the functions for ABC-SMC")


# use a simple data for testing
mat = matrix(c(15,12,4,
               11,17,4,
               0, 21,4,
               0, 0, 5), byrow = TRUE, nrow = 4, ncol = 3)

mat = sweep(mat, 2, colSums(mat), "/")


q_comm = 0.7 
q_home = 0.4


# --- tests --- #


# test the data_generating_function

test_that("data_generating_function works on the testing data", {
    W = data_generating_function(mat, q_comm, q_home)
    
    # check output matrix has the correct dimensions
    expect_equal(dim(W), dim(mat))
    
    # check all elements in the output matrix are between 0 and 1
    expect_true(all(W >= 0) & all(W <= 1))
    
    # check the first row of the output matrix: w_0s = qc^s
    expect_equal(W[1, ], q_comm^seq(1: ncol(mat)))
    
    # check the lower trangle the output matrix are all zero
    expect_true(all(W[3:nrow(W), 1] == 0))
    expect_true(all(W[4:nrow(W), 2] == 0))
})



# test the ABC_SMC_sample function
test_that("ABC_SMC_sample works on the testing data", {
    q=ABC_SMC_sample(mat)
    
    # check output has the correct length, N=5000 and two parameters
    expect_equal(length(q), 10000)
    
    # check all elements of the output (probability) are between 0 and 1
    expect_true(all(q >= 0) & all(q <= 1))
})



