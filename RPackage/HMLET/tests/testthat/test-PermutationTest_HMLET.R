library(reshape)

inRead = read.csv("Output_PermutationTestDataPrep.csv")
input_df = inRead[c("testName","ID","trial","timepoint","condition","AOI")]

outRead = read.csv("Output_PermutationTest.csv")
output_df = outRead[c("testName","timepoint","value","Positive","Negative")]
# note that permutation tests returns a list of 5, but testing only 3rd

# note: can't test becaue of clusterstats blocker

test_that("all required parameters work", {
  ret_df = PermutationTest_HMLET(input_df, samples = 1000, paired = T, permuteTrialsWithinSubject = F)

  expect_equal(names(ret_df[[3]]), names(output_df))
})
