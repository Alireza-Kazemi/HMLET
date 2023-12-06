inRead = read.csv("Output_PermutationTestDataPrep.csv")
input_df = inRead[c("testName","ID","trial","timePoint","condition","AOI")]

outRead = read.csv("Output_PermutationTest.csv")
output_df = outRead[c("testName","timePoint","value","Positive","Negative")]
# note that permutation tests returns a list of 5, but testing only 3rd

# note: can't test becaue of clusterstats blocker

test_that("all required parameters work", {
  set.seed(5)
  res = PermutationTest_HMLET(input_df, samples = 1000, paired = T, permuteTrialsWithinSubject = F)
  test_plotNULL <- PlotNullDistribution_HMLET(res)
  test_plotGT <- PlotTemporalGazeTrends_HMLET(res)

  expect_equal(names(res[[3]]), names(output_df))
  vdiffr::expect_doppelganger("Null Distribution", test_plotNULL)
  vdiffr::expect_doppelganger("Temporal Gaze Trends", test_plotGT)
})


