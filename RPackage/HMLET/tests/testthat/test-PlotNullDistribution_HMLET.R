library(ggplot2)
library(reshape)

inRead = read.csv("Output_PermutationTestDataPrep.csv")
input_df = inRead[c("testName","ID","trial","timepoint","condition","AOI")]
# note: can't test becaue of clusterstats blocker
test_that("plot null distribution works", {
  res = PermutationTest_HMLET(input_df, samples = 1000, paired = T, permuteTrialsWithinSubject = F)
  test_plot <- PlotNullDistribution_HMLET(res)
  vdiffr::expect_doppelganger("Null Distribution", test_plot)
})

# include more tests that play with different sets of parameters?
