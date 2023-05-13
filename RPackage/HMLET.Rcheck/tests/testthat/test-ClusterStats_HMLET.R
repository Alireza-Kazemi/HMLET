inRead = read.csv("Output_PermutationTestDataPrep.csv")
input_df = inRead[c("testName","ID","trial","timepoint","condition","AOI")]

outRead = read.csv("Output_ClusterStats.csv")
output_df = outRead[c("testName","Direction","index","tStatistic","timeStart", "timeEnd")]

test_that("all required parameters work", {
  ret_df = ClusterStats_HMLET(input_df, paired = T, detailed = F)
  #TODO: output has two value columns?
  expect_equal(names(ret_df), c("testName","Direction","index","tStatistic","timeStart", "timeEnd"))

  #expect_equal(ret_df$testName, output_df$testName)
  #expect_equal(ret_df$timepoint, output_df$timepoint)
  #expect_equal(ret_df$value, output_df$value)
  #expect_equal(ret_df$variable, output_df$variable)
})
