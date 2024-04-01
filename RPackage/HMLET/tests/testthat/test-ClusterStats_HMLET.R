inRead = read.csv("Output_PermutationTestDataPrep.csv")
input_df = inRead[c("testName","ID","trial","timePoint","condition","AOI")]

outRead = read.csv("Output_ClusterStats.csv")
output_df = outRead[c("testName","Direction","index","tStatistic","timeStart", "timeEnd")]

test_that("all required parameters work", {
  datP = PermutationTestDataPrep_HMLET(data = input_df, ID = "ID", trial = "trial", timePoint = "timePoint",
                                       condition = "condition", conditionLevels = c("C1","C2"),
                                       gazeMeasure = "AOI", targetAOI = 1)

  ret_df = ClusterStats_HMLET(input_df, paired = T, detailed = F)
  expect_equal(names(ret_df), c("testName","Direction","index","tStatistic","timeStart", "timeEnd"))
})

