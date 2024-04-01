inRead = read.csv("HMLET_Testing_Data.csv")
input_df = inRead[c("ID","trial","timePoint","condition","AOI")]

test_that("all required parameters work", {
  ret_df = CreateTimeBinData_HMLET(data=input_df, timePoint = "timePoint",
                                   groupingColumns = c("ID","trial","condition"),
                                   FixatedOn = "AOI")
  expect_equal(names(ret_df), c("ID", "trial", "condition","timeBin",
                                "timeBinIndex", "AOI_0", "AOI_1"))

})


