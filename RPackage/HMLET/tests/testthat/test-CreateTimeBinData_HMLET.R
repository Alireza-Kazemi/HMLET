inRead = read.csv("HMLET_Testing_Data.csv")
input_df = inRead[c("ID","trial","timePoint","condition","AOI")]

test_that("all required parameters work", {
  ret_df = CreateTimeBinData_HMLET(data=input_df,
                                   timePoint = "timePoint",
                                   FixatedOn = "AOI")
  expect_equal(names(ret_df), c("ID", "trial", "timePoint", "condition", "AOI",
                                "timeBin", "timeBinIndex", "AOI_0", "AOI_1"))
})

# include more tests that play with different sets of parameters?

