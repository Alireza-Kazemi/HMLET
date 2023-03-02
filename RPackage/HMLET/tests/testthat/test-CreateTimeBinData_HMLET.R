inRead = read.csv("HMLET_Testing_Data.csv")
input_df = inRead[c("ID","trial","timepoint","condition","AOI")]

test_that("all required parameters work", {
  ret_df = CreateTimeBinData_HMLET(data=input_df,
                                   FixatedOn = "AOI", AOIs = "AOI")
  expect_equal(names(ret_df), c("ID", "trial", "timepoint", "condition", "AOI",
                                "timeBin", "timeBinIndex", "AOI_AOI"))
})

# include more tests that play with different sets of parameters?
