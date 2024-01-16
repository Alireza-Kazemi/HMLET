inRead = read.csv("HMLET_Testing_Data.csv")
input_df = inRead[c("ID","trial","timePoint","condition","AOI")]

outRead = read.csv("Output_PermutationTestDataPrep.csv")
output_df = outRead[c("testName","ID","trial","timePoint","condition","AOI")]

test_that("all required parameters work", {
  ret_df = PermutationTestDataPrep_HMLET(data = input_df, ID = "ID", trial = "trial", timePoint = "timePoint",
                                         condition = "condition", conditionLevels = c("C1","C2"),
                                         gazeMeasure = "AOI", targetAOI = 1)

  expect_equal(names(ret_df), c("testName", "ID", "trial", "timePoint", "condition", "AOI"))

  expect_equal(ret_df$testName, output_df$testName)
  expect_equal(ret_df$ID, output_df$ID)
  expect_equal(ret_df$trial, output_df$trial)
  expect_equal(ret_df$timePoint, output_df$timePoint)
  ret_df$condition <- as.character(ret_df$condition)
  expect_equal(unique(ret_df$condition), unique(output_df$condition))
  expect_equal(ret_df$AOI, output_df$AOI)
})

test_that("gazeMeasure column should be numeric", {
  input_df$AOI = unique("Not_A_Number")
  expect_warning(
    PermutationTestDataPrep_HMLET(data = input_df, ID = "ID", trial = "trial", timePoint = "timePoint",
                                  condition = "condition", conditionLevels = c("C1","C2"),
                                  gazeMeasure = "AOI"),
    "gazeMeasure column must be in numeric form")
})


test_that("timePoint column should be numeric", {
  input_df$timePoint = unique("Not_A_Number")
  expect_warning(
    PermutationTestDataPrep_HMLET(data = input_df, ID = "ID", trial = "trial", timePoint = "timePoint",
                                  condition = "condition", conditionLevels = c("C1","C2"),
                                  gazeMeasure = "AOI", targetAOI = 1),
    "timePoint column must be in numeric form")
})

test_that("contains time bin name column if exists", {
  output = CreateTimeBinData_HMLET(data=input_df,  timePoint = "timePoint",
                                   FixatedOn = "AOI")
  ret_df = PermutationTestDataPrep_HMLET(data=output, ID = "ID", trial = "trial", timePoint = "timePoint",
                                         condition = "condition", conditionLevels = c("C1","C2"),
                                         gazeMeasure = "AOI", targetAOI = 1, timeBinName = "timeBin")
  expect_named(ret_df, c("testName", "ID", "trial", "timePoint", "condition",
                         "AOI", "timeBinName"))
})


