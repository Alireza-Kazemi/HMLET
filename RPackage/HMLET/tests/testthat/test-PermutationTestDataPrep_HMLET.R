# ASK: maybe run the matlab script and generate new sample data each time?
datRead = read.csv("SampleData.csv")
df = datRead[c("ID","trial","timepoint","condition","AOI")]

test_that("all required parameters work", {
  dfRet = PermutationTestDataPrep_HMLET(data=df, ID="ID", trial="trial",
                                        timepoint="timepoint", condition="condition",
                                        gazeInAOI="AOI")
  # null test name parameter should populate df with default value:
  expect_equal(unique(dfRet$testName), "PermutationTest0")
  # null time bin name parameter should create df with the following columns:
  expect_equal(names(dfRet), c("testName", "ID", "trial", "timepoint", "condition", "AOI"))
})

test_that("AOI column should be numeric", {
  df$AOI = unique("Not_A_Number")
  expect_warning(
    PermutationTestDataPrep_HMLET(data=df, ID="ID", trial="trial",
                                  timepoint="timepoint", condition="condition",
                                  gazeInAOI="AOI"),
                 "AOI column must be in numeric form")
})

test_that("timepoint column should be numeric", {
  df$timepoint = unique("Not_A_Number")
  expect_warning(
    PermutationTestDataPrep_HMLET(data=df, ID="ID", trial="trial",
                                  timepoint="timepoint", condition="condition",
                                  gazeInAOI="AOI"),
    "timepoint column must be in numeric form")
})


test_that("comtains time bin name column if exists", {
  df = CreateTimeBinData_HMLET(data=df, FixatedOn = "AOI", AOIs = "AOI")
  dfRet = PermutationTestDataPrep_HMLET(data=df, ID="ID", trial="trial",
                                        timepoint="timepoint", condition="condition",
                                        gazeInAOI="AOI", timeBinName = "timeBin")
  expect_named(dfRet, c("testName", "ID", "trial", "timepoint", "condition",
                        "AOI", "timeBinName"))
})


# generate test for not null condition levels (need more input data)
# generate test for not null target aoi (need more input data)




