datRead = read.csv("SampleData.csv")
df = datRead[c("ID","trial","timepoint","condition","AOI")]

test_that("create time bin data works", {
  dfRet = CreateTimeBinData_HMLET(data=df, FixatedOn = "AOI")
  expect_named(dfRet, c('ID', 'trial', 'timepoint', 'condition', 'AOI',
                        'timeBin', 'timeBinIndex', 'AOI_0', 'AOI_1'))
})
