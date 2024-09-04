### set testdirectory
dirtestdata <- dir(system.file("extdata","Tutorial_Data", package = "hiaR"))
print(dirtestdata)


test_that("run dynamo_hia", {
  results <- runDynamoHIA(dirtestdata, "Tutorial_1" )
  ## do some test on result
})
