### NB these test only work after first running the tests for the writing functions!!


## to do test if rr for disability is read (add to tutorial data)
## to do test relriskfordeath compound (gaat iets fout met schemas)

dirtestdata <- system.file("extdata","Tutorial_Data", package = "hiaR")
### this should be run only after the test for
dirtestdata2 <- paste0( "c:/temp/test")  ### same as in testing writing functions
list.files(paste0(dirtestdata2,"/Reference_Data","/Diseases/dis/Prevalences"))


test_that("test read single population", {                   
 currentpath <- paste0(dirtestdata2,"/Reference_Data/Populations/testcountry")
 filename <- "size"
 xsdname <- "populationsize"
 res <- getSingleFile(currentpath, filename, xsdname)

 expect_true(length(res$validList)==1)
})

test_that("test read single disease", {                   
  currentpath <- paste0(dirtestdata2,"/Reference_Data/Diseases/dis/prevalences")
  filename <- "testfile"
  xsdname <- "diseaseprevalences"
  res <- getSingleFile(currentpath, filename, xsdname)
  
  expect_true(length(res$validList)==1)
})

test_that("test read multiple files", { 
  basedir <- dirtestdata
  subdir <- "/Reference_Data/Risk_Factors/BMI_cat3/Prevalences"
  xsdname <- "riskfactorprevalences_categorical"
  res <- getMultipleFiles(basedir, subdir, xsdname)
  
  expect_true(length(res$validList)==3)
})


test_that("a valid tree is returned", {
  tree <- makeTreeList( dirtestdata)
  expect_true(attr(tree,"valid"))
  expect_equal(length(tree$populations$validList), 1)
  expect_equal(length(tree$diseases$validList), 9)
  expect_equal(length(tree$riskFactors$validList), 5)
})

### more test to think of
