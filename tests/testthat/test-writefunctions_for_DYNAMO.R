### tests for writefunctions

library(xml2)
### one test per file

# make data to use in tests

# data containing one row for each age/sex combination
data_1r <- data.frame(age= rep(rep(0:95,each=3),2), sex=rep(c(0,1,0,1,0,1), each=96),cat=rep(1:3,192),
                   RR=rep(rep(seq(1,10.5, 0.1),each=3),2))

# data containing one row per riskfactor category
data2 <- data.frame(age= rep(0:95,2), sex=rep(0:1, each=96),
                    prev=rep(seq(0,9.5, 0.1),2),
                    inc=rep(seq(0,0.95, 0.01),2),
                    dw=rep(seq(10,19.5, 0.1),2),
                    number =rep(seq(195000,100000, -1000),2),
                    RR=rep(seq(1,1.95, 0.01),2),
                    cat1 = rep(seq(0,95/2,0.5),2),
                    cat2 = rep(seq(0,95/4,0.25),2)
)
data2$cat3 <- 100-data2$cat1-data2$cat2

# each test writes the file to the testdirectory and then validates it
# we do not test whether the data are entered correctly in the file
# there should been visual inspection and snapshot test can be added after that
# start with emptying the test directory



dirtestdata2 <- paste0(system.file("extdata",package = "hiaR"),"/testdirectory") 
unlink(dirtestdata2, recursive = TRUE) ## delete the directory and its contents
if (!file.exists(dirtestdata2)) dir.create(dirtestdata2) ## and create an empty one


test_that("validate writing simulation configuration file" , {
  dirtestdata <- system.file("extdata","Tutorial_Data", package = "hiaR")
  tree <- getValidTree(dirtestdata)
  populationName <- tree$populations[[1]]$populationName
  rfList <- tree$riskfactors[[1]]
  rfnew <- list(name=rfList$riskfactorName, transitionFile=rfList$transitionFiles[[1]],
                prevalenceFile=rfList$RFprevalence[[1]])
  disListnew <-list()
  for (dis in tree$diseases){
    disList <- list(name=dis$diseaseName, incidenceFile=dis$incidence[[1]],
                    prevalenceFile=dis$prevalence[[1]],excessmortFile=dis$excessmortality[[1]],
                    disabilityFile=dis$disability[[1]])
  disListnew <- append(disListnew, list(disList))  
  }
  relriskList <- list()
  rrdat <-tree$relativeRisks
  RFname <- rfList$riskfactorName
  for (dis in tree$diseases){
    disname <- dis$diseaseName
    subdat <- subset(rrdat, rrdat$to==disname & rrdat$from==RFname)
    element <- list(to = disname, from= RFname, filename=subdat$fileName) 
    relriskList <- append(relriskList, list(element))
  
   
  }
  
  scenarioList <-list( list(name = "testalternative ", successRate = "100", minAge= "0",maxAge="95",targetSex = 2,
                        transitionFile =rfList$transitionFiles[[2]], prevalenceFile=rfList$RFprevalence[[2]]))
                     
   
  writeSimulationConfiguration( dirtestdata, "TestSimulationWriting", newborns=TRUE, startingyear="2020",
                                yearsSimulated="5",popSize="10", minAge="0", maxAge="95",
                                          refScenarioName="test",randomSeed="5", 
                                populationName=populationName, 
                                scenarioList, disListnew,  rfnew,relriskList)
    
  ### validate using the schema 
  
  docC <- read_xml(paste0(dirtestdata,"/Simulations/TestSimulationWriting/configuration.xml"))
  schemaC <-  read_xml(system.file("extdata/schemas", "simulation.xsd", package = "hiaR"))
  expect_true(
    xml_validate(docC,schemaC))  
    
    
}
)

## test for writing
  
  test_that("validate writing of riskfactor categorical and its configuration " , {
  writeRiskFactorsPrevCategorical("RF",3,dirtestdata2,"testfile",data2,c("cat1","cat2","cat3")) 
  doc1 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Risk_factors\\RF\\configuration.xml"))
  doc2 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Risk_factors\\RF\\Prevalences\\testfile.xml"))
  schema1 <-  read_xml(system.file("extdata/schemas", "riskfactor_categorical.xsd", package = "hiaR"))
  schema2 <-  read_xml(system.file("extdata/schemas", "riskfactorprevalences_categorical.xsd", package = "hiaR"))
  expect_true(
  xml_validate(doc1,schema1))
  expect_true(
    xml_validate(doc2,schema2))}
  )
  
  

test_that("validate writing of RR for  riskfactor categorical " , {
  writeRRriskfactorCat("dis","RF",3,dirtestdata2,"testfile",data_1r,"RR")   
  doc3 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Diseases\\dis\\Relative_Risks_From_Risk_Factor\\testfile-RF.xml"))
  schema3 <-  read_xml(system.file("extdata/schemas", "relrisksfromriskfactor_categorical.xsd", package = "hiaR"))
  expect_true(
    xml_validate(doc3,schema3))
 }
)



test_that("validate writing of RR disease on disease" , {
  writeRRdisease("dis", "dis2", dirtestdata2, "testfile" , data2,"RR") 
  doc3a <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Diseases\\dis\\Relative_Risks_From_Diseases\\testfile-dis2.xml"))
  schema3a <-  read_xml(system.file("extdata/schemas", "relrisksfromdisease.xsd", package = "hiaR"))
  expect_true(
    xml_validate(doc3a,schema3a))
}
)


test_that("validate writing RR for riskfactor categorical on death" , {
  writeRRMortality("RF",3,dirtestdata2,"testfile",data_1r,"RR")    
  doc4 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Risk_Factors\\RF\\Relative_Risks_For_Death\\testfile.xml"))
  schema4 <-  read_xml(system.file("extdata/schemas", "relrisksfordeath_categorical.xsd", package = "hiaR"))
  expect_true(
    xml_validate(doc4,schema4))
}
)






test_that("validate riskfactor categorical OR for disability" , {
  writeORdisability("RF",3,dirtestdata2,"testfile",data_1r,"RR")    
  doc5 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Risk_Factors\\RF\\Odds_Ratios_For_Disability\\testfile.xml"))
  schema5 <-  read_xml(system.file("extdata/schemas", "relrisksfordisability_categorical.xsd", package = "hiaR"))
  expect_true(
    xml_validate(doc5,schema5))
}
)

test_that("validate writing of disease prevalence file" , {
  writeDisPrevalence("dis",dirtestdata2,"testfile",data2,"prev")    
  doc6 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Diseases\\dis\\Prevalences\\testfile.xml"))
  schema6 <-  read_xml(system.file("extdata/schemas", "diseaseprevalences.xsd", package = "hiaR"))
  expect_true(
    xml_validate(doc6,schema6))
}
)


test_that("validate writing of disease incidence file" , {
  writeDisIncidence("dis",dirtestdata2,"testfile",data2,"inc")    
  doc7 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Diseases\\dis\\Incidences\\testfile.xml"))
  schema7 <-  read_xml(system.file("extdata/schemas", "diseaseincidences.xsd", package = "hiaR"))
  expect_true(
    xml_validate(doc7,schema7))
}
)

test_that("validate old style dalyweights for diseases" , {
  writeDisDaly("dis",dirtestdata2,"testfile",data2, "dw")
  doc8 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Diseases\\dis\\DALY_weights\\testfile.xml"))
  schema8 <-  read_xml(system.file("extdata/schemas", "dalyweights.xsd", package = "hiaR"))
  expect_true(
    xml_validate(doc8,schema8))
}
)
  
 test_that("validate writing of files for disability for disease (dynamo-2)" , {
    writeDiseaseDisability("dis",dirtestdata2,"testfile",data2,"dw")    
    doc9 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Diseases\\dis\\Disability\\testfile.xml"))
    schema9 <-  read_xml(system.file("extdata/schemas", "dalyweights.xsd", package = "hiaR"))
    expect_true(
      xml_validate(doc9,schema9))
  }
  )
  
  
  test_that("validate writing of overall mortality data" , { 
   writeOverallMortality(dirtestdata2, "testcountry",  data2, "inc") 
   doc10 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Populations\\testcountry\\overallmortality.xml"))
  schema10 <-  read_xml(system.file("extdata/schemas", "overallmortality.xsd", package = "hiaR"))
  expect_true(
    xml_validate(doc10,schema10))
}
)

  
  
  test_that("validate writing of overall disability data" , { 
    writeOverallDisability(dirtestdata2, "testcountry",  data2, "prev") 
    doc11 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Populations\\testcountry\\overalldisability.xml"))
    schema11 <-  read_xml(system.file("extdata/schemas", "overalldisability.xsd", package = "hiaR"))
    expect_true(
      xml_validate(doc11,schema11))
  }
  )

  
  test_that("validate writing of population size data" , { 
    writeSize(dirtestdata2, "testcountry",  data2, "number") 
    doc12 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Populations\\testcountry\\size.xml"))
    schema12 <-  read_xml(system.file("extdata/schemas", "populationsize.xsd", package = "hiaR"))
    expect_true(
      xml_validate(doc12,schema12))
  }
  )
  
  test_that("validate writing of newborn data" , { 
    data1<-c(103000,105000,107000)  #Number of newborns in 2013, in 2014, and in 2015 and all future years
    writeNewborn(dirtestdata2, "testcountry", 1.02, 2013, data1)
    doc12 <- read_xml(paste0(dirtestdata2,"/Reference_Data\\Populations\\testcountry\\overalldisability.xml"))
    schema12 <-  read_xml(system.file("extdata/schemas", "overalldisability.xsd", package = "hiaR"))
    expect_true(
      xml_validate(doc12,schema12))
  }
  )
