################################################################################
#                                                                              #
#           functions reading the xml files in the                             #
#           DYNAMO-HIA homedirectory and making lists of valid information     #                   #
#                                                                              #
################################################################################

## author: Hendriek Boshuizen#

## TODO make functions to get warnings and errors

## Function that scans the dynamo-HIA workdirectory and makes an object 
## with all valid file and invalid information in the dynamo-directory
##
## Basis for getting the valid tree, and for getting warnings and errors
## 
## 
 
# dynamodir <- system.file("extdata","Tutorial_Data", package = "hiaR")

## package xml2 is used for validation of the xmls (using schemas)
require(xml2)

#' Makes a list of the valid information in the Dynamo-HIA work directory. It also contains information on invalid files in the population,
#' risk factor and disease directories. Relative risk files are only included for valid disease and valid risk factors.
#'
#' @param dynamodir name of Dynamo-HIA workdirectory (parent directory of the subdirectories Reference_Data and Simulations). This is the full path
#'
#' @return A nested list with on the lowest level the file names of xml files, divided in lists of valid and invalid files.
#' The list contains on the highest level 1) list of populations, 2) list of diseases 3) list of risk factors
#' 4) dataframe containing names of relative risk files associated with valid diseases and valid risk factors
#' The attribute "valid" is TRUE when the is at least one valid population and one valid risk factor
#' @export
#'
#' @examples
#' dynamodir <-   dir(system.file("extdata", package = "hiaR"))
#' treeList <- makeTreeList(dynamodir)
#' attributes(treeList)$valid
#' # if not find errors
#' attributes(treeList, "errors")
#' # check if the population is valid
#' attr(treeList$populations, "errors")

makeTreeList <- function(dynamodir) {
  popList <- getPopulations(dynamodir)
  disList <- getDiseaseList(dynamodir)
  rfList  <- getRiskFactorList(dynamodir)
  rrTable <- getRRTable(dynamodir, disList, rfList) 
  
## set valid attribute
## in contrast to the java ui we only require a population and a riskfactor to be present
  valid <-  ((length(popList$validList)>0)  &  (length(rfList$validList)>0)) 
  tree <-
    list(
      populations = popList,
      diseases = disList,
      riskFactors = rfList,
      relativeRisks = rrTable
    )

  attr(tree, "valid") <- valid
  if (valid) {
    attr(tree, "errors") <- NULL
  } else {
    if (!attributes(popList)$valid)
      attr(tree, "errors") <-
        "No valid population, look at attr(this$populations,'errors') for more information. "
    if (!attributes(rfList)$valid)
      attr(tree, "errors") <-
        append(
          attr(tree, "errors"),
          "No valid riskfactor, look at attr(this$riskFactors,'errors') for more information"
        )
  }
  return(tree)
  
}



#' Get valid data in the Dynamo-Hia home directory
#'
#' @param dynamodir full path to the Dynamo-HIA workdirectory (parent directory of the subdirectories 
#' Reference_Data and Simulations)
#'
#'
#' @return list with valid information in the tree. This list has 4 elements:
#' $populations containing a list of valid populations
#' $disease containing a list of valid diseases
#' $riskfactors containing a list of valid riskfactors
#' $relativeRisks containing a data.frame with relative risks
#' Populuations, diseases and riskfactors with incomplete data (particular files missing)
#' are invalid and not in this list
#' 
#' 
#' 
#' @export
#'
#' @examples
#' getValidTree(dynamodir)
getValidTree <- function(dynamodir){
  
  tree <- makeTreeList(dynamodir)
  if (! attr(tree, "valid")){ 
    print("directory is not valid")
    print (attr(tree, "errors")) 
    stop("not a valid tree")
  }
  
    ## select valid populations
 popList <- list()
  for (pop in tree$populations$validList){
     newList <- list()
     newList$populationName <- attr(pop, "name")
     newList$size <- pop$size$validList
     newList$newborns <- pop$newborns$validList
     newList$overallmortality <-pop$overallmortality$validList
     newList$overalldisability <-pop$overalldisability$validList
      
    popList <- append(popList, list(newList) )
    
  }
  ## select valid diseases
  disList <- list()
  for (dis in tree$diseases$validList){
    newList <- list()
    newList$diseaseName <- attr(dis,"name")
    newList$incidence <- dis$inc$validList
    newList$prevalence <- dis$prev$validList
    newList$excessmortality <- dis$em$validList
    newList$disability <- dis$disab$validList
    disList <- append(disList, list(newList)) ## without list() this is added to the previous disease
    
  }
  ## select valid populations
  rfList <- list()
  for (rf in tree$riskFactors$validList){
    newList <- list()
    newList$riskfactorName <- attr(rf,"name")
    newList$riskfactorType <- rf$type # not really needed, 
    newList$nClasses <- rf$nClasses # not really needed, 
    newList$RFprevalence <- rf$prevalenceFiles$validList
    newList$transitionFiles <- rf$transitionFiles$validList
    rfList <- append(rfList, list(newList)) ## without list() this is added to the previous riskfactor
    
  }
  ## select valid relative risks
  RRdat <- subset(tree$relativeRisks, valid)
  
  tree2 <- list(populations=popList,
                diseases=disList,
                riskfactors=rfList,
                relativeRisks=RRdat)
  return(tree2)
  
}


##################################################################################

##   read populations

##################################################################################


#' Gets a list of valid and invalid populations
#'
#' @param dynamodir full path to dynamo-HIA workdirectory 
#'
#' @return list with two elements:  list valid populations, list of invalid populationns
#' 
#'
#' @examples
getPopulations <- function(dynamodir) {
  dir2scrape <- paste0(dynamodir, '/Reference_Data/Populations')
  crudelist <- list.files(dir2scrape)
##     subdir = crudelist[[1]]
  message <- ""
  validList <- list()  # list containing all valid populations
  invalidList <- list()
  for (subdir in crudelist) {
    ## this directory either contains valid data or invalid data
    valid <- TRUE  ## is this particular population valid
    sizefile <-
      getSingleFile(paste0(dir2scrape, "/", subdir), "size", "populationsize")
    if (length(sizefile[["validList"]]) != 1)  {
      valid <- FALSE
      message <- append(message,
             paste("No valid file 'size' in population", subdir))
    }
    
    newbornsfile <-
      getSingleFile(paste0(dir2scrape, "/", subdir), "newborns", "newborns")
    if (length(sizefile[["validList"]]) != 1)  {
      valid <- FALSE
      message <- append(message,
             paste("No valid file 'newborns' in population", subdir))
    }
    
    overallmortalityfile <-
      getSingleFile(paste0(dir2scrape, "/", subdir),
                    "overallmortality",
                    "overallmortality")
    if (length(overallmortalityfile[["validList"]]) != 1)  {
      valid <- FALSE
      message <- append(message,
             paste("No valid file 'overallmortality' in population", subdir))
    }
    
    overalldisabilityfile <-
      getSingleFile(paste0(dir2scrape, "/", subdir),
                    "overalldisability",
                    "overalldisability")
    if (length(overalldisabilityfile[["validList"]]) != 1)  {
      valid <- FALSE
      message <- append(message,
             paste("No valid file 'overalldisability' in population", subdir))
    }
    
    
    
    popList <- list( 
      size = sizefile,
      newborns = newbornsfile,
      overallmortality = overallmortalityfile,
      overalldisability = overalldisabilityfile
    )
    attr(popList, "name") <- subdir
    if (message != "") paste("errors: ", message)
    attr(popList, "errors") <- message
    
    ## valid is true only when all the separate file objects contain a valid list,  here we assign to the higher level validList of valid populations
    if (valid) {
      validList <- append(
        validList, list(popList) # double list needed to make the list an element on its own
      ) }
  else { # might contain partly valid elements
         
          invalidList <- append(
            invalidList,list(popList) 
      )} 
  } ## end loop over the separate populations
  

  
  return(list(validList=validList,invalidList=invalidList))
  
} ## end function



##################################################################################

##   read disease info

##################################################################################

#' file that retrieves diseases from the reference data, split into valid and invalid disease
#' Invalid diseases are those where something is wrong with the underlying files
#' This list does not includes relative risks / oddsratios for death and disability,
#' those are read separately
#' @param dynamodir Dynamo-Hia work directory (full path)
#'
#' @return list with two elements:  list valid diseases, list of invalid disease
#'         invalid list contains values on errors
#' 
#'
#' @examples
#'
#'
#'   dynamodir <- dirtestdata
getDiseaseList <- function(dynamodir) {
  dir2scrape <- paste0(dynamodir, '/Reference_Data/Diseases')
  crudelist <- list.files(dir2scrape)
  ## subdir = crudelist[[1]]
  validList <- list()
  invalidList <- list()
  for (subdir in crudelist) {
    ## this directory either contains valid data or invalid data
    disDat <- getDiseaseData(paste0(dir2scrape, "/", subdir))
    attr(disDat,"name")=subdir
    if (attr(disDat, "valid"))
      validList <- append(validList, list(disDat)) ## ad as a list, otherwise this will get the same level as other diseases
    else
      invalidList <- append(invalidList, list(disDat))
    
    
  }
  return(list(validList=validList,invalidList=invalidList))
}


#' reads the filenames in the  disease directory and checks if they are OK (does not consider the relative risks)
#'
#' @param diseaseDir Full path to the directory of the disease to be checked
#'
#' @return a list of lists:  list(incList, prevList, emList, dwList) containing respectively
#' incidence files, prevalence files, excess mortality files and Dalyweigth files
#' attributes are valid (true/false), errors, warnings
#'#'
#' @examples
#' diseaseDir <- paste0(dirtestdata, '/Reference_Data/Diseases/',subdir)
getDiseaseData <- function(diseaseDir) {
  valid = TRUE
  errorMessage <- "Errors: "
  warningMessage <- "Warnings: "
  # read the subdata names (=directory names)
  dirlist <- list.files(diseaseDir,  include.dirs = TRUE)
  ### DALYweights
  if (!("Disability" %in% dirlist)) {
    valid <- FALSE
    errorMessage <- append(errorMessage, "No DALY-Weights directory present ")
  } else {
    dwList <- getMultipleFiles(diseaseDir, "/Disability", "dalyweights")
    if (length(dwList[["validList"]]) == 0) {
      valid <- FALSE
      message <- append(message,
             paste(
               "No valid disability files in disease directory",
               diseaseDir
             ))
    }
  }
  
  ### Excess mortality
  if (!("Excess_Mortalities" %in% dirlist)) {
    valid <- FALSE
    errorMessage <- append(errorMessage, "Excess_Mortalities directory present ")
  } else {
    emList <-
      getMultipleFiles(diseaseDir, "/Excess_Mortalities", "excessmortality")
    if (length(emList[["validList"]]) == 0) {
      valid <- FALSE
      message <- append(
        message,
        paste(
          "No valid excess mortality files in disease directory",
          diseaseDir
        )
      )
    }
  }
  
  ### Incidence
  if (!("Incidences" %in% dirlist)) {
    valid <- FALSE
    errorMessage <- append(errorMessage, "No Incidences directory present ")
  } else {
    incList <-
      getMultipleFiles(diseaseDir, "/Incidences", "diseaseincidences")
    if (length(incList[["validList"]]) == 0) {
      valid <- FALSE
      message <- append(
        message,
        paste(
          "No valid disease incidence files in disease directory",
          diseaseDir
        )
      )
    }
  }
  
  ### prevalence
  if (!("Prevalences" %in% dirlist)) {
    valid <- FALSE
    errorMessage <- append(errorMessage, "No Prevalences directory present ")
  } else {
    prevList <-
      getMultipleFiles(diseaseDir, "/Prevalences", "diseaseprevalences")
    if (length(prevList[["validList"]]) == 0) {
      valid <- FALSE
      message <- append(message,
        paste(
          "No valid disease incidence files in disease directory",
          diseaseDir
        )
      )
    }
  }
  
  
  
  ## relative risks are not considered here
  
  
  
  if (length(incList[["invalidList"]]) != 0) {
    warningMessage <- append(
      warningMessage,
      paste(
        "XML file with wrong format in",
        diseaseDir,
        " use getDiseasefilesErrors for more info "
      )
    )
  }
  if (length(emList[["invalidList"]]) != 0) {
    valid <- FALSE
    warningMessage <- append(
      warningMessage,
      paste(
        "XML file with wrong format in",
        diseaseDir,
        " use getDiseasefilesErrors for more info "
      )
    )
  }
  if (length(prevList[["invalidList"]]) != 0) {
    valid <- FALSE
    warningMessage <- append(
      warningMessage,
      paste(
        "XML file with wrong format in",
        diseaseDir,
        " use getDiseasefilesErrors for more info "
      )
    )
  }
  if (length(dwList[["invalidList"]]) != 0) {
    valid <- FALSE
    warningMessage <- append(
      warningMessage,
      paste(
        "\nXML file with wrong format in",
        diseaseDir,
        " use getDiseasefilesErrors for more info "
      )
    )
  }
  
  
  
  
  if (valid)
    errorMessage = 0
  disData <- list(inc=incList, prev=prevList, em=emList, disab=dwList)
  attr(disData, "valid") <- valid
  attr(disData, "errors") <- errorMessage
  attr(disData, "warnings") <- warningMessage
  
  return(disData)
  
}

##################################################################################

##   read risk factors

##################################################################################

#' file that retrieves riskfactors from the reference data, split into valid and invalid risk factors
#' this does not includes relative risks / oddsratios for death and disability
#' @param dynamodir full path to the Dynamo-HIA work directory
#'
#' @return list with two elements:  list valid risk factors, list of invalid risk factors
#'         invalid list contains values on errors, both give the type of risk factor
#' 
#'
#' @examples
#'
getRiskFactorList <- function(dynamodir) {
  dir2scrape <- paste0(dynamodir, '/Reference_Data/Risk_Factors')
  crudelist <- list.files(dir2scrape) ### LIST WITH ALL RISKFACTORS 
  #subdir = crudelist[[2]]
  validList <- list()
  invalidList <- list()
  for (subdir in crudelist) {
    # print(subdir)
    ## this directory either contains valid data or invalid data
    rfDat <- getRiskFactorData(paste0(dir2scrape, "/", subdir))
    attr(rfDat,"name") <- subdir
    if (attr(rfDat, "valid"))
      validList <- append(validList, list(rfDat))
    else
      invalidList <- append(invalidList, list(rfDat))
    
    
  }
  return(list(validList=validList, invalidList=invalidList))
}


#' Checks if the riskfactor directory seems OK (does not consider the relative risks)
#'
#' @param rfDir Full path to the directory for the risk factor to be checked
#'
#' @return a list with 4 elemens:   prevalenceFiles , transitionFiles,nClasses , type
#' containing respectively
#' a list of prevalence files, a list of transition file names, number of classes of
#' the riskfactor and type of the riskcator
#' attributes are valid (true/false), errors, warnings
#'
#'
#' @examples
#'
getRiskFactorData <- function(rfDir) {
  require(xml2)
  valid = TRUE
  #print(rfDir)
  errorMessage <- "Errors: "
  warningMessage <- "Warnings: "
  # read the subdata names (=directory names)
  dirlist <- list.files(rfDir, include.dirs = TRUE)
  
  if (!("configuration.xml" %in% dirlist)) {
    valid <- FALSE
    errorMessage <- append(errorMessage,
           paste("No riskfactor configuration file found in directory ", rfDir))
  } else
    
    
  doc <- read_xml(paste0(rfDir, "/configuration.xml"))
  type <- NULL
  if ((xml_name(doc)) == "riskfactor_categorical")
    type <- "categorical"
  if ((xml_name(doc)) == "riskfactor_continuous")
    type <- "continuous"
  if ((xml_name(doc)) == "riskfactor_compound")
    type <-    "compound"
  #print(type)
  
  ## VALIDATE the risk factor configuration file
  if (type == "categorical") {
    xsd.filename <- system.file("extdata","schemas","riskfactor_categorical.xsd", package = "hiaR")
    xsd <- read_xml(xsd.filename)
    valid <- xml_validate(doc, xsd)
    }
  if (type == "compound"){
    xsd.filename <- system.file("extdata","schemas","riskfactor_compound.xsd", package = "hiaR")
    xsd <- read_xml(xsd.filename)
    valid <- xml_validate(doc, xsd)
  }
    
  if (type == "continuous") {
    xsd.filename <- system.file("extdata","schemas","riskfactor_continuous.xsd", package = "hiaR")
    xsd <- read_xml(xsd.filename)
    valid <- xml_validate(doc, xsd)
  }
    
  if (is.null(type))     valid <- FALSE
  if (!valid)
    errorMessage <- append(
      errorMessage,
      paste(
        "Riskfactor configuration file has wrong format in directory ",
        rfDir
      )
    )
  if (type != "continuous")
    nClasses <-
    length(xml_find_all(xml_child(doc), ".//class"))   else
    nClasses <- NULL
  
  ### read and return the prevalence information
  if (!("Prevalences" %in% dirlist)) {
    valid <- FALSE
    errorMessage <- append(
      errorMessage,
      paste("No Prevalences directory present in risk factor directory"),
      rfDir
    )
  } else {
    if (type == "continuous")
      prevList <-
        getMultipleFiles(rfDir,
                         "/Prevalences",
                         "riskfactorprevalences_continuous")
    if (type == "categorical")
      prevList <-
        getMultipleFiles(rfDir,
                         "/Prevalences",
                         "riskfactorprevalences_categorical")
    if (type == "compound")
      prevList <-
        getMultipleFiles(rfDir,
                         "/Prevalences",
                         "riskfactorprevalences_categorical")
    if (length(prevList[["validList"]]) == 0) {
      valid <- FALSE
      message <- append(message,
             paste(
               "No valid prevalence directory in risk factor directory",
               rfDir
             ))
    }
    
    
  }
  
  ### read the transitionfiles 
  if (!("Transitions" %in% dirlist)) {
    valid <- FALSE
    errorMessage <- append(
      errorMessage,
      paste(
        "No Transitions directory present in risk factor directory ",
        rfDir
      )
    )
  } else{
    transList <-
      getTransitionFiles(rfDir) ### deze apart maken
    if (length(transList[["validList"]]) == 0) {
      valid <- FALSE
      message <- append(message,
             paste(
               "No valid transitions directory in risk factor directory",
               rfDir
             ))
    }
    
  }
  
  
  if (length(transList[["invalidList"]]) != 0) {
    warningMessage <- append(
      warningMessage,
      paste(
        "XML file with wrong format in",
        rfDir,
        " use getRiskfactorfilesErrors for more info "
      )
    )
  }
  if (length(prevList[["invalidList"]]) != 0) {
    valid <- FALSE
    warningMessage <- append(
      warningMessage,
      paste(
        "XML file with wrong format in",
        rfDir,
        " use getRiskfactorfilesErrors for more info "
      )
    )
  }
  
  if (valid)
    errorMessage = 0
  rfData <-
    list(
      prevalenceFiles = prevList,
      transitionFiles = transList,
      nClasses = nClasses,
      type = type
    )
  attr(rfData, "valid") <- valid
  attr(rfData, "errors") <- errorMessage
  attr(rfData, "warnings") <- warningMessage
   return(rfData)
  
}



#' Reads and validates the riskfactor transitionfiles
#'
#' @param currentpath full path to the directory with riskfactor data
#'
#' @return list containing 1. list van valid transitionfiles 2. list of invalid transition files
#'
#'
#' @examples
#' 
#'currentpath <-  paste0(rfDir)
getTransitionFiles <- function(currentpath) {
  require(xml2)
  
  file.list <- list.files(paste0(currentpath,"/Transitions"), pattern = "\\.xml$")
  validList <- list()
  invalidList <- list()
  ## file.name <- file.list[[1]]
  for (file.name in file.list) {
    currentfile <- paste0(currentpath, "/Transitions/", file.name)
    doc <- read_xml(currentfile)
    type = NULL
    if ((xml_name(doc)) == "transitionmatrix_zero")
      type = "zero"
    if ((xml_name(doc)) == "transitionmatrix_netto")
      type = "netto"
    if ((xml_name(doc)) == "transitionmatrix")
      type = "user"
    if ((xml_name(doc)) == "transitiondrift_zero")
      type = "zerodrift"
    if ((xml_name(doc)) == "transitiondrift_netto")
      type = "nettodrift"
    if ((xml_name(doc)) == "transitiondrift")
      type = "userdrift"
    
    if (type == "zero"){
      
      xsd.filename <- system.file("extdata","schemas","transitionmatrix_zero.xsd", package = "hiaR")
      xsd <- read_xml(xsd.filename)
      valid <- xml_validate(doc, xsd)}
    if (type == "netto"){
      xsd.filename <- system.file("extdata","schemas","transitionmatrix_netto.xsd", package = "hiaR")
      xsd <- read_xml(xsd.filename)
      valid <- xml_validate(doc, xsd)
      }
    if (type == "user"){
      xsd.filename <- system.file("extdata","schemas","transitionmatrix.xsd", package = "hiaR")
      xsd <- read_xml(xsd.filename)
      valid <- xml_validate(doc, xsd)
    }
    if (type == "zerodrift"){
      xsd.filename <- system.file("extdata","schemas","transitiondrift_zero.xsd", package = "hiaR")
      xsd <- read_xml(xsd.filename)
      valid <- xml_validate(doc, xsd)
    }
    if (type == "nettodrift"){
      xsd.filename <- system.file("extdata","schemas","transitiondrift_netto.xsd", package = "hiaR")
      xsd <- read_xml(xsd.filename)
      valid <- xml_validate(doc, xsd)
    }
    if (type == "userdrift"){
      xsd.filename <- system.file("extdata","schemas","transitiondrift.xsd", package = "hiaR")
      xsd <- read_xml(xsd.filename)
      valid <- xml_validate(doc, xsd)
    }
    if (is.null(type)) {
      valid <- c(FALSE)
      attr(valid, "errors") <-
        "transition file has incorrect root tag, should be one of <transitionmatrix>, <transitionmatrix_zero> or <transitionmatrix_netto>"
    }
    
    if (valid)
      validList <- append(validList, gsub("\\.XML","", gsub("\\.xml","",file.name)))     else {
      invalidFile <- list(gsub("\\.xml","",file.name))
      attr(invalidFile, "errors") <- attr(valid, "errors")
      invalidList <- append(invalidList, invalidFile)
    }
  }
  return(list(validList=validList, invalidList=invalidList))
}




##################################################################################

##   Relative risk, only for valid diseases and riskfactors

##################################################################################

#' Gets a dataframe containing the relative risk files for valid risk factors and diseases
#'
#' @param dynamodir workdirectory Dynamo-Hia (full path)
#' @param disList the list with diseases
#' @param rfList the list with riskfactors
#'
#' @return a dataframe with variables from (disease or riskfactor), to (disease, death or disability),
#' filename (excluding path), valid (TRUE/FALSE) 
#' Only the relative risks pertaining to valid diseases and riskfactors are included. 
#' 
#' @export
#'
#' @examples
#'
getRRTable <- function(dynamodir, disList, rfList) {
  
  ### niet echt efficient, maar kleine omvang
  nDis <- length(disList$validList)
  nRF <- length(rfList$validList)
  dis=1 
  rf=1
  i=1
  for (dis in 1:nDis) for (rf in 1:nRF ){
    disName <- attr(disList$validList[[dis]],"name")
    rfName <- attr(rfList$validList[[rf]],"name")
    rfType <- rfList$validList[[rf]]$type
    rrDir <- paste0(dynamodir,"/Reference_Data/Diseases/",disName,"/Relative_Risks_From_Risk_Factor")
# read only files for this riskfactor (ending on -rfName)
    file.list <- list.files(rrDir, pattern =paste0(rfName, "\\.xml"))
    nrows <- length(file.list)
    rrTemp <- data.frame(from=rep(rfName, nrows),to=rep(disName,nrows),fileName=file.list,valid=rep(FALSE, nrows))
   if (nrows>0) for (i in 1:nrows) {
      currentfile <- paste0(rrDir, "/", file.list[[i]])
      doc <- read_xml(currentfile)
      if (rfType == "categorical"){
        xsd.filename <- system.file("extdata","schemas","relrisksfromriskfactor_categorical.xsd", package = "hiaR")
        xsd <- read_xml(xsd.filename)
        rrTemp[i,]$valid <- xml_validate(doc, xsd)}
      if (rfType == "continuous"){
        
        xsd.filename <- system.file("extdata","schemas","relrisksfromriskfactor_continuous.xsd", package = "hiaR")
        xsd <- read_xml(xsd.filename)
        rrTemp[i,]$valid <- xml_validate(doc, xsd)}
      
      if (rfType == "compound"){
        
        xsd.filename <- system.file("extdata","schemas","relrisksfromriskfactor_compound.xsd", package = "hiaR")
        xsd <- read_xml(xsd.filename)
        rrTemp[i,]$valid <- xml_validate(doc, xsd)}
      
      if (is.null(rfType)) {
        print("error in riskfactors table as riskfactor encountered without type")
      }
      
    }
    if (dis==1 & rf==1) rrDat <- rrTemp else rrDat <- rbind(rrDat,rrTemp)
  }
  
  ### rr mortality
  rf=1
  for (rf in 1:nRF ){
    rfName <- attr(rfList$validList[[rf]],"name")
    rfType <- rfList$validList[[rf]]$type
    rrDir <- paste0(dynamodir,"/Reference_data/Risk_factors/",rfName,"/Relative_Risks_For_Death")
    file.list <- list.files(rrDir, pattern = "\\.xml$")
    nrows <- length(file.list)
    rrTemp <- data.frame(from=rep(rfName, nrows),to=rep("death",nrows),fileName=file.list,valid=rep(FALSE, nrows))
    
     if (nrows>0) for (i in 1:nrows) {
      currentfile <- paste0(rrDir, "/", file.list[[i]])
      doc <- read_xml(currentfile)
      if (rfType == "categorical"){
        
        xsd.filename <- system.file("extdata","schemas","relrisksfordeath_categorical.xsd", package = "hiaR")
        xsd <- read_xml(xsd.filename)
        rrTemp[i,]$valid <- xml_validate(doc, xsd)}
        if (rfType == "continuous"){
        
        xsd.filename <- system.file("extdata","schemas","relrisksfordeath_continuous.xsd", package = "hiaR")
        xsd <- read_xml(xsd.filename)
        rrTemp[i,]$valid <- xml_validate(doc, xsd)}
      
      if (rfType == "compound"){
        
        xsd.filename <- system.file("extdata","schemas","relrisksfordeath_compound.xsd", package = "hiaR")
        xsd <- read_xml(xsd.filename)
        rrTemp[i,]$valid <- xml_validate(doc, xsd)
        if (!rrTemp[i,]$valid ) print(xml_validate(doc, xsd) )
        }
      
      if (is.null(rfType)) {
        print(paste("error in riskfactors table as riskfactor for death encountered without type. RF=",RFname))
      }
      
    }
    if (exists("rrDat")) rrDat <- rbind(rrDat,rrTemp) else rrDat <- rrTemp
     }
  
  ### rr disablity
  for (rf in 1:nRF ){
    rfName <- attr(rfList$validList[[rf]],"name")
    rfType <- rfList$validList[[rf]]$type
    rrDir <- paste0(dynamodir,"/Reference_data/Risk_factors/",rfName,"/Relative_Risks_For_Disability")
    file.list <- list.files(rrDir, pattern = "\\.xml$")
    nrows <- length(file.list)
    rrTemp <- data.frame(from=rep(rfName, nrows),to=rep("disability",nrows),fileName=file.list,valid=rep(FALSE, nrows))
    if (nrows>0) for (i in 1:nrows) {
      currentfile <- paste0(rrDir, "/", file.list[[i]])
      doc <- read_xml(currentfile)
      if (rfType == "categorical"){
        
        xsd.filename <- system.file("extdata","schemas","relrisksfordisability_categorical.xsd", package = "hiaR")
        xsd <- read_xml(xsd.filename)
        rrTemp[i,]$valid <- xml_validate(doc, xsd)}
      if (rfType == "continuous"){
        
        xsd.filename <- system.file("extdata","schemas","relrisksfordisability_continuous.xsd", package = "hiaR")
        xsd <- read_xml(xsd.filename)
        rrTemp[i,]$valid <- xml_validate(doc, xsd)}
      
      if (rfType == "compound"){
        
        xsd.filename <- system.file("extdata","schemas","relrisksfordisability_compound.xsd", package = "hiaR")
        xsd <- read_xml(xsd.filename)
        rrTemp[i,]$valid <- xml_validate(doc, xsd)}
      
      if (is.null(rfType)) {
        print(paste("error in riskfactors table as riskfactor for death encountered without type. RF=",RFname))
      }
      
    }
    if (exists("rrDat")) rrDat <- rbind(rrDat,rrTemp) else rrDat <- rrTemp
  }
  
  ### rr disease on disease
  i=6
  for (dis in 1:nDis ){
    disName <- attr(disList$validList[[dis]],"name")
   
    rrDir <- paste0(dynamodir,"/Reference_Data/Diseases/",disName,"/Relative_Risks_From_Diseases")
    file.list <- list.files(rrDir, pattern = "\\.xml$")
    nrows <- length(file.list)
    rrTemp <- data.frame(from=rep(NA, nrows),to=rep(disName,nrows),fileName=file.list,valid=rep(FALSE, nrows))
    
    if (nrows>0) for (i in 1:nrows) {
      currentfile <- paste0(rrDir, "/", file.list[[i]])
      subs <- unlist(strsplit(currentfile,"/"))
      file.name <- subs[length(subs)]
      subs2 <- unlist(strsplit(file.name,"\\."))[1]
      subs3 <- unlist(strsplit(subs2,"-"))
      fromName <- subs3[length(subs3)]
      rrTemp$from[i] <- fromName
      doc <- read_xml(currentfile)
      xsd.filename <- system.file("extdata","schemas","relrisksfromdisease.xsd", package = "hiaR")
      xsd <- read_xml(xsd.filename)
      rrTemp[i,]$valid <- xml_validate(doc, xsd)
      }
      
    if (exists("rrDat")) rrDat <- rbind(rrDat,rrTemp) else rrDat <- rrTemp
  }
  
  rrDat$fileName <- gsub("\\.xml","",rrDat$fileName)  
  rrDat$fileName <- gsub("\\.XML","",rrDat$fileName)  
  return(rrDat)
}

##################################################################################

##   low level general functions

##################################################################################



#' Title
#'
#' @param basedir full path to a base directory 
#' @param subdir path relative to the base directory of the directory to be scraped
#' @param xsd name of the schema
#'
#' @return list containing 1. a list of valid files in the directory
#'                         2. a list of invalid files in the directory (with errors added as attributes)
#'
#'
#' @examples
#' 
getMultipleFiles <- function(basedir, subdir, xsdname) {
  require(xml2)
  currentpath = paste0(basedir, subdir)
  file.list <- list.files(currentpath, pattern = "\\.xml$")
  validList <- list()
  invalidList <- list()
  for (file.name in file.list) {
    #print(file.name)
    currentfile <- paste0(currentpath, "/", file.name)
    doc <- read_xml(currentfile)
    xsd <- read_xml( system.file("extdata","schemas",paste0(xsdname,".xsd"), package = "hiaR"))
    valid <- xml_validate(doc, xsd)
    if (valid)        validList <- append(validList, gsub("\\.XML","", gsub("\\.xml","",file.name)))     else {
      invalidFile <- c(file.name)
      attr(invalidFile, "errors") <- attr(valid, "errors")
      attr(invalidFile, "warnings") <- attr(valid, "warnings")
      invalidList <- append(invalidList,gsub("\\.XML","", gsub("\\.xml","",file.name)))
    }
  }
  return(list(validList=validList, invalidList=invalidList))
}



#' validate a single file with a known name
#'
#' 
#' @param currentpath directory to read from 
#' @param filename name to read (excluding .xml)
#' @param xsdname name of the schema to validate the file 

#' @return list containing 1. a list of the valid file in the directory
#'                         2. a list of the invalid file in the directory (with errors added as attributes)
#'         although the list always only contain a single or no files, this structure is compatable with structures
#'         for other parts where multiple files are possible                     
#'
#'
#' @examples
#' currentpath <- "c:/temp/test/Reference_Data/Populations/testcountry"
#' filename <- "size"
#' xsdname <- "populationsize"
#' getSingleFile(currentpath, filename, xsdname)
#' 
#' currentpath <- "c:/temp/test/Reference_Data/Diseases/dis/Prevalences"
#' filename <- "testfile"
#' xsdname <- "diseaseprevalences"
#' 
getSingleFile <- function(currentpath, filename, xsdname) {
  require(xml2)
  file.list <-
    list.files(currentpath, pattern = paste0(filename, ".xml"))
  xsd.filename <- system.file("extdata","schemas",paste0(xsdname,".xsd"), package = "hiaR")
  
  xsd <- read_xml(xsd.filename)
  validList <- list()
  invalidList<- list()
  if (length(file.list) == 1) {
    currentfile <- paste0(currentpath, "/", filename,".xml")
    doc <- read_xml(currentfile)
    valid <- xml_validate(doc, xsd)
    if (valid)
      validList <- append(validList, filename)
    else {
      invalidFile <- c(paste0(filename,".xml"))
      attr(invalidFile, "errors") <- attr(valid, "errors")
      attr(invalidFile, "warnings") <- attr(valid, "warnings")
      invalidList <- append(invalidList, filename)
    }
  } else {
    invalidList <- append(invalidList, paste("No file with filename", paste0(filename,".xml")))
      attr(invalidFile, "errors") <-
        paste("No file with filename",
              filename,
              "in directory",
              currentpath)
    }
  
  
  return(list(validList=validList, invalidList=invalidList))
}
