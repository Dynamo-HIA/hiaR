#'###############################################################################
#'                                                                              #
#'                          XML writing functions                               #
#'                                                                              #
#'###############################################################################

## author: Hendriek Boshuizen
## functions to write XML files

## history of the code: using package XML in the past was very slow, so XML are just made as a string
## and then written using cat
## XML package is now obsolete and replaced by xml2
## it might be more elegant to rewrite using xml2, but has not been done as this works OK

## todo: maybe make wrap functions, so that you give the type of the riskfactor to the function
## write RR functions: make ncat 0 mean that this will be continuous plus extra input compound+FALSE 
## update the examples



#' Make an DYNAMO-HIA XML file with data per category, where the value is a value
#'
#' @param tag1 first level tag of XML
#' @param tag2 second level tag of XML
#' @param data dataframe with variables cat, age, sex and the value to be
#' used (in a variable with a name as given by varname) 
#' dataframe with variables age, sex, value to be used (as given by varname) 
#' where sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' 
#' @param ncat number of categories
#' @param varname the name of the variable with the value  (in quotes)
#'
#' @return string containing the xml
#'
#'
makeXML.age.sex.cat<-function(tag1,tag2,data,ncat, varname)
{ data <- as.data.frame(data)
string<-paste('<?xml version=\"1.0\" ?> <',tag1,">",sep="")
for (iage in 0:95){
  for (isex in 0:1) {
    for (icat in 1:ncat) {
      subdat <- subset(data, cat==icat & sex==isex & age==iage)
      if (nrow(subdat) != 1) simpleError(paste("no or to many data for age=",iage,"sex=",isex,"category=",icat,"for variable",varname))
      string <- paste(string, "<", tag2,"><age> ",as.character(iage), "</age>",
                      "<sex>",as.character(isex),"</sex>",
                      "<cat>",as.character(icat), "</cat>",
                      " <value>",as.character(subdat[,eval(varname)]),"</value>",
                      "</",tag2, ">",sep="")
    }}}
string<-paste(string, "</", tag1,">",sep="")
return(string)
}

#' Make an DYNAMO-HIA XML file with data per category, where the value is a percentage
#'
#' @param tag1 first level tag of XML
#' @param tag2 second level tag of XML
#' @param data  dataframe with variables cat (categories of riskfactor) age, sex, value to be used (as given by varname) 
#' where cat is coded with values 1 to ncat (without gaps), sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param ncat number of categories
#' @param varnamevarname the name of the variable with the value (in quotes) to be written
#' 
#'
#'
#' @return string containing the xml
#' 
#'
#' @examples
makeXML.age.sex.cat2<-function(tag1,tag2,data,ncat, varname)
{ data <- as.data.frame(data)
string<-paste('<?xml version=\"1.0\"?> <',tag1,">",sep="")
for (iage in 0:95){
  for (isex in 0:1) {
    for (icat in 1:ncat)  {
      subdat <- subset(data, cat==icat & sex==isex & age==iage)
      
      if (nrow(subdat) != 1) simpleError(paste("no or to many data for age=",iage,"sex=",isex,"category=",icat,"for variable",varname))
      
      string<-paste(string, "<", tag2,">  <age> ",as.character(iage), "</age>",
                    " <sex> ",as.character(isex), "</sex>",
                    " <cat> ",as.character(icat), "</cat>",
                    " <percent> ",as.character(subdat[,eval(varname)]), "</percent>",
                    "</",tag2, ">",sep="")
    }}}
string<-paste(string, "</", tag1,">",sep="")
return(string)
}


#' Make an DYNAMO-HIA XML file with a value as content
#'
#' @param tag1 first level tag of XML
#' @param tag2 second level tag of XML
#' @param data dataframe with variables age, sex, value to be used (as given by varname) 
#' where sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of the variable (in quotes) to be written
#'
#' @return string containing the xml

#'
#' @examples
#' 
makeXML.age.sex<-function(tag1,tag2,data, varname)
{ browser
  string<-paste('<?xml version=\"1.0\" ?> <',tag1,">",sep="")
  for (iage in 0:95){
    for (isex in 0:1) {
      subdat <- subset(data,sex==isex & age==iage)
      if (nrow(subdat) != 1) simpleError(paste("no or to many data for age=",iage,"sex=",isex,"for variable",varname))
      
      
      string<-paste(string, "<", tag2,">  <age> ",as.character(iage), "</age>",
                    " <sex>",as.character(isex),"</sex>",
                    " <value>",as.character(subdat[,eval(varname)]), "</value>",
                    "</",tag2, ">",sep="")
    }}
  string<-paste(string, "</", tag1,">",sep="")
  return(string)
}

#' Make an DYNAMO-HIA XML file with a percentage as the content
#'
#' @param tag1 first level tag of XML
#' @param tag2 second level tag of XML
#' @param data dataframe with variables age, sex, value to be used (as given by varname) 
#' where sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of the variable (in quotes) to be written
#'
#'
#' @return string containing the xml

#'
#' @examples

makeXML.age.sex2<-function(tag1,tag2,data, varname)
{ 
  string<-paste('<?xml version=\"1.0\" ?> <',tag1,">",sep="")
  for (iage in 0:95){
    for (isex in 0:1) {
      subdat <- subset(data,sex==isex & age==iage)
      if (nrow(subdat) != 1) simpleError(paste("no or to many data for age=",iage,"sex=",isex,"for variable",varname))
      string<-paste(string, "<", tag2,">  <age> ",as.character(iage), "</age>",
                    " <sex>",as.character(isex),"</sex>",
                    " <percent>",as.character(subdat[,eval(varname)]), "</percent>",
                    "</",tag2, ">",sep="")
    }}
  string<-paste(string, "</", tag1,">",sep="")
  return(string)
}


#' Make an DYNAMO-HIA input ML file with a number (integer) as the content
#'
#' @param tag1 first level tag of XML
#' @param tag2 second level tag of XML
#' @param data dataframe with variables age, sex, value to be used (as given by varname) 
#' where sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of the variable (in quotes) to be written
#'
#'
#' @return string containing the xml

#'
#' @examples

makeXML.age.sex3<-function(tag1,tag2,data, varname)
{ 
  string<-paste('<?xml version=\"1.0\" ?> <',tag1,">",sep="")
  for (iage in 0:95){
    for (isex in 0:1) {
      subdat <- subset(data,sex==isex & age==iage)
      if (nrow(subdat) != 1) simpleError(paste("no or to many data for age=",iage,"sex=",isex,"for variable",varname))
      string<-paste(string, "<", tag2,">  <age> ",as.character(iage), "</age>",
                    " <sex>",as.character(isex),"</sex>",
                    " <number>",as.character(format(subdat[,eval(varname)], scientific=F)), "</number>",
                    "</",tag2, ">",sep="")
    }}
  string<-paste(string, "</", tag1,">",sep="")
  return(string)
}

#' produces a string to be written to as configuration file for a categorical or compound risk factor
#'
#' @param nclass number of categories in the risk factor
#' @param classnames names of the categories
#' @param compound boolean: whether compound or not
#' @param referenceclass reference category (category for which the relative risk is equal to 1)
#' @param durationclass for compound risk factors: the number of the category for which the relative risk 
#' depends on how long the persons has been in the particular risk category,
#'

#' @return string to be written as configuration file for a categorical/compound risk factor (xml) 
#' 
#'
#' @examples
makeXML.rf.configuration<-function(nclass, classnames, compound, referenceclass=1, durationclass)
{ if (compound) string<-paste('<?xml version=\"1.0\" ?><riskfactor_compound><classes>',sep="") else
  string<-paste('<?xml version=\"1.0\" ?><riskfactor_categorical><classes>',sep="")
  for (icat in 1:nclass){
    string<-paste(string, "<class><flexdex>",as.character(icat),
                  "</flexdex><name>",as.character(classnames[icat]), "</name></class>",sep="") }
  
  string <- paste(string, "</classes><referenceclass>",as.character(referenceclass),"</referenceclass>",sep="")
  if (compound & is.null(durationclass)) stop("you need to give a duration class for a compound riskfactor",sep="") 
  if (compound)  paste(string, "</classes><durationclass>",as.character(durationclass),"</durationclass>",sep="")
  
  if (compound) string <- paste(string, "</riskfactor_compound>",sep="") else
   string <- paste(string,"</riskfactor_categorical>",sep="")
  
  return(string)
}


#' produces a string to be written to as configuration file for a continuous risk factor
#'
#' @param cutoffs Cut-off values used in output
#' @param referencevalue reference value (value of the risk factor for which the relative risk is equal to 1)
#'
#' @return string to be written as configuration file for a continuous risk factor (xml) 
#' 
#'
#' @examples
makeXML.rf.configuration2<-function(cutoffs, referencevalue=0)
{ 
  string<-paste('<?xml version=\"1.0\" ?><riskfactor_continuous><referencevalue>',
                as.character(referencevalue),"</referencevalue><cutoffs>", sep="")
  if (!is.null(cutoffs))  string<-paste(string, "<cutoffs/></riskfactor_continuous>",sep="")else {
for (icat in 1:length(cutoffs)){
  string<-paste(string, "<cutoff><flexdex>", as.character(icat),
                "</flexdex>,<value>",     as.character(cutoff[i]),
                "</value></cutoff>",sep="") }

string<-paste(string, "</cutoffs></riskfactor_continuous>",sep="")} 

return(string)
}



#'####################  for compound relative risk (general)       #####################



#' Writes the DYNAMO_HIA input XML file containing the relative risk of a categorical risk factor on diseae
#'
#'
#' 
#' @param tag1  main tag of xml
#' @param tag2  main tag of xml
#' @param ncat number of categories in the riskfactor
#' @param durcat number of the category with duration dependent relative risk
#' @param data dataframe with variables cat (categories of riskfactor), age, sex, value to be used (as given by varname) 
#' where cat is coded 1 to ncat (consecutive) sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of variable that contains the value of the relative risk
#' for the duration category this is the beginvalue #' relative risk on the disease for each category of the risk factor
#' @param varend, name of variable that contains the value of the RR at infinity 
#' @param alpha, name of variable that contains the value for alpha 
#' @return string with xml content
#' 
#'
#' @examples todo
#


makeXML.age.sex.compound <-
  function(tag1,
           tag2,
           ncat,
           durcat ,
           data,
           varname,
           varend,
           alpha)
  {
    data <- as.data.frame(data)
    string <- paste('<?xml version=\"1.0\" ?> <', tag1, ">", sep = "")
    for (iage in 0:95) {
      for (isex in 0:1) {
        for (icat in 1:ncat) {
          subdat <- subset(data, cat == icat & sex == isex & age == iage)
          if (nrow(subdat) != 1)
            simpleError(
              paste(
                "no or to many data for age=",
                iage,
                "sex=",
                isex,
                "category=",
                icat,
                "for variable",
                varname
              )
            )
          if (icat == durcat)
            valuestring <- paste(
              "<begin>",
              as.character(
                subdat[, eval(varname)]),
                "</begin><alfa>",
                as.character(
                  subdat[, eval(alpha)]),
                  "</alfa><end>",
                  as.character(subdat[, eval(varend)]),
                               "</end>", sep =
                                 "")
                  else
                    valuestring <-
                    paste(
                      "<begin>",
                      as.character(
                        subdat[, eval(varname)]),
                        "</begin><alfa>0",
                        "</alfa><end>",
                        as.character(subdat[, eval(varname)]),
                                     "</end>", sep =
                                       "")
                        
                        string <-    paste(
                          string,
                          "<",
                          tag2,
                          "><age> ",
                          as.character(iage),
                          "</age>",
                          "<sex>",
                          as.character(isex),
                          "</sex>",
                          "<cat>",
                          as.character(icat),
                          "</cat>",
                          valuestring,
                          "</",
                          tag2,
                          ">",
                          sep = ""
                        )
        }
      }
    }
    string <- paste(string, "</", tag1, ">", sep = "")
    return(string)
    
  }

################ for duration

#' Make an DYNAMO-HIA XML file with data per duration, where the value is a value
#'
#' @param tag1 first level tag of XML
#' @param tag2 second level tag of XML
#' @param data dataframe with variables duration, age, sex and the value to be
#' used (in a variable with a name as given by varname) 
#' dataframe with variables age, sex, value to be used (as given by varname) 
#' where sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' 
#' @param varname the name of the variable with the value  (in quotes)
#'
#' @return string containing the xml
#'
#'
makeXML.age.sex.duration<-function(data, varname)
{ data <- as.data.frame(data)
tag1 <-"riskfactorprevalences_duration"
tag2 <- "prevalence"
string<-paste('<?xml version=\"1.0\" ?> <',tag1,">",sep="")
for (iage in 0:95){
  for (isex in 0:1) {
    for (idur in 1:20) {
      subdat <- subset(data, duration==idur & sex==isex & age==iage)
      if (nrow(subdat) != 1) simpleError(paste("no or to many data for age=",iage,"sex=",isex,"duration=",idur,"for variable",varname))
      string <- paste(string, "<", tag2,"><age> ",as.character(iage), "</age>",
                      "<sex>",as.character(isex),"</sex>",
                      "<duration>",as.character(idur), "</duration>",
                      " <value>",as.character(subdat[,eval(varname)]),"</value>",
                      "</",tag2, ">",sep="")
    }}}
string<-paste(string, "</", tag1,">",sep="")
return(string)
}
#'#################################################################################
#'#################################################################################
#'#################################################################################

##  user function for writing     ######################################################################
##  to be used by users           ######################################################################

#'#################################################################################
#'#################################################################################
#'#################################################################################

#     riskfactor data

#'#################################################################################
#'#################################################################################
#'#################################################################################


##  risk factor prevalence categorical/compound ################################


## data:  3-dimensional array of dimension=c(ncat,96,2)
## dimension 1: risk factor level
## dimension 2: gender (1 = men, 2 = women)
## dimension 3: age (1 = 0 year,  2 = 1 year .... 96 = 95+ year)



#' Writes the DYNAMO_HIA input XML file  containing the riskfactor prevalence of a categorical (or compound) risk factor
#'
#' @param RFname = Risk factor name as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param ncat = number of categories in the riskfactor
#' @param dynamodir = DYNAMO-HIA work directory
#' @param filename = name to give to the riskfactor prevalence XML file (without XML extension)
#' @param data dataframe with variables age, sex, value to be used (as given by varname) 
#' where sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varnames, list with the names of variables (one for each risk factor category) that contains the value to put in file, namely the 
#' risk factor prevalence in percent (values between 0-100)
#' @param referenceclass the number of the referenceclass (numbered in order as given in varnames). 
#' If not given, the first category given in varnames is taken as the reference class 
#' @param compound Boolean indicating whether this is a compound risk factor 
#' @param durationclass the number of the category with a time dependent relative risk 
#' @return the function writes a file and does not return
#' @export
#'
#' @examples
#' # writing the prevalence (5%,10%,85% in men, 4%,6%,90% in women) of a 3 category riskfactor
#' # as well as the risk factor configurationfile
#' # this is written to the file testfile.xml in the dynamo-hia home directory c:\temp\test
#' data3<-array(0,c(3,2,96))
#' data3[1,1,]<-rep(5.1,96) # prevalence category 1 in men
#' data3[1,2,]<-rep(4.1,96) # prevalence category 1 in women
#' data3[2,1,]<-rep(10,96)  # prevalence category 2 in men
#' data3[2,2,]<-rep(6,96)   # prevalence category 2 in women
#' data3[3,1,]<-rep(85,96)  # prevalence category 3 in men
#' data3[3,2,]<-rep(90,96)  # prevalence category 3 in women
#' writeRiskFactorsPrev("RF",3,"c:/temp/test","testfile",data3) 
#' writeRiskFactorsPrev("RF",3,"/rivm/n/boshuizh/dynamo-hia/GUI","testfile",data3) 


writeRiskFactorsPrevCategorical <- function(RFname, ncat, dynamodir, filename , data, varnames, referenceclass=1, compound=FALSE,durationclass=NULL) 
{ require(dplyr)
  RFname<-gsub(" ","_", RFname)
  outdirDP<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                  "/Prevalences","/", sep="")
  filename<-gsub(" ","_", filename)
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  
  # reformat data in file where each category is a line
  data_long <- pivot_longer(data,cols=varnames, names_to = "catname", values_to = "rfprev")
  data_long$cat <- as.numeric(factor(data_long$catname, levels = varnames))
  ncat <- length(varnames)
  # write configuration
  dynamoconfig<-makeXML.rf.configuration(ncat, varnames, compound, referenceclass,durationclass)
  # write data
  dynamodat<-makeXML.age.sex.cat2("riskfactorprevalences_categorical","prevalence",data_long,ncat,"rfprev")
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamodat, file = outfilename, sep = "\n"  )
  cat(dynamoconfig, file =paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                                "/configuration.xml", sep=""), sep="\n")       
  return(NULL)
}

#'#  function to write prevalence files                   #####################
#'#  for the duration category of a compound risk factor  ####################



#' Writes the DYNAMO_HIA input XML file  containing the riskfactor prevalence of a categorical (or compound) risk factor
#'
#' @param RFname = Risk factor name as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param dynamodir = DYNAMO-HIA work directory
#' @param data dataframe with variables age, sex, duration and value to be used (as given by varname) 
#' where sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+ and 
#' duration has values 1 to 20, where 20 is 20 years and longer
#' @param varname, list with the name of variables (one for each risk factor category) that contains 
#' percentage in the duration class (values between 0-100)

#' @return the function writes a file and does not return
#' @export
#'
#' @examples
#' 

writeRiskFactorsDurationPrev <- function(RFname, dynamodir,  data,varname) 
{ require(dplyr)
  RFname<-gsub(" ","_", RFname)
  outdirDP<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                  "/DurationDistributions","/", sep="")
  
  outfilename<-paste(outdirDP,"durationprevalence",".xml" , sep="")
  
  # reformat data in file where each category is a line
  # write data
  dynamodat<-write.age.sex.duration(data,varname)
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamodat, file = outfilename, sep = "\n"  )
    return(NULL)
}


#####################  function to write prevalence files                   #####################
#####################  for a continuous risk factor                          ###################



#' Writes the DYNAMO_HIA input XML file containing the prevalence of a continuous risk factor
#'
#' @param RFname riskfactorname as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param dynamodir DYNAMO-HIA work directory
#' @param filename filename of the xml file to write the prevalence to (without the extension .xml)
#' @param data dataframe with variables age, sex, and variable names as given below by varnames 
#' #' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param cutoffs list with cutoff values to be used. Default: the mean over all ages
#' @param varname name of the variable in the dataframe that contains the mean value of the 
#' @param acutely_fatal name of the variable in the dataframe that contains the values for the acutely
#' fatal fraction (default: zero)
#' in the file, namely the DALY weight (values between 0 and 100)
#' @param cured_fraction name of the variable in the dataframe that contains the values for the acutely
#' fatal fraction (values between 0 and 100). Defaults to zero
#' @param isnormal boolean indicating whether the distribution is normal




#' @param type one of "Rate" or "Median Survival"
#'
#' @return NULL
#' @export
#'
#' @examples

writeRiskFactorsPrevCont<-function(RFname, dynamodir, filename , data, varname, stdev, skewness=NULL, type="Normal",referencevalue=NULL, cutoffs=NULL) 
{
  RFname<-gsub(" ","_", RFname)
  outdirDP<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                  "/Prevalences","/", sep="")
  outdirDP2<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                  "/", sep="")
  filename<-gsub(" ","_", filename)
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  
  
  data <- as.data.frame(data)
  # write configuration
  if (is.null(referencevalue)) referencevalue <-  mean(data[,varname])
  config <- makeXML.rf.configuration2(cutoffs, referencevalue)
  # write data
  
  ## the information after tag "parametertype" is not read  by dynamo  so it does not matter
  ## what is given here
  if (sum(data[,eval(skewness)]!=0) > 0)  type ="Log normal" else type="Normal" 
  dynamo<- paste('<?xml version=\"1.0\" ?> <riskfactorprevalences_continuous>',
                 "<distributiontype>",type,"</distributiontype>", 
                  "<prevalences>", sep="")
  
  for (iage in 0:95){
    for (isex in 0:1) {
      subdat <- subset(data, age==iage & sex==isex)
      if (is.null(acutely_fatal)) ac <- 0 else ac <- subdat[,eval(acutely_fatal)]
      if (is.null(cured_fraction)) cf <- 0 else cf <- subdat[,eval(cured_fraction)]
      dynamo<-paste(dynamo, "<prevalence>",
                    "<age>", as.character(iage), "</age>",
                    "<sex>", as.character(isex),"</sex>",
                    "<mean>",as.character(subdat[,eval(varname)]),"</mean>",
                    "<standarddeviation>",as.character(subdat[,eval(stdev)]),"</standarddeviation>",
                    "<skewness>",as.character(subdat[,eval(skewness)]),"</skewness>","</prevalence>", sep="")
    }
  }
  dynamo<-paste(dynamo, "</prevalences></riskfactorprevalences_continuous>",sep="")
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  cat(config, file = paste(outdirDP2 ,
                                "configuration.xml", sep=""), sep="\n")       
  
}




#####################  function to write transition files           #####################
#####################  user specified categorical (and compound)    #####################

### nb verplicht varnames from and to 


#' Writes the DYNAMO_HIA input XML file containing the transition probabilities for 
#' a categorical or compound riskfactor
#' 
#' @param RFname riskfactorname as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param dynamodir DYNAMO-HIA work directory
#' @param filename filename of the xml file to write the prevalence to (without the extension .xml)
#' @param ncat number of risk factor classes
#' @param data dataframe with variables age, sex, to and from and variable name as given below by varname
#' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' from gives the category from which the transition goes to the category indicated by to
#' when to=from, the value of varname should be 100 
#' @param varname name of the variable in the data.frame that contains the transition probability
#'
#' @return NULL
#' @export 
#'
#' @examples
writeTransitionMatrix<-function(RFname, dynamodir, filename , ncat, data, varname) 
{
  
RFname<-gsub(" ","_", RFname)
outdirDP<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                "/transitions","/", sep="")
filename<-gsub(" ","_", filename)
outfilename<-paste(outdirDP,filename,".xml" , sep="")

data <- as.data.frame(data)
# write configuration
if (is.null(cutoffs)) cutoffs <-  mean(data[,varname])
if (is.null(referencevalue)) referencevalue <-  mean(data[,varname])
config <- makeXML.rf.configuration2(cutoffs, referencevalue)
# write data

## the information after tag "parametertype" is not read  by dynamo  so it does not matter
## what is given here
if (sum(data[,eval(skewness)]!=0) > 0)  type ="Log normal" else type="Normal" 
dynamo<- paste('<?xml version=\"1.0\" ?> <transitionmatrix>', sep="")

    for (iage in 0:95) {
      for (isex in 0:1) {
        for (icat1 in 1:ncat){ ## check if add up to 100 
          subdat <- subset(data, age==iage & sex==isex & from==icat1) 
           if(sum(subdat[,eval(varname)]!= 100)) stop("for age ",iage," sex = ",isex," and category",
                       icat1,"transitionrate out of the category do not add up to 100 but to",
                       sum(subdat[,eval(varname)]!= 100))
          for (icat2 in 1:ncat) {
            subdat <- subset(subdat, to==icat2)
        dynamo<-paste(dynamo, "<transition>",
                  "<age>", as.character(iage), "</age>",
                  "<sex>", as.character(isex),"</sex>",
                  "<from>",as.character(icat1),"</from>",
                  "<to>",as.character(icat2),"</to>",
                  "<percent>",as.character(subdat[,eval(varname)]),"</percent>","</transition>", sep="")
  }
}}}
dynamo<-paste(dynamo, "</transitionmatrix>",sep="")

if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
cat(dynamo, file = outfilename, sep = "\n"  )
   

}




#####################  function to write transition files           #####################
#####################  user specified categorical (and compound)    #####################

### nb verplicht varnames from and to 

#' Write the transitions (drift) for a continuous riskfactor
#'
#' @param RFname riskfactorname as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param dynamodir DYNAMO-HIA work directory
#' @param filename filename of the xml file to write the prevalence to (without the extension .xml)
#' @param data dataframe with variables age, sex, and variable names as given below by varnames 
#' #' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname name in the dataset given above that contains the drift of the riskfactor
#' defined as the increase when the subject ages one year
#'
#' @return NULL a file is written
#' @export
#'
#' @examples
writeTransitionsCont<-function(RFname, dynamodir, filename , data, varname) 
{
RFname<-gsub(" ","_", RFname)
filename<-gsub(" ","_", filename)
outdirDP<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                "/transitions","/", sep="")
outfilename<-paste(outdirDP,filename,".xml" , sep="")


data <- as.data.frame(data)
# write configuration
if (is.null(cutoffs)) cutoffs <-  mean(data[,varname])
if (is.null(referencevalue)) referencevalue <-  mean(data[,varname])
config <- makeXML.rf.configuration2(cutoffs, referencevalue)
# write data

## the information after tag "parametertype" is not read  by dynamo  so it does not matter
## what is given here
if (sum(data[,eval(skewness)]!=0) > 0)  type ="Log normal" else type="Normal" 
dynamo<- paste('<?xml version=\"1.0\" ?> <transitiondrift>', sep="")

    for (iage in 0:95) {
      for (isex in 0:1) {
          subdat <- subset(data, age==iage & sex==isex ) 
          dynamo<-paste(dynamo, "<transition>",
                  "<age>", as.character(iage), "</age>",
                  "<sex>", as.character(isex),"</sex>",
                 "<mean>",as.character(subdat[,eval(varname)]),"</mean>","</transition>", sep="")
  }
}
dynamo<-paste(dynamo, "</transitiondrift>",sep="")
if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
cat(dynamo, file = outfilename, sep = "\n"  )
   

}



#'####################  function to write transition files           #####################
#'####################  nett for cat and duration                      #####################
#'
#' Write the net transition file for a categorical or compound riskfactor
#'
#' @param RFname riskfactorname as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param dynamodir DYNAMO-HIA work directory
#' @param filename filename of the xml file to write the prevalence to (without the extension .xml)
#'
#' @return
#' @export
#'
#' @examples

writeNetTransition <- function(RFname, dynamodir, filename ) {
  RFname<-gsub(" ","_", RFname)
  filename<-gsub(" ","_", filename)
  outdirDP<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                  "/Transitions","/", sep="")
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat("<transitionmatrix_netto/>", file = outfilename, sep = "\n"  )
  
}
  



##  function to write transition files           #####################
##  netto continuous riskfactors            #####################


#' write the nett-transitions file (nett drift) for a continuous riskfactor
#'
#' @param RFname riskfactorname as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param dynamodir DYNAMO-HIA work directory (full path)
#' @param filename filename of the xml file to write the transition data (without the extension .xml)
#' @param trend trend to be applied on top of a netto drift
#'
#' @return
#' @export
#'
#' @examples
writeNetTransitionCont <-function(RFname, dynamodir, filename, trend ) {
  RFname<-gsub(" ","_", RFname)
  filename<-gsub(" ","_", filename)
  outdirDP<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                  "/Transitions","/", sep="")
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  dynamo <- paste("<transitiondrift_netto><trend>",as.character(trend),
   "</trend></transitiondrift_netto>",sep="")
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}



#####################  function to write transition files           #####################
#####################  zero cat and duration                               #####################

#' Write the zero transitions for a categorical or compound riskfactor
#'
#' @param RFname riskfactorname as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param dynamodir DYNAMO-HIA work directory
#' @param filename filename of the xml file to write the transition rates (without the extension .xml)
#'
#' @return NULL
#' @export
#'
#' @examples
writeZeroTransition <-function(RFname, dynamodir, filename ) {
  RFname<-gsub(" ","_", RFname)
  filename<-gsub(" ","_", filename)
  outdirDP<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                  "/Transitions","/", sep="")
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat("<transitionmatrix_zero/>", file = outfilename, sep = "\n"  )
  
}


#####################  function to write transition files           #####################
#####################  zero continuous                               #####################


#' Write the zero transitions (drift) for a continuous riskfactor
#'
#' @param RFname riskfactorname as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param dynamodir DYNAMO-HIA work directory
#' @param filename filename of the xml file to write the transition rates (without the extension .xml)
#'
#' @return
#' @export
#'
#' @examples
writeZeroTransitionCont <-function(RFname, dynamodir, filename ) {
  RFname<-gsub(" ","_", RFname)
  filename<-gsub(" ","_", filename)
  outdirDP<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                  "/Transitions","/", sep="")
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat("<transitiondrift_zero/>", file = outfilename, sep = "\n"  )
  
}



#'#################################################################################
#'#################################################################################
#'#################################################################################

## relative risks

#'#################################################################################
#'#################################################################################
#'#################################################################################




#'####################  function to write  RR for a           #####################
#'####################  compound riskfactor on mortality      #####################


#' Writes the DYNAMO_HIA input XML file containing the relative risks (RR) from a riskfactor on all cause mortality
#'
#' @param RFname name if the risk factor as used in DYNAMO-HIA (any spaces will be replace by underscores)
#' @param ncat number of categories in the riskfactor
#' @param durcat number of the categories where the relative risk is time dependent
#' @param dynamodir DYNAMO-HIA work directory 
#' @param filename name to give to the relative risk XML file (without the prescibed riskfactor name and the XML extension)
#' @param data dataframe with variables cat (categories of riskfactor), age, sex, value to be used (as given by varname) 
#' where cat is coded 1 to ncat (consecutive) sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname name of variable that contains the relative risk on all cause mortality for each category of the risk factor
#' For the category where the relative risk is time dependent, this contains the relative risk at the start of exposure
#' that is, the moment the subject enters in this risk category
#' @param RRend name of the variable containing the relative risk after an infinitely long time in the risk category with
#' @param alpha name of the variable containing the alpha parameter (rate of change) of the time dependent relative risk
#' the time dependent relative risk
#' @return NULL
#' @export
#'
#' @examples
#' 
writeRRMortalityCompound<-function(RFname, ncat, durcat, dynamodir, filename , data, varname,RRend, alpha) 
{  RFname<-gsub(" ","_", RFname) 
  filename<-gsub(" ","_", filename)
  outdirRRD<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                   "/Relative_Risks_For_Death","/", sep="")
  outfilename<-paste(outdirRRD,filename,".xml" , sep="")
  
  # write data
  dynamo<-makeXML.age.sex.compound("relrisksfordeath_compound","relriskfordeath",data,ncat, durcat,varname,RRend,alpha)
  
  if (!file.exists(outdirRRD)) dir.create(outdirRRD,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}


#####################  function to write OR for a             #####################
#####################  compound riskfactor on disability      #####################
#####################  is the same as for categorical      #####################

          ## not needed ###

#####################  function to write  RR for a          #####################
#####################  compound riskfactor on a disease     #####################



#' Writes the DYNAMO_HIA input XML file containing the odds ratios (OR) from a riskfactor on all cause disability
#'
#' @param diseasename name if the disease as used in DYNAMO-HIA.Any spaces in the name will be replaced by _
#' @param RFname name if the risk factor as used in DYNAMO-HIA
#' @param ncat number of categories in the riskfactor
#' @param durcat number of the categories where the relative risk is time dependent
#' @param dynamodir DYNAMO-HIA work directory 
#' @param filename name to give to the relative risk XML file (without the prescibed riskfactor name and the XML extension)
#' @param data dataframe with variables cat (categories of riskfactor), age, sex, value to be used (as given by varname) 
#' where cat is coded 1 to ncat (consecutive) sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname name of variable that contains the relative risk on all cause mortality for each category of the risk factor
#' For the category where the relative risk is time dependent, this contains the relative risk at the start of exposure
#' that is, the moment the subject enters in this risk category
#' @param RRend name of the variable containing the relative risk after an infinitely long time in the risk category with
#' @param alpha name of the variable containing the alpha parameter (rate of change) of the time dependent relative risk
#' the time dependent relative risk
#' @return NULL
#' @export
#'
#' @examples
#' 
writeRRDiseaseCompound<-function(diseasename, RFname, ncat, durcat, dynamodir, filename , data, varname,RRend, alpha) 
{ diseasename<-gsub(" ","_", diseasename)
  RFname<-gsub(" ","_", RFname)
  filename<-gsub(" ","_", filename)
  outdirRRD<-paste(dynamodir,"/Reference_Data/Diseases/",diseasename,
                     "/Relative_Risks_From_Risk_Factor","/", sep="")  
  outfilename <- paste(outdirRRD,filename,"-",RFname,".xml" , sep="")
 
  # write data
  dynamo<-makeXML.age.sex.compound("relrisksfordisease_compound","relriskfordisease",data,ncat, durcat,varname,RRend,alpha)
  
  if (!file.exists(outdirRRD)) dir.create(outdirRRD,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}


#####################  function to write  RR for a             #####################
#####################  continuous riskfactor on mortality      #####################


#' Writes the DYNAMO_HIA input XML file containing the relative risks (RR) from a riskfactor on all cause mortality
#'
#' @param RFname = name if the risk factor as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param ncat number of categories in the riskfactor
#' @param dynamodir = DYNAMO-HIA work directory 
#' @param filename = name to give to the relative risk XML file (without the prescibed riskfactor name and the XML extension)
#' @param data dataframe with variables cat (categories of riskfactor), age, sex, value to be used (as given by varname) 
#' where cat is coded 1 to ncat (consecutive) sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of variable that contains the value to put in file, namely the 
#' relative risk on all cause mortality for each category of the risk factor
#' #'
#' @return NULL
#' @export
#'
#' @examples
#' ## writing the relative risks on mortality for a 3 category risk factor named RF
#' ## this is written to the file RRmortFromStudyX-RF.xml in the right place in the 
#' ## dynamo-hia home directory c:\temp\test
#' data3<-array(0,c(3,2,96))
#' data3[1,1,]<-rep(1,96) # relative risk for category 1 in men
#' data3[1,2,]<-rep(1,96) # relative risk for category 1 in women
#' data3[2,1,]<-rep(1.1,96)  # relative risk for category 2 in men
#' data3[2,2,]<-rep(1.15,96)   # relative risk for category 2 in women
#' data3[3,1,]<-rep(1.2,96)  # relative risk for category 3 in men
#' data3[3,2,]<-rep(1.3,96)  # relative risk for category 3 in women
#' writeRiskFactorsPrev("RF",3,"c:/temp/test","testfile",data3) 
#' writeRRMortality("RF",3,"c:/temp/test", "RRmortFromStudyX",  data2) 
#' 
writeRRMortalityCont<-function(RFname, ncat, dynamodir, filename , data, varname) 
{  RFname<-gsub(" ","_", RFname) 
  filename<-gsub(" ","_", filename)
  outdirRRD<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                   "/Relative_Risks_For_Death","/", sep="")
  outfilename<-paste(outdirRRD,filename,".xml" , sep="")
  
  # write data
  dynamo<-makeXML.age.sex("relrisksfromriskfactor_continuous","relativerisk",data, varname)
  
  if (!file.exists(outdirRRD)) dir.create(outdirRRD,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}


#####################  function to write OR for a               #####################
#####################  continuous riskfactor on disability      #####################




## data:  3-dimensional array of dimension=c(ncat,2,96)
## dimension 1: risk factor level
## dimension 2: gender (1 = men, 2 = women)
## dimension 3: age (1 = 0 year,  2 = 1 year .... 96 = 95+ year)



#' Writes the DYNAMO_HIA input XML file containing the odds ratios (ORs) of a risk factor on disability
#'
#' @param RFname = name of the risk factor as used in DYNAMO-HIA (spaces will be replace by underscore)
#' @param dynamodir = DYNAMO-HIA work directory 
#' @param filename filename under which to write the OR on disability XML file (without the riskfactor name and XML extension) 
#' @param data dataframe with variables cat (categories of riskfactor), age, sex, value (of the oddsratio) to be used (as given by varname) 
#' where sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of variable that contains the value to put in file, namely the odds ratios from the risk factor on disability
#'  
#'
#' @return NULL
#' @export
#'
#' @examples to do
#' 
#'  

writeORdisabilityCont<-function(RFname, dynamodir, filename , data, varname) 
{ RFname<-gsub(" ","_", RFname)
filename<-gsub(" ","_", filename)
  outdirRRD<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                   "/Odds_Ratios_For_Disability","/", sep="")
  outfilename<-paste(outdirRRD,filename,".xml" , sep="")
  
  # write data
  dynamo<-makeXML.age.sex("relrisksfordisability_categorical","relriskfordisability",data,varname)
  
  if (!file.exists(outdirRRD)) dir.create(outdirRRD,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}


#####################  function to write  RR for a            #####################
#####################  continuous riskfactor on a disease     #####################

## data:  3-dimensional array of dimension=c(ncat,2,96)
## dimension 1: risk factor level
## dimension 2: gender (1 = men, 2 = women)
## dimension 3: age (1 = 0 year,  2 = 1 year .... 96 = 95+ year)


#' Writes the DYNAMO_HIA input XML file containing the relative risk of a categorical risk factor on diseae
#'
#'
#' @param diseasename Disease name as used in DYNAMO-HIA
#' @param RFname Risk Factor name as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param ncat number of categories in the riskfactor
#' @param dynamodir = DYNAMO-HIA work directory
#' @param filename = name to give to the relative risk XML file (without XML extension and excluding 
#' the risk factor name, this will be added automatically)
#' @param data dataframe with variables cat (categories of riskfactor), age, sex, value to be used (as given by varname) 
#' where cat is coded 1 to ncat (consecutive) sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of variable that contains the value to put in file, namely the 
#' relative risk on the disease for each category of the risk factor
#'
#' @return
#' @export
#'
#' @examples todo


writeRRriskfactorCont <-
  function(diseasename, RFname, dynamodir, filename , data ,varname)
    { diseasename<-gsub(" ","_", diseasename)
      RFname<-gsub(" ","_", RFname)
      filename<-gsub(" ","_", filename)
  diseasename<-gsub(" ","_", diseasename)
    outdirDP<-paste(dynamodir,"/Reference_Data/Diseases/",diseasename,
                    "/Relative_Risks_From_Risk_Factor","/", sep="")
    
    outfilename<-paste(outdirDP,filename,"-",RFname,".xml" , sep="")
    
    # write data
    
    dynamo<-makeXML.age.sex("relrisksfromriskfactor_categorical","relativerisk",data, varname)
    
    if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
    cat(dynamo, file = outfilename, sep = "\n"  )
    ##  print(paste(outfilename, "written"))
  }	


#####################  function to write  RR for a       #####################
#####################  riskfactor on all cause mortality #######################################



#' Writes the DYNAMO_HIA input XML file containing the relative risks (RR) from a riskfactor on all cause mortality
#'
#' @param RFname = name if the risk factor as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param ncat number of categories in the riskfactor
#' @param dynamodir = DYNAMO-HIA work directory 
#' @param filename = name to give to the relative risk XML file (without the prescibed riskfactor name and the XML extension)
#' @param data dataframe with variables cat (categories of riskfactor), age, sex, value to be used (as given by varname) 
#' where cat is coded 1 to ncat (consecutive) sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of variable that contains the value to put in file, namely the 
#' relative risk on all cause mortality for each category of the risk factor
#' #'
#' @return NULL
#' @export
#'
#' @examples
#' ## writing the relative risks on mortality for a 3 category risk factor named RF
#' ## this is written to the file RRmortFromStudyX-RF.xml in the right place in the 
#' ## dynamo-hia home directory c:\temp\test
#' data3<-array(0,c(3,2,96))
#' data3[1,1,]<-rep(1,96) # relative risk for category 1 in men
#' data3[1,2,]<-rep(1,96) # relative risk for category 1 in women
#' data3[2,1,]<-rep(1.1,96)  # relative risk for category 2 in men
#' data3[2,2,]<-rep(1.15,96)   # relative risk for category 2 in women
#' data3[3,1,]<-rep(1.2,96)  # relative risk for category 3 in men
#' data3[3,2,]<-rep(1.3,96)  # relative risk for category 3 in women
#' writeRiskFactorsPrev("RF",3,"c:/temp/test","testfile",data3) 
#' writeRRMortality("RF",3,"c:/temp/test", "RRmortFromStudyX",  data2) 
#' 
writeRRMortality<-function(RFname, ncat, dynamodir, filename , data, varname) 
{    RFname<-gsub(" ","_", RFname)
  filename<-gsub(" ","_", filename)
  outdirRRD<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                   "/Relative_Risks_For_Death","/", sep="")
  outfilename<-paste(outdirRRD,filename,".xml" , sep="")
  
  # write data
  dynamo<-makeXML.age.sex.cat("relrisksfordeath_categorical","relriskfordeath",data,ncat, varname)
  
  if (!file.exists(outdirRRD)) dir.create(outdirRRD,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}





#####################  function to write  the odds ratio for a       #####################
#####################  riskfactor (both categorical and compound) on all cause disability #######################################



#' Writes the DYNAMO_HIA input XML file containing the odds ratios (ORs) of a risk factor on disability
#'
#' @param RFname = name of the risk factor as used in DYNAMO-HIA. Any spaces in the name will be replaced by _
#' @param ncat number of categories in the riskfactor
#' @param dynamodir = DYNAMO-HIA work directory 
#' @param filename filename under which to write the OR on disability XML file (without the riskfactor name and XML extension) 
#' @param data dataframe with variables cat (categories of riskfactor), age, sex, value to be used (as given by varname) 
#' where cat is coded 1 to ncat (consecutive) sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of variable that contains the value to put in file, namely the odds ratios from the risk factor on disability
#'  
#'
#' @return NULL
#' @export
#'
#' @examples to do
#' ## Write the file with odds ratios for (all cause) disability for the 3 category risk factor RF to the file
#' testfile-RF.xml at the right place in the dynamo-hia home  directory C:\temp\test 
#' data3[1,1,]<-rep(1,96) # Odds ratio for category 1 in men
#' data3[1,2,]<-rep(1,96) # Odds ratio for category 1 in women
#' data3[2,1,]<-rep(1.1,96)  # Odds ratio for category 2 in men
#' data3[2,2,]<-rep(1.2,96)  # Odds ratio for category 2 in women
#' data3[3,1,]<-rep(1.3,96)  # Odds ratio for category 3 in men
#' data3[3,2,]<-rep(1.4,96)  # Odds ratio for category 3 in women
#
#' writeORdisability("RF",3,"c:\\temp/test","testfile",data3) 
#'  

writeORdisability<-function(RFname, ncat, dynamodir, filename , data, varname, compound=FALSE) 
{    RFname<-gsub(" ","_", RFname)
   filename<-gsub(" ","_", filename)
  outdirRRD<-paste(dynamodir,"/Reference_Data/Risk_Factors/",RFname,
                   "/Odds_Ratios_For_Disability","/", sep="")
  outfilename<-paste(outdirRRD,filename,".xml" , sep="")
  
  # write data
  dynamo<-makeXML.age.sex.cat("relrisksfordisability_categorical","relriskfordisability",data,ncat,varname)
  
  if (!file.exists(outdirRRD)) dir.create(outdirRRD,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}

#####################  function to write  RR for a       #####################
#####################  categorical riskfactor on disease #####################



#' Writes the DYNAMO_HIA input XML file containing the relative risk of a categorical risk factor on diseae
#'
#'
#' @param diseasename Disease name as used in DYNAMO-HIA
#' @param riskfactorname Risk Factor name as used in DYNAMO-HIA
#' @param ncat number of categories in the riskfactor
#' @param dynamodir = DYNAMO-HIA work directory
#' @param filename = name to give to the relative risk XML file (without XML extension and excluding 
#' the risk factor name, this will be added automatically)
#' @param data dataframe with variables cat (categories of riskfactor), age, sex, value to be used (as given by varname) 
#' where cat is coded 1 to ncat (consecutive) sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of variable that contains the value to put in file, namely the 
#' relative risk on the disease for each category of the risk factor
#'
#' @return
#' @export
#'
#' @examples todo

writeRRriskfactorCat <-
  function(diseasename, riskfactorname, ncat, dynamodir, filename , data ,varname) {
    diseasename<-gsub(" ","_", diseasename)
    filename<-gsub(" ","_", filename)
    outdirDP<-paste(dynamodir,"/Reference_Data/Diseases/",diseasename,
                    "/Relative_Risks_From_Risk_Factor","/", sep="")
  #  print(outdirDP)
  #  print(varname)
    outfilename<-paste(outdirDP,filename,"-",riskfactorname,".xml" , sep="")
    ##	outfilename<-gsub(" ","", outfilename)
    ##	outfilename<-gsub("no.","no", outfilename)
    
    
    # write data
    
    
    dynamo<-makeXML.age.sex.cat("relrisksfromriskfactor_categorical","relativerisk",data,ncat, varname)
    
    if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
    cat(dynamo, file = outfilename, sep = "\n"  )
    ##  print(paste(outfilename, "written"))
  }	


#######################   function to write     ###############################
####################################  RR disease on disease ####################

## in input array "data" has the following dimensions:
## dimension 1: gender (1 = men, 2 = women)
## dimension 2: age (1 = 0 year,  2 = 1 year .... 96 = 95+ year)


#' Writes the DYNAMO-HIA input xml file containing relative risks of a disease on another disease
#'
#' @param diseasename Disease name as used in DYNAMO-HIA of the disease that is influenced by the causal disease
#' @param causaldiseasename Disease name as used in DYNAMO-HIA of the disease influencing the other disease
#' @param dynamodir = DYNAMO-HIA work directory (full path)
#' @param filename  name to give to the relative risk XML file (without XML extension)
#' @param data  dataframe with variables age, sex, value to be used (as given by varname) 
#' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, namely the relative risk of the causal disease on the disease
#' in the file
#'
#' @return
#' @export
#'
#' @examples

writeRRdisease <-
  function(diseasename, causaldiseasename, dynamodir, filename , data, varname) 
  {  diseasename<-gsub(" ","_", diseasename)
     causaldiseasename<-gsub(" ","_", causaldiseasename)
     filename<-gsub(" ","_", filename)
  outdirDP<-paste(dynamodir,"/Reference_Data/Diseases/",diseasename,
                  "/Relative_Risks_From_Diseases","/", sep="")
  outfilename<-paste(outdirDP,filename,"-",causaldiseasename,".xml" , sep="")
  
  
  # write data
  dynamo<-makeXML.age.sex("relrisksfromdisease","relativerisk",data, varname)
  
  
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  
  cat(dynamo, file = outfilename, sep = "\n"  )
  
  }

#'#################################################################################
#'#################################################################################
#'#################################################################################

###                              disease data

#'#################################################################################
#'#################################################################################
#'#################################################################################




#'######################################################################   
#'#####################   function to write disease prevalence   ####### 
#'######################################################################	 


#' Writes the DYNAMO_HIA input XML file containing the prevalence of a disease 
#'
#' @param diseasename Disease name as used in DYNAMO-HIA
#' @param dynamodir = DYNAMO-HIA work directory
#' @param filename Filename under which to write the disease prevalence XML file (without XML extension)
#' @param data dataframe with variables age, sex, value to be used (as given by varname) 
#' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of variable that contains the value to put in file, namely the disease prevalence
#'
#' @return
#' @export
#'
#' @examples

writeDisPrevalence<-function(diseasename, dynamodir, filename , data, varname) 
{ diseasename<-gsub(" ","_", diseasename) 
  filename<-gsub(" ","_", filename)
outdirDP<-paste(dynamodir,"/Reference_Data/Diseases/",diseasename,
                "/Prevalences","/", sep="")
outfilename<-paste(outdirDP,filename,".xml" , sep="")

# write data
dynamo<-makeXML.age.sex2("diseaseprevalences","prevalence",data,varname)


if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
cat(dynamo, file = outfilename, sep = "\n"  )
}


#'######################################################################   
#'#####################     function to write disease incidence   ######
#'######################################################################   


#' Writes the DYNAMO_HIA input XML file containing the incidence of a disease
#'
#' @param diseasename Disease name as used in DYNAMO-HIA
#' @param dynamodir = DYNAMO-HIA work directory 
#' @param filename filename under which to write the XML file (without XML extension)
#' @param data dataframe with variables age, sex, value to be used (as given by varname) 
#' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of variable that contains the value to put in file, namely the disease incidence

#' @return NULL
#' @export
#'
#' @examples 


writeDisIncidence<-function(diseasename, dynamodir,  filename , data, varname) 
{
  diseasename<-gsub(" ","_", diseasename)
  filename<-gsub(" ","_", filename)
  outdirDP<-paste(dynamodir,"/Reference_Data/Diseases/",diseasename,
                  "/Incidences","/", sep="")
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  
  # write data
  dynamo<-makeXML.age.sex("diseaseincidences","incidence",data,varname)
  
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}

#'######################   function to write     ###############################
#'#####################  disease Excess mortality   #############################



#' Writes the DYNAMO_HIA input XML file containing the excess mortality of a disease
#'
#' @param diseasename Disease name as used in DYNAMO-HIA
#' @param dynamodir DYNAMO-HIA work directory
#' @param filename filename of the xml file containing the excess mortality data (without the extension .xml)
#' @param data dataframe with variables age, sex, and values to be used (with variable names as given by next parameters) 
#' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname name of the variable in the dataframe that contains the excess mortality values to be put
#' in the file, namely rate or median survival
#' @param acutely_fatal name of the variable in the dataframe that contains the values for the acutely
#' fatal fraction (should be NULL (default when no acutely fatal fraction is present))
#' in the file, namely the DALY weight (values between 0 and 100)
#' @param cured_fraction name of the variable in the dataframe that contains the values for the acutely
#' fatal fraction (values between 0 and 100). (should be NULL (default when no cured fraction is present))



#' @param type one of "Rate" or "Median Survival"
#'
#' @return NULL
#' @export
#'
#' @examples
writeExcessMort<-function(diseasename, dynamodir, filename , data, type="Rate", varname, acutely_fatal=NULL, cured_fraction=NULL) 
{ diseasename<-gsub(" ","_", diseasename)
  filename<-gsub(" ","_", filename)
  outdirDP<-paste(dynamodir,"/Reference_Data/Diseases/",diseasename,
                  "/Excess_Mortalities","/", sep="")
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  data <- as.data.frame(data)
  
  if(!is.null(acutely_fatal) & !is.null(cured_fraction) ) stop("a disease can not have both an acutely fatal fraction and a cured fraction")
  ## not clear if parameter type is really used in dynamo, but in case
  dynamo<- paste('<?xml version=\"1.0\" ?> <excessmortality>',
                 "<unittype>",type,"</unittype>", sep="") 
  if (!is.null(cured_fraction) )  dynamo<- paste(dynamo,
                 "<parametertype>Cured Fraction</parametertype>",
                 "<mortalities>", sep="") else
                     dynamo<- paste(dynamo, "<parametertype>Acutely Fatal</parametertype>",
                                                                 "<mortalities>", sep="") 
  for (iage in 0:95){
    for (isex in 0:1) {
      subdat <- subset(data, age==iage & sex==isex)
      if (is.null(acutely_fatal)) ac <- 0 else ac <- subdat[,eval(acutely_fatal)]
      if (is.null(cured_fraction)) cf <- 0 else cf <- subdat[,eval(cured_fraction)]
      dynamo<-paste(dynamo, "<mortality>",
                    "<age>", as.character(iage), "</age>",
                    "<sex>", as.character(isex),"</sex>",
                    "<unit>",as.character(subdat[,eval(varname)]),"</unit>",
                    "<acutelyfatal>",as.character(ac),"</acutelyfatal>",
                    "<curedfraction>",as.character(cf),"</curedfraction>",
                    "</mortality>","\n",sep="")
    }
  }
  dynamo<-paste(dynamo, "</mortalities></excessmortality>",sep="")
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
}


#'####################   function to write     ###############################
#'####################  disease DALY-weight #####################################
#'####################  this delivers DYNAMO-1 data ###########################


#' Writes the DYNAMO_HIA input XML file containing the DALY weights for a disease in DANOMO-I format (obsolete)
#'
#' @param diseasename Disease name as used in DYNAMO-HIA
#' @param dynamodir  = DYNAMO-HIA work directory
#' @param filename 
#' @param data dataframe with variables age, sex, value to be used (as given by varname) 
#' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname name of the variable in the dataframe that contains the values to be put
#' in the file, namely the DALY weight (values between 0 and 100)
#'
#' @return
#' this is not exported to prevent users using the wrong function
#'
#' @examples 

writeDisDaly<-function(diseasename, dynamodir, filename , data, varname) 
{   diseasename<-gsub(" ","_", diseasename)
  filename<-gsub(" ","_", filename)	
  outdirDP<-paste(dynamodir,"/Reference_Data/Diseases/",diseasename,
                  "/DALY_Weights","/", sep="")
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  
  dynamo<-makeXML.age.sex2("dalyweights","weight",data, varname)
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
  
  
}

#'######################   function to write     ###############################
#'####################  disease disability  #####################################
#' this delivers  DYNAMO-2 data #############################################


#' Writes the DYNAMO_HIA input XML file containing the DALYweights / percentage disability for a disease
#'
#' @param diseasename Disease name as used in DYNAMO-HIA
#' @param dynamodir DYNAMO-HIA work directory 
#' @param filename name of the xml file that contains the DALYweights / percentage disability (without the .xml extension) 
#' @param data dataframe with variables age, sex, value to be used (as given by varname) 
#' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, namelijk the percentage disability caused bythe disease
#' in the file
#' @return NULL
#' @export
#'
#' @examples

writeDiseaseDisability<-function(diseasename, dynamodir, filename , data, varname) 
{   diseasename<-gsub(" ","_", diseasename)
  filename<-gsub(" ","_", filename)  
  outdirDP<-paste(dynamodir,"/Reference_Data/Diseases/",diseasename,
                  "/Disability","/", sep="")
  outfilename<-paste(outdirDP,filename,".xml" , sep="")
  
  dynamo<-makeXML.age.sex2("dalyweights","weight",data, varname)
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
  
  
}


#'#################################################################################
#'#################################################################################
#'#################################################################################

##  population data

#'#################################################################################
#'#################################################################################
#'#################################################################################



#'###################### function to write ###############################
#'####################### population size ######################################


#' writes the DYNAMO-HIA input file containing the number of newborns
#'
#' @param dynamodir DYNAMO-HIA work directory 
#' @param popdir name of population
#' @param data a dataframe containing ages 0-95 (95 stands for 95+) and ses and a variable
#' given by varname containing the numbers in the population
#' @param varname  name of the variable containing the numbers in the population
#' @return NULL
#' @export
#'
#' @examples


writeSize<-function(dynamodir, popdir, data, varname)
{
  outdirDP<-paste(dynamodir,"/Reference_Data/Populations/",popdir,"/",sep="")
  outfilename<-paste(outdirDP,"size.xml" , sep="")
  # write data
  dynamo<-makeXML.age.sex3("populationsize","size",data, varname)
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n" )
}

#'######################   function to write     ###############################
#'#######################  newborns ######################################



#' writes the DYNAMO-HIA input file containing the number of newborns
#'
#' @param dynamodir DYNAMO-HIA work directory 
#' @param popdir name of population
#' @param sexratio ratio of male versus female children born
#' @param startingyear the first number in the file give the newborns for this year 
#' @param data a vector (one dimensional array) containing the newborn data starting in the year "startingyear"
#' only the first 30 elements are used. If it is shorter than 30, the other elements up to
#' 30 are filled with the last value of the vector  
#'
#' @return NULL
#' @export
#'
#' @examples

writeNewborn<-function(dynamodir, popdir, sexratio, startingyear, data) 
{  
  outdirDP<-paste(dynamodir,"/Reference_Data/Populations/",popdir,"/",sep="")
  outfilename<-paste(outdirDP,"newborns.xml" , sep="")
  
  # write data
  
  maxdata<- length(data) 
  
  dynamo<-'<?xml version=\"1.0\" ?> <newborns>'
  dynamo<-paste(dynamo,"<sexratio>", as.character(sexratio),"</sexratio>",
                "<startingYear>", as.character(startingyear) ,"</startingYear>",  "<amounts>",
                sep="")
  for (year in 0:29){
    
    
    dynamo<-paste(dynamo, "<amount>  <year> ",as.character(startingyear+year), "</year>",
                  sep="")        
    if (year< maxdata)  dynamo<-paste(dynamo," <number>",as.character(round(data[year+1])), "</number>",
                                      sep="") else
                                        dynamo<-paste(dynamo," <number>",as.character(round(data[maxdata])), "</number>",
                                                      sep="")
                                      dynamo<-paste(dynamo,"</amount>",sep="")
  }
  
  
  
  dynamo<-paste(dynamo,"</amounts>","</newborns>",sep="")
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}

#'######################   function to write     ###############################
#'######################    total mortality      ###############################


#' writes the DYNAMO_HIA input XML file containing the rates of all cause mortality 
#'
#' @param dynamodir = DYNAMO-HIA work directory
#' @param popdir = name of population
#' @param data  dataframe with variables age, sex, value to be used (as given by varname) 
#' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, namely the all cause mortality rate
#' in the file
#'
#' @return NULL
#' @export
#'
#' @examples

writeOverallMortality<-function(dynamodir, popdir,  data, varname) 
{ 
  outdirDP<-paste(dynamodir,"/Reference_Data/Populations/",popdir,"/",sep="")
  outfilename<-paste(outdirDP,"overallmortality.xml" , sep="")
  
  # write data
  dynamo<-makeXML.age.sex("overallmortality","mortality",data, varname)  
  
  
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}



#'######################   function to write     ###############################
#'#######################  total disability    #################################

## in input data files has the following dimensions:
## dimension 1: gender (1 = men, 2 = women)
## dimension 2: age (1 = 0 year,  2 = 1 year .... 96 = 95+ year)



#' Function to write the DYNAMO_HIA input XML file for Overall Disability
#'
#' @param dynamodir = DYNAMO-HIA work directory
#' @param popdir = name of population
#' @param data  dataframe with variables age, sex, value to be used (as given by varname) 
#' were sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, namely the overall disability in the population
#' in the file
#'
#' @return NULL
#' @export
#'
#' @examples 
#' 
writeOverallDisability<-function(dynamodir, popdir,  data, varname) 
{ 
  outdirDP<-paste(dynamodir,"/Reference_Data/Populations/",popdir,"/",sep="")
  outfilename<-paste(outdirDP,"overalldisability.xml" , sep="")
  
  # write data
  dynamo<-makeXML.age.sex2("overalldisability","weight",data, varname)  
  
  
  
  if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
  cat(dynamo, file = outfilename, sep = "\n"  )
  
}




#' Make an DYNAMO-HIA input ML file with a number (integer) as the content
#'
#' @param tag1 first level tag of XML
#' @param tag2 second level tag of XML
#' @param data dataframe with variables age, sex, value to be used (as given by varname) 
#' where sex is coded as (0 = men, 1 = women) and age has values 0 - 95 where 95 stands for 95+
#' @param varname, name of the variable (in quotes) to be written
#'
#'
#' @return string containing the xml

#'
#' @examples

makeXML.age.sex3<-function(tag1,tag2,data, varname)
{ 
  string<-paste('<?xml version=\"1.0\" ?> <',tag1,">",sep="")
  for (iage in 0:95){
    for (isex in 0:1) {
      subdat <- subset(data,sex==isex & age==iage)
      if (nrow(subdat) != 1) simpleError(paste("no or to many data for age=",iage,"sex=",isex,"for variable",varname))
      string<-paste(string, "<", tag2,">  <age> ",as.character(iage), "</age>",
                    " <sex>",as.character(isex),"</sex>",
                    " <number>",as.character(format(subdat[,eval(varname)], scientific=F)), "</number>",
                    "</",tag2, ">",sep="")
    }}
  string<-paste(string, "</", tag1,">",sep="")
  return(string)
}


#'####################################################################################
#'#
#'#   write the simulation configuration file
#'#
#'#####################################################################################


#' Write the configuration file for simulation with simulationName
#'
#' @param dynamodir Dynamo-HIA workdirectory
#' @param simulationName Name of the simulation
#' @param newborns Boolean: False= newborns added during simulation True: newborns added
#' @param startingyear startyear of simulation
#' @param yearsSimulated number of years to simulate
#' @param popSize number of simulated persons per age and sex group
#' @param minAge minimum age of simualted population
#' @param maxAge  maximum age of simulated population 
#' @param refScenarioName name of the reference (business as usual) scenario
#' @param randomSeed random seed used in the simulation
#' @param populationName name of the population to be used
#' @param scenarioList list containing the information per scenario (not including the reference scenario)
#' each element contains information on a single scenario, with named elements: 
#' name (name of scenario),
#' successRate: percentage of population to which the scenario is applied. 
#' minAge,maxAge: minimum and maximum age for which the scenario is applied.
#' targetSex: should scenario be applied to men (0), women (1) or both (2)
#' transitionFile (name of the transition file for the risk factor,
#' prevalenceFile: name of the prevalence file for the risk factor
#' 
#' name, transitionfilename and prevalencefilename
#' @param diseaseList list with one element per disease. Each element has the following named elements:
#' name, 
#' prevalenceFile: file with disease prevalence data, 
#' incidenceFile:file with disease incidence data, 
#' excessMortFile: file with diseaes excess mortality data, 
#' disabilityFile: file with disease disability data
#' @param riskfactorList list with named elements: 
#' name: name of the riskfactor,
#' transitionFile: name of the file with the transition for the riskfactor in the reference scenario,
#' prevalenceFile: name of the file with the starting prevalence for the riskfactor in the reference scenario
#' @param relriskList list with one element per relative risk. Each element has the following named elements:
#' name: name of the relative risk, 
#' from: name of the causal exposure (risk factor or a disease causing another disease), 
#' to: name of the effect (a disease, death or disability), 
#' filename: name of the file containing the relative risks
#'
#' @return
#' @export
#'
#' @examples
writeSimulationConfiguration<-function( dynamodir, simulationName, newborns, startingyear,yearsSimulated,popSize, minAge, maxAge,
                                           refScenarioName,randomSeed, populationName, scenarioList, diseaseList, riskfactorList, relriskList){
  
  outdirDP<-paste(dynamodir,"/Simulations/",simulationName, sep="")
  outfilename<-paste(outdirDP,"/configuration.xml" , sep="")
  # print(refScenarioName)
  # write configuration
  dynamoconfig <-  makeXML.simulation.configuration(newborns, startingyear,yearsSimulated,popSize, minAge, maxAge,
                                             refScenarioName,randomSeed, populationName, scenarioList, diseaseList, riskfactorList, relriskList)
if (!file.exists(outdirDP)) dir.create(outdirDP,recursive = TRUE)
 cat(dynamoconfig, file = outfilename, sep = "\n"  )  }







#' writes a string with the simulation configuration file
#' 
#' @param newborns 0: no newborns added during simulation 1: newborns added
#' @param startingyear startyear of simulation
#' @param yearsSimulated number of years to simulate
#' @param popSize number of simulated persons per age and sex group
#' @param minAge minimum age of simualted population
#' @param maxAge  maximum age of simulated population 
#' @param refScenarioName name of the reference (business as usual) scenario
#' @param randomSeed random seed used in the simulation
#' @param populationName name of the population to be used
#' @param scenarioList list with one element per scenario. Each element has the following named objects:
#' name, successRate, minAge, maxAge,  targetSex, transitionFile, prevalenceFile
#' @param diseaseList list with one element per disease. Each element has the following named objects:
#' name, prevalenceFile, incidenceFile, excessMortFile, disabilityFile
#' @param riskfactorList list with named elements: name, transitionFile, prevalenceFile
#' @param relriskList list with one element per relative risk. Each element has the following named elements:
#' from, to, filename
#'
#' @return
#' 
#'
#' @examples
makeXML.simulation.configuration<-function(newborns, startingyear,yearsSimulated,popSize, minAge, maxAge,
                                           refScenarioName,randomSeed, populationName, scenarioList, diseaseList, riskfactorList, relriskList)
{ 
  
  string<-paste0('<?xml version=\"1.0\" ?><simulation>\n<hasnewborns>',
                ifelse (newborns,"true","false"),'</hasnewborns>\n<startingYear>',
                as.character(startingyear),'</startingYear> \n<numberOfYears>',
                as.character(yearsSimulated),'</numberOfYears>\n<simPopSize>',
                as.character(popSize),'</simPopSize>\n<minAge>',
                as.character(minAge),'</minAge><maxAge>',
                as.character(maxAge),'</maxAge><timeStep>1</timeStep>\n<refScenarioName>',
                as.character(refScenarioName),'</refScenarioName>\n<randomSeed>',
                as.character(randomSeed),'</randomSeed>\n<resultType>Emptyness2BFilled</resultType>\n<popFileName>',
                as.character(populationName),'</popFileName>\n<scenarios>')
 
   ### loop through scenarios

  for (scen in scenarioList)     string <- paste0(string,'<scenario><uniquename>',scen$name, '</uniquename>',
      '<successRate>',scen$successRate,'</successRate>','\n',
      '<targetMinAge>',scen$minAge,'</targetMinAge>' ,'\n',
      '<targetMaxAge>',scen$maxAge,'</targetMaxAge>','\n',
      '<targetSex>',scen$targetSex,'</targetSex>','\n',
      '<transfilename>',scen$transitionFile,'</transfilename>','\n',
        '<prevfilename>',scen$prevalenceFile,'</prevfilename>','\n',
      '</scenario>')
  string <- paste0(string,'</scenarios><diseases>')
  for (dis in diseaseList)
    string <- paste0(string,'<disease>\n<uniquename>',dis$name,'</uniquename>','\n',
            '<prevfilename>',dis$prevalenceFile,'</prevfilename>','\n',
      '<incfilename>',dis$incidenceFile,'</incfilename>','\n',
              '<excessmortfilename>',dis$excessmortFile,'</excessmortfilename>','\n',
      '<dalyweightsfilename>',dis$disabilityFile,'</dalyweightsfilename>\n</disease>\n')
  
  rf <- riskfactorList # shorter name, if ever more risk factors are possible, can be put in loop
 # print(riskfactorList)
  string <- paste0(string,  '</diseases>\n<riskfactors>\n<riskfactor>\n<uniquename>',rf$name,'</uniquename>','\n',
  '<transfilename>',rf$transitionFile,'</transfilename>\n',
  '<prevfilename>',rf$prevalenceFile,'</prevfilename>\n</riskfactor>\n</riskfactors>\n<RRs>')
  index <- 1
  for(rr in relriskList)   {
    string <- paste0(string,'<RR><RRindex>',as.character(index),'</RRindex>',
           '<isRRfrom>',rr$from,'</isRRfrom>\n',
           '<isRRto>',rr$to,'</isRRto>\n',
           '<isRRFile>',rr$filename,'</isRRFile>\n</RR>')
    index <- index+1
  }
    
  string<-paste0(string, "</RRs></simulation>")
  


  return(string)
}
