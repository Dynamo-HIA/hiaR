if (F){
  require(xml2)
##setwd("C:/Users/boshuizh/OneDrive - Wageningen University & Research/Documents/dynamo-hia-r/inst/extdata")
file.list <-list.files(pattern = "\\.xsd$")
## de eerste 8 zijn baseline values niet in input
## evenals 22:27 36 38 47 55
cleaned.list <- file.list[c(9:21,28:35,37,39:46,48:54)]
schema.names <- gsub("\\.xsd","",cleaned.list)
for (i in 1:length(cleaned.list)){
  schema <- read_xml(cleaned.list[i]) 
  
  nam <- schema.names[i]
  assign(nam, schema)
  
}
usethis::use_data(
  dalyweights         ,                diseaseincidences,                  
  diseaseprevalences   ,               excessmortality   ,                 
  newborns              ,              overalldalyweights ,                
  overalldisability      ,             overallmortality    ,               
  populationsize          ,            relativeRisks        ,              
  relativerisks_begin      ,           relativerisks_diseaseondisease,     
  relativerisks_end         ,          relrisksfordeath_categorical,       
 relrisksfordeath_compound   ,        relrisksfordeath_continuous   ,     
  relrisksfordisability_categorical,   relrisksfordisability_compound,     
  relrisksfordisability_continuous,    relrisksfromdisease            ,    
  relrisksfromriskfactor_categorical,  relrisksfromriskfactor_compound,    
  relrisksfromriskfactor_continuous,   relrisksfromriskfactor_continuous4p,
  riskfactor_categorical,              riskfactor_compound    ,            
  riskfactor_continuous  ,             riskfactorprevalences_categorical,  
  riskfactorprevalences_continuous,    riskfactorprevalences_duration ,    
  simulation                      ,    transitiondrift                 ,   
  transitiondrift_netto           ,    transitiondrift_zero             ,  
  transitionmatrix                ,    transitionmatrix_netto           ,  
  transitionmatrix_zero ) 
### te halen als data/...
}