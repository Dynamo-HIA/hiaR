
# functions to query reference data for the shiny app

# omit file ending from names 
# we need a mapping from each filename as it is displayed to the user to the full path (relative to root or so) 
  # would this not be useful in a dataframe? or in a flattened list -- unless it can be queried very fast from the nesting
  # but I suppose when it's nested, you have to search through all nests until you find it
  # -- this assumes we'll need the full paths for the model? can I know this already?
# of the file, so that it can be passed to the model
# for relative risks: we need "from", "to", and the file name
  # this should perhaps go  into the data_loading file?

# TODO: check that we capture all relative risks??

#ref_data <- get_reference_data("../data/Tutorial_Data/Reference_Data")
# relrisk <- ref_data$relative_risks
# a < - relrisk$diseases

#nested_list_to_paths <- function(nested_list, base_path = "") {
#  result <- character()
#  
#  process_list <- function(current_list, current_path) {
#    for (name in names(current_list)) {
#      new_path <- file.path(current_path, name)
#      
#      if (is.list(current_list[[name]])) {
#        process_list(current_list[[name]], new_path)
#      } else {
#        result <<- c(result, new_path)
#      }
#    }
#  }
#  
#  process_list(nested_list, base_path)
#  
#  result
#}
#
#check_item <- function(x, base = "") {
#  if (is.list(x)) {
#    if (length(x) == 0) {
#      return(NULL)
#    } 
#    else {
#      list_names <- names(x)
#      base <- file.path(base, x)
#      lapply(x, function(z) {check_item(z, base)})
#    }
#  } else if (is.character(x)) {
#    return(file.path(base, x))
#  } else {
#    stop("unexpected")
#  }
#}
#
#
##ref_data <- get_reference_data("../data/Tutorial_Data/Reference_Data")
## relrisk <- ref_data$relative_risks
## a < - relrisk$diseases
#
#b <- stack(a$Breast_Cancer$Relative_Risks_From_Risk_Factor)



#data <- get_reference_data("../data/Tutorial_Data/Reference_Data")
#test <- lapply(data$relative_risks$diseases, function(x) x[["Relative_Risks_From_Risk_Factor"]])
#bind_rows(test, .id = "to")
#
#
#test <- lapply(data$relative_risks$diseases, function(x) x[["Relative_Risks_From_Diseases"]])
#bind_rows(test, .id = "to")
#
#a <- lapply(data$relative_risks$risk_factors, function(x) x[["Relative_Risks_For_Death"]]) |> bind_rows() # need to define "to" column manually
#a$to <- "death"
#
#
#lapply(data$relative_risks$risk_factors, function(x) x[["Relative_Risks_For_Disability"]]) |> bind_rows()
#
#
#keywords_disease <- c("Relative_Risks_From_Risk_Factor", "Relative_Risks_From_Diseases")
#keywords_rf <- c("Relative_Risks_For_Death", "Relative_Risks_For_Disability") 
#
#rr_diseases = get_relative_risks(data$diseases, keywords_disease)
#rr_rf <- get_relative_risks(data$risk_factors, keywords_rf, FALSE)
#
#
#
#
#
#
#mylist <- list(Relative_Risks_From_Risk_Factor = NULL, Relative_Risks_From_Diseases = NULL)
#bind_relative_risks(in_list = data$relative_risks$diseases, mapping = mylist)
#
#mylist <- list(Relative_Risks_For_Death = "death", Relative_Risks_For_Disability = "disability")
#bind_relative_risks(in_list = data$relative_risks$risk_factors, mapping = mylist)
#


