generate_filtered_relative_risk_data <- function() {
  from_diseases <- data.frame(
    from = c("disease1", "disease1", "disease2"),
    filename = c(
      "RR_to_disease4-disease1.xml",
      "RR_to_disease3-disease1.xml",
      "RR_to_disease1-disease2.xml"
    ),
    to = c("disease4", "disease3", "disease1")
  )

  from_risk_factors <- data.frame(
    from = c("risk_factor1", "risk_factor2"),
    filename = c("RR_to_disease1-risk_factor1.xml", "RR_to_disease2-risk_factor2.xml"),
    to = c("disease1", "disease2")
  )

  to_death <- data.frame(
    from = c("risk_factor1", "risk_factor2"),
    filename = c("RR_to_death-risk_factor1.xml", "RR_to_death-risk_factor_2.xml"),
    to = c("death", "death")
  )

  to_disability <- data.frame(
    from = c("risk_factor1", "risk_factor2"),
    filename = c("RR_to_disability-risk_factor1.xml", "RR_to_disability-risk_factor_2.xml"),
    to = c("disability", "disability")
  )

  return(
    list(
      from_diseases = from_diseases,
      from_risk_factors = from_risk_factors,
      to_death = to_death,
      to_disability = to_disability
    )
  )
}


generate_raw_relative_risk_data <- function() {
  from_diseases <- data.frame(
    from = c("disease1", "disease1", "disease2", "disease2"),
    filename = c("file1.xml", "file2.xml", "file3.xml", "file4.xml"),
    to = c("disease2", "disease3", "disease1", "disease3")
  )
  from_risk_factors <- data.frame(
    from = c("risk_factor1", "risk_factor1", "risk_factor2", "risk_factor2", "risk_factor3", "risk_factor3"),
    filename = c("file10.xml", "file11.xml", "file12.xml", "file13.xml", "file14.xml", "file15.xml"),
    to = c("disease1", "disease2", "disease1", "disease2", "disease3", "disease3")
  )
  to_death <- data.frame(
    from = c("risk_factor1", "risk_factor2", "risk_factor3"),
    filename = c("file20.xml", "file21.xml", "file22.xml"),
    to = rep("death", 3)
  )
  to_disability <- data.frame(
    from = c("risk_factor1", "risk_factor2", "risk_factor3"),
    filename = c("file30.xml", "file31.xml", "file32.xml"),
    to = rep("death", 3)
  )
  return(list(
    diseases = list(
      Relative_Risks_From_Risk_Factor = from_risk_factors,
      Relative_Risks_From_Diseases = from_diseases
    ),
    risk_factors = list(
      Relative_Risks_For_Death = to_death,
      Relative_Risks_For_Disability = to_disability
    )
  ))
}

generate_dummy_prevalence_and_transition_data <- function() {
  # Define a simple naming function for generating dummy file names
  create_dummy_files <- function(prefix, category, count) {
    paste0(prefix, "_", category, "_File", seq_len(count), ".xml")
  }

  # Create a list structure matching your example
  dummy_data <- list(
    transitions = list(
      Alcohol_cat5 = create_dummy_files("T", "Alcohol_cat5", 4),
      BMI_cat3 = create_dummy_files("T", "BMI_cat3", 3),
      BMI_cont = create_dummy_files("T", "BMI_cont", 2),
      Smoking_cat3 = create_dummy_files("T", "Smoking_cat3", 5),
      Smoking_dur = create_dummy_files("T", "Smoking_dur", 2)
    ),
    prevalences = list(
      Alcohol_cat5 = create_dummy_files("P", "Alcohol_cat5", 3),
      BMI_cat3 = create_dummy_files("P", "BMI_cat3", 3),
      BMI_cont = create_dummy_files("P", "BMI_cont", 1),
      Smoking_cat3 = create_dummy_files("P", "Smoking_cat3", 3),
      Smoking_dur = create_dummy_files("P", "Smoking_dur", 1)
    )
  )

  return(dummy_data)
}

generate_dummy_selected_risk_factors <- function() {
  # Define a simple naming function for generating dummy file names
  create_dummy_files <- function(prefix, category, type, count) {
    paste0(prefix, "_", category, "_", type, "_File", seq_len(count), ".xml")
  }

  # Create the dummy data structure
  dummy_data <- list(
    Alcohol_cat5 = list(
      prevalence = create_dummy_files("P", "Alcohol_cat5", "Prev", 1),
      transitions = create_dummy_files("T", "Alcohol_cat5", "Transition", 1)
    ),
    BMI_cat3 = list(
      prevalence = create_dummy_files("P", "BMI_cat3", "Prev", 1),
      transitions = create_dummy_files("T", "BMI_cat3", "Transition", 1)
    )
  )

  return(dummy_data)
}






