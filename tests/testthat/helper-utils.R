
generate_filtered_relative_risk_data <- function() {

  from_diseases <- data.frame(
    from = c("disease1", "disease1", "disease2"),
    filename = c("RR_to_disease4-disease1.xml",
                 "RR_to_disease3-disease1.xml",
                 "RR_to_disease1-disease2.xml"),
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
  to_death = data.frame(
    from = c("risk_factor1", "risk_factor2", "risk_factor3"),
    filename = c("file20.xml", "file21.xml", "file22.xml"),
    to = rep("death", 3)
  )
  to_disability = data.frame(
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








