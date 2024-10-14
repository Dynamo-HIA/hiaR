

create_transition_matrix_xml <- function(transition_df) {
  root <- xml2::xml_new_root("transitionmatrix")

  add_df_to_xml(root, transition_df, "transition")

  return(root)
}


create_relative_risks_death_xml <- function(relative_risks_death_df,
                                            type = c("continuous",
                                                     "categorical")
                                            ) {
  root <- xml2::xml_new_root(paste0("relrisksfordeath_", type))

  add_df_to_xml(root, relative_risks_death_df, "relriskfordeath")

  return(root)
}


create_risk_factor_prevalences_xml <- function(prevalences_df,
                                            type = c("continuous",
                                                     "categorical")
                                            ) {
  root <- xml2::xml_new_root(paste0("riskfactorprevalences_", type))

  add_df_to_xml(root, prevalences_df, "prevalence")

  return(root)
}


create_risk_factor_prevalences_duration_xml <- function(duration_df) {
  root <- xml2::xml_new_root("riskfactorprevalences_duration")

  add_df_to_xml(root, duration_df, "prevalence")

  return(root)
}

# TODO: Relative_Risks_For_Disability
# TODO: Odds_Ratios_For_Disability
# TODO: configuration
