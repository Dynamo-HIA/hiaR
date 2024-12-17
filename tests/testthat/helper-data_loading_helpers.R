generate_test_file_groups <- function(file_group) {
  if (file_group == "disease") {
    lookup_items <- c("Relative_Risks_From_Risk_Factor", "Relative_Risks_From_Diseases")

    input_list <- list(
      Disease1 = list(
        Relative_Risks_From_Risk_Factor = list(
          "RR_to_Disease1-Risk1.xml" = NULL,
          "RR_to_Disease1-Risk2.xml" = NULL
        ),
        Relative_Risks_From_Diseases = list(
          "RR_to_Disease1-Disease2.xml" = NULL
        ),
        unrelated_item = list(
          "another-item.xml" = NULL
        )
      ),
      Disease2 = list(
        Relative_Risks_From_Risk_Factor = list(
          "RR_to_Disease2-Risk3.xml" = NULL
        ),
        empty_item = NULL
      )
    )
  } else if (file_group == "risk_factor") {
    lookup_items <- c("Relative_Risks_For_Death", "Relative_Risks_For_Disability")
    input_list <- list(
      Factor1 = list(
        Relative_Risks_For_Death = list(
          "some_file.xml" = NULL,
          "another_file.xml" = NULL
        ),
        Relative_Risks_For_Disability = list(),
        unrelated_item = list(
          "another-item.xml" = NULL
        )
      ),
      Factor2 = list(
        Relative_Risks_For_Death = list(
          "file2.xml" = NULL,
          "file4.xml" = NULL
        ),
        Relative_Risks_For_Disability = list(
          "file10.xml" = NULL
        )
      )
    )
  }

  return(list(
    inputs = input_list,
    lookup_items = lookup_items
  ))
}

generate_relative_risk_data <- function() {
  df_a1 <- data.frame(
    filename = c("file1", "file2"),
    from = c("source1", "source2"),
    to = c("disease1", "disease1")
  )
  df_b1 <- data.frame(
    filename = c("file3", "file4"),
    from = c("source3", "source4"),
    to = c("disease1", "disease1")
  )
  df_a2 <- data.frame(
    filename = c("file11", "file12"),
    from = c("source11", "source12"),
    to = c("disease2", "disease2")
  )
  df_b2 <- data.frame(
    filename = c("file13", "file14"),
    from = c("source13", "source14"),
    to = c("disease2", "disease2")
  )
  cols_input <- c("filename", "from")

  input_list <- list(
    disease1 = list(
      key_a = df_a1[cols_input],
      key_b = df_b1[cols_input],
      key_nonmatching = NULL
    ),
    disease2 = list(
      key_a = df_a2[cols_input],
      key_b = df_b2[cols_input]
    )
  )

  return(list(
    input_list = input_list,
    kw_map = list(
      key_a = NULL,
      key_b = NULL
    ),
    expected = list(
      key_a = rbind(df_a1, df_a2),
      key_b = rbind(df_b1, df_b2)
    )
  ))
}
