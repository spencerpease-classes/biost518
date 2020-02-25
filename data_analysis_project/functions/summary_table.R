summary_table <- function(data, summary_funcs = NULL) {

  # Validate parameters -----------------------------------------------------

  if (ncol(data) == 0) stop("dataframe must have at least one column.")


  # Set default parameters --------------------------------------------------

  if (is.null(summary_funcs)) {
    summary_funcs <- list(
      n       = ~length(.),
      #valid   = ~sum(!is.na(.)),
      missing = ~sum(is.na(.)),
      mean    = ~mean(., na.rm = TRUE),
      sd      = ~sd(., na.rm = TRUE),
      #min     = ~min(., na.rm = TRUE),
      #q25     = ~quantile(., 0.25, na.rm = TRUE),
      median  = ~median(., na.rm = TRUE),
      #q75     = ~quantile(., 0.75, na.rm = TRUE),
      IQR     = ~IQR(., na.rm = TRUE)
      #max     = ~max(., na.rm = TRUE)
    )
  }


  # Compute summary table ---------------------------------------------------

  summary_tbl <- dplyr::summarise_all(data, summary_funcs)

  # Additional reshaping when summarising multiple columns
  if (ncol(data) - length(dplyr::group_vars(data)) > 1) {
    summary_tbl <- summary_tbl %>%
      tidyr::pivot_longer(
        cols = -tidyselect::one_of(dplyr::group_vars(data)),
        names_to = c("variable", "stat"),
        names_pattern = "(.*)_(.*)",
        values_to = "value"
      ) %>%
      tidyr::pivot_wider(
        names_from = "stat",
        values_from = "value"
      )

  } else {
    summary_tbl <- summary_tbl %>%
      dplyr::mutate(
        variable = setdiff(colnames(data), dplyr::group_vars(data))
      ) %>%
      dplyr::select(
        tidyselect::one_of(dplyr::group_vars(data)),
        .data$variable,
        tidyselect::everything()
      )
  }

  summary_tbl

}
