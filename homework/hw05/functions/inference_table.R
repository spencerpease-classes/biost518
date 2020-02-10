inference_table <- function(lm,
                            paramter_names = NULL,
                            se_type = "Robust",
                            include_intercept = FALSE) {

  # Validate inputs ---------------------------------------------------------

  if (!is(lm, "uRegress")) {
    stop("This function currently works only on `uRegress` objects.")
  }

  se_type <- paste(se_type, "SE")
  valid_se_types <- c("Naive SE", "Robust SE")

  if (!all(se_type %in% valid_se_types)) {
    stop("`se_type` must include only 'Robust' and/or 'Naive'.")
  }


  # Determine parameter names -----------------------------------------------

  default_names <- rownames(lm[["coefficients"]])

  if (!include_intercept) {
    default_names <- default_names[-1]
  }

  if (is.null(paramter_names)) {
    paramter_names <- default_names

  } else if (length(default_names) != length(paramter_names)) {
    stop(paste(
      "Given parameter names must be the same length as the number of",
      "parameters in the model. This may or may not include the intercept,",
      "depending on the value os `include_intercept`."
    ))
  }


  # Build inference table ---------------------------------------------------

  removed_se_types <- setdiff(valid_se_types, se_type)

  row_start <- as.integer(!include_intercept) + 1

  inference_tbl <- lm[["coefficients"]] %>%
    as_tibble() %>%
    slice(row_start:length(lm[["coefficients"]])) %>%
    mutate(Parameter = paramter_names) %>%
    select(.data$Parameter, everything(), -all_of(removed_se_types))

  inference_tbl

}
