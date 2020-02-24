factor_inf_table <- function(model) {

  # Validate inputs ---------------------------------------------------------

  if (!is(model, "uRegress")) {
    stop("This function currently works only on `uRegress` objects.")
  }


  # Build inference table ---------------------------------------------------

  model[["augCoefficients"]] %>%
    as.table() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("param", "var", "val")) %>%
    tidyr::pivot_wider(names_from = "var", values_from = "val") %>%
    select(.data$param, .data$df, dplyr::everything(), -.data$`F stat`, -.data$`Naive SE`)

}
