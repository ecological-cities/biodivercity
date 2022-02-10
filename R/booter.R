#'Helper function to generate bootstrapped statistics of accuracy metrics
#'
#'@param df_predictions Dataframe of predictions from the function `validate_newdata()`
#'(output of `MuMIn::get.models()`).
#'@param n Number of repetitions
#'
#'@importFrom dplyr slice_sample summarise
#'@importFrom rlang .data
#'
booter <- function(df_predictions, n){

  boot_results <- list()

   for(i in 1:n){

    boot_results[[i]] <- df_predictions %>%
      dplyr::slice_sample(n = nrow(df_predictions), replace = TRUE) %>%
      dplyr::summarise(error_mean = mean(.data$error, na.rm = TRUE),
                       error_normalised_mean = mean(.data$error_normalised, na.rm = TRUE),
                       rmse = sqrt(mean(.data$error^2, na.rm = TRUE)))
   }

  boot_results <- do.call(rbind, boot_results)

}
