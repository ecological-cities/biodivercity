#'Validate the accuracy of provided models using new data from animal surveys
#'
#'For a specific animal (taxon) group, predict the number of species (richness) at each
#'point location (rows in `newdata`) based on the landscape predictors (columns in `newdata`)
#'summarised at these locations. Comparison with the actual data from animal surveys are used to
#'calculate the accuracy (error rate) of the models provided.
#'
#'@param models Model objects for a specific animal (taxon) group to be used for predictions
#'(output of `MuMIn::get.models()`).
#'@param recipe_data recipe object (`recipes::recipe()`) containing information on how to
#'transform the new data to the appropriate format, prior to making predictions (e.g. scale and center variables).
#'@param newdata New data collected at point locations for the specific taxon group.
#'Used to validate the accuracy of the models. Should contain the columns for landscape predictors found in the models.
#'@param response_var Column name of the response variable specified in `newdata`.
#'Defaults to `sprich` (species richness).
#'
#'@return A dataframe containing the predictions (column `mod.avg.pred`) and other
#'accuracy metrics when compared to the actual data (column name as defined by the `response_var` argument).
#'
#'@import checkmate
#'@importFrom dplyr select mutate
#'@importFrom tidyselect all_of starts_with
#'@importFrom recipes bake
#'@importFrom lme4 ranef
#'@importFrom AICcmodavg modavgPred
#'@importFrom rlang .data
#'@importFrom stats sd
#'
#'@export
validate_newdata <- function(models,
                             recipe_data,
                             newdata,
                             response_var = "sprich"){

  # scale predictors based on original input data, unecessary columns will be removed
  newdata <- newdata %>%
    rename(sprich = response_var) # rename to response var in model object
  newdata <- recipe_data %>% recipes::bake(newdata)


  # extract predictors of interest from models
  randef <- lapply(models, lme4::ranef)
  randef_names <- lapply(randef, names) %>%
    unlist() %>% unname() %>% unique()
  rm(randef)

  fixedef_names <- lapply(models, function(x) names(x@frame)) %>%
    unlist() %>% unique()
  fixedef_names <- fixedef_names[!grepl(paste0("^sprich$"), fixedef_names)] # remove response var
  fixedef_names <- fixedef_names[!fixedef_names %in% randef_names] # remove rand effect


  # select cols in newdata that match response & predictors (all must be present)
  data_validate <- newdata %>%
    dplyr::select(#tidyselect::all_of(response_var),
                  tidyselect::all_of(fixedef_names),
                  tidyselect::any_of(randef_names))
  # if random effect is absent after bake(), modavgPred uses the "average" randeffect for predictions


  # make predictions
  avgpred <- AICcmodavg::modavgPred(models,
                                    newdata = data_validate,
                                    type = "response")


  # add error metrics as new cols - absolute & normalised values
  data_validate <- avgpred %>% # overwrite var
    as.data.frame() %>%
    dplyr::select(-tidyselect::starts_with("matrix.output.")) %>%
    dplyr::mutate("{response_var}" := newdata$sprich) %>% # convert back to users' response var name
    dplyr::mutate(error = .data$mod.avg.pred - .data[[response_var]]) %>%
    dplyr::mutate(error_normalised = .data$error / stats::sd(.data[[response_var]]))

  return(data_validate)
}
