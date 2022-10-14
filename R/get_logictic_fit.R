#' Fit model on data
#'
#' @param inputData input data
#' @param groupingVariables variables to group
#' @param upperLimit up boundary for fitting function
#' @param lowerLimit lower boundary for fitting function
#' @param saveModel should model be saved
#'
#' @return fitted logistic params/model
#'
get_logistic_fit <- function(inputData, groupingVariables,
                              upperLimit, lowerLimit = 1E-10,
                              saveModel){

  if(!is.null(upperLimit)){upperLimit <- c(NA,NA, upperLimit, NA)}

  if(!is.null(lowerLimit)){lowerLimit <- c(NA,NA, lowerLimit, NA)}

  output_drc <- drc::drm(
    formula = as.numeric(fc_ttm) ~ as.numeric(concentration),
    na.action = stats::na.omit,
    data = inputData,
    upperl = upperLimit,
    lowerl = lowerLimit,
    fct = drc::L.4())

  # test if logictic fit outperforms linear regression using ANOVA

  if(class(output_drc) == "drc" &&
     !is.null(stats::coef(output_drc)) &&
     !is.null(stats::residuals(output_drc))
  ) {

     output_df <- coef(output_drc)

     output_df["p_val"] <- summary(output_drc)[3]$coefficients[1,4]


     }else{

    output_df <- NA

     }

  if(saveModel==TRUE){
    model = output_drc
    return(list(model,output_df))

  }else{
    return(output_df)
  }
}
