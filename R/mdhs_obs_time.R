
#' @title Compute Observation Time for Survival Analysis using DHS Century Month Codes
#'
#' @description Generates a 'Time Under Observation' variable from DHS data, accounting for right censoring.
#'      For individuals who are alive at the time of the survey, observation time is their age, in months, at the start of the survey.
#'      For individuals who are deceased at the time of the survey, observation time is their age, in months, at the time of death.
#'
#' @param data The relevant data frame
#' @param surv_stat String name for the column containing survival status. Requires that deceased is coded as 0, and alive is coded as 1. Defaults to 'mm2' as in the DHS Individual Recode File.
#' @param DOB_CMC String name for the column containing dates of birth in century month code. Defaults to 'mm4', which is the corresponding variable name in the DHS individual recode files.
#' @param DOD_CMC String name for the column containing dates of death in century month code. Defaults to 'mm8', which is the corresponding variable name in the DHS individual recode files.
#' @param interview_date_CMC String name for the column containing dates of interview in century month code. Defaults to 'v008', which is the corresponding variable name in the DHS individual recode files.
#'
#' @return A vector of observation times
#' @export
#' @examples
#' # first generate the sibling recode dataframe
#' sib_recode <- mortDHS_reshape(data = rwandadhs, sib_cols = c(1,2,4,8))
#' # then merge it with the individual recode file
#' merged_file <- mortDHS_merge(sib_df = sib_recode, respondent_df = rwandadhs)
#' # then create an observation time column in the merged data frame
#' merged_file$obs_time <-
#'    mortDHS_obs_time(merged_file, surv_stat = 'mm2' , DOB_CMC = 'mm4', DOD_CMC = 'mm8',
#'    interview_date_CMC = 'v008' )

mortDHS_obs_time <- function(data, surv_stat ='mm2', DOB_CMC ='mm4', DOD_CMC='mm8',interview_date_CMC='v008'){

  data$age_at_death_in_months <- data[[DOD_CMC]] - data[[DOB_CMC]]

  data$age_at_survey_in_months <- ifelse( test = data[[surv_stat]] == 1,
                                          yes = data[[interview_date_CMC]] - data[[DOB_CMC]],
                                          no = NA )

  data$obs_time <- ifelse(test = data[[surv_stat]] == 0, # if the individuals are dead
                          yes = data$age_at_death_in_months,  # then their obs time is until their age of death
                          no = data$age_at_survey_in_months ) # otherwise, obs time is until their age at the survey

  return(as.numeric(data$obs_time))

}



