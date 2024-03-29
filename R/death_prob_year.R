#' @title Compute Death Probabilities for a Specified Year at a Specified Age
#'
#' @description Calculates the probability of death for a user-specified year and a user-specified age.
#'
#' For example, the question 'what was the probability of death at age 1 in the year 1995?' can be answered with this function.
#' The computation of this not straightforward, since one individual may be aged 1 for a single day in 1995, while another may be aged 1 for the entire year of 1995.
#' The formula used here is based on that found in Chapter 2 of Demography: Measuring and Modeling Population Processes by Preston et.al (tinyurl.com/demograsource).
#'
#' Requires a data frame that contains the variables date of birth in century month code, survival status and observation time, as generated by the mortDHS_obs_time function in this package.
#' @param data The relevant data frame
#' @param age The age for which the statistic is desired
#' @param year The year for which the statistic is desired
#' @param surv_stat String name for the column containing survival status. Requires that deceased is coded as 0, and alive is coded as 1. Defaults to 'mm2', the original DHS variable name.
#' @param DOB_CMC String name for column containing dates of birth in century month code. Defaults to 'mm4', the original DHS variable name.
#' @param obs_time String name for the column containing observation times, as can be generated by the mortDHS_obs_time function. Defaults to 'obs_time'
#'
#' @seealso \code{\link[mortDHS]{death_prob_year_range}}
#'
#' @return A single numerical value
#' @export
#' @examples
#' # compute probability of death at age 0 in the year 2000
#' death_prob_year(malawisib, 0, 2000)
#'
#' # same example as above, with each param more explicitly defined
#' death_prob_year(data = malawisib, age = 0, year = 2000,
#' DOB_CMC = 'mm4', surv_stat = 'mm2', obs_time = 'obs_time')


death_prob_year <- function(data, age, year, surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){
  # using the formula: sDxY/BxY  +  ((BxY-sDxY)/BxY)   *   (pDxY/(BxYminus1 - sDxYminus1))
  # see tinyurl.com/demograsource for explanation of formula

  # convert specified age to CMC range
  lower_lim_spec_age_in_months <- age * 12
  upper_lim_spec_age_in_months <- (age * 12) + 11

  leading_cohort <- year - age         # the cohort that turned x in specified year, where x is the specified age
  # convert leading cohort year of birth to CMC range
  leading_cohort_lower_limit_CMC <- ((leading_cohort - 1900) * 12 ) + 1
  leading_cohort_upper_limit_CMC <-  ((leading_cohort - 1900) * 12) + 12

  lagging_cohort <- leading_cohort - 1 # the cohort that turned x in the year prior to the specified year
  # convert lagging cohort year of birth to CMC range
  lagging_cohort_lower_limit_CMC <- ((lagging_cohort - 1900) * 12 ) + 1
  lagging_cohort_upper_limit_CMC <-  ((lagging_cohort - 1900) * 12) + 12

  # define function that converts years to CMC
  yr_to_CMC_start <- function(yr){
    CMC <- ((yr  - 1900) * 12) + 1
    return(CMC)
  }

  sDxY <-    sum(data[[DOB_CMC]] >= leading_cohort_lower_limit_CMC
                 & data[[DOB_CMC]] <= leading_cohort_upper_limit_CMC
                 # counts individual in the leading cohort,
                 &  (data[[surv_stat]] == 0
                     & (data[[DOB_CMC]] + data[[obs_time]] >=  yr_to_CMC_start(year)
                        & data[[DOB_CMC]] + data[[obs_time]] <=  yr_to_CMC_start(year) + 11 ))
                 # who died in the specified year,
                 &  (data[[surv_stat]] == 0
                     & (data[[obs_time]] >= lower_lim_spec_age_in_months &
                          data[[obs_time]] <= upper_lim_spec_age_in_months)), na.rm = TRUE)
                 # at the specified age


  BxY <- sum(data[[DOB_CMC]] >= leading_cohort_lower_limit_CMC
             & data[[DOB_CMC]] <= leading_cohort_upper_limit_CMC
             # counts individuals in the leading cohort,
             & (( data[[surv_stat]] == 1 & data[[obs_time]] >= lower_lim_spec_age_in_months )
                # who are EITHER alive and at or above the specified age,
                |(data[[surv_stat]] == 0 & data[[obs_time]] >= lower_lim_spec_age_in_months)), na.rm = TRUE)
               # OR deceased but died at or above the specified age


  pDxY <- sum(data[[DOB_CMC]] >= lagging_cohort_lower_limit_CMC
              & data[[DOB_CMC]] <= lagging_cohort_upper_limit_CMC
              # counts individuals in the lagging cohort,
              &  (data[[surv_stat]] == 0 & (
                data[[DOB_CMC]] + data[[obs_time]] >=  yr_to_CMC_start(year)
                & data[[DOB_CMC]] + data[[obs_time]] <=  yr_to_CMC_start(year) + 11 ))
              # who died in the specified year,
              &  (data[[surv_stat]] == 0
                  & (data[[obs_time]] >= lower_lim_spec_age_in_months &
                       data[[obs_time]] <= upper_lim_spec_age_in_months)), na.rm = TRUE)
  # at the specified age


  BxYminus1 <- sum(data[[DOB_CMC]] >= lagging_cohort_lower_limit_CMC
                   & data[[DOB_CMC]] <= lagging_cohort_upper_limit_CMC
                   # counts individuals in the lagging cohort,
                   &(( data[[surv_stat]] == 1 & data[[obs_time]] >= lower_lim_spec_age_in_months )
                     # who are EITHER alive and at or above the specified age
                     |(data[[surv_stat]] == 0 & data[[obs_time]] >= lower_lim_spec_age_in_months)), na.rm = TRUE)
  # OR deceased but died at or above the specified age


  sDxYminus1 <- sum(data[[DOB_CMC]] >= lagging_cohort_lower_limit_CMC
                    & data[[DOB_CMC]] <= lagging_cohort_upper_limit_CMC
                    # counts individuals in the lagging cohort,
                    & (( data[[surv_stat]] == 1 & data[[obs_time]] >= lower_lim_spec_age_in_months )
                       # who are EITHER alive and at or above the specified age
                       |(data[[surv_stat]] == 0 & data[[obs_time]] >= lower_lim_spec_age_in_months))
                    # OR deceased but died at or above above the specified age
                    &  (data[[surv_stat]] == 0 & (
                      data[[DOB_CMC]] + data[[obs_time]] >=  yr_to_CMC_start(year - 1)
                      & data[[DOB_CMC]] + data[[obs_time]] <=  yr_to_CMC_start(year - 1) + 11 )), na.rm = TRUE)
  # AND who died in the year prior to the specified one

  finalrate <- (sDxY/BxY)  + (   ((BxY-sDxY)/BxY)   *   (pDxY/(BxYminus1 - sDxYminus1))   )
  return(finalrate)
}

