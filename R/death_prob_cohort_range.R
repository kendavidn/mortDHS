#' @title Compute Death Probabilities for a Specified Cohort Range over a Specified Age Range
#'
#' @description This function salculates the probability of death for a user-specified cohort and age range.
#' It simply iterates the 'death_prob_cohort' function over the range of values provided by the user.
#'
#' For example, the question 'what was the probability of death during teenage (13-19) years for the cohorts born between 1995 and 1997?' can be answered with this function.
#' This would be an averaged value of:
#'
#' -The death probability at age 13 for the 1995 cohort (The number of people in the 1995 cohort cohort who died at age 13 divided by the number that reached their 1st birthday.)
#'
#' -The death probability at age 14 for the 1995 cohort
#'
#' -...
#'
#' -The death probability at age 19 for the 1995 cohort
#'
#' -The death probability at age 13 for the 1996 cohort
#'
#' -The death probability at age 14 for the 1996 cohort
#'
#' -...
#'
#' -The death probability at age 19 for the 1996 cohort
#'
#' Requires a data frame that contains columns for date of birth in century month code, survival status and observation time, as can be generated by the mortDHS_obs_time function in this package.
#'
#' @param data The data frame containing sibling histories
#' @param lower_age The lower boundary of the age interval for which the statistic is desired
#' @param upper_age The upper boundary of the age interval for which the statistic is desired
#' @param lower_cohort The lower boundary of the cohort interval for which the statistic is desired
#' @param upper_cohort The upper boundary of the cohort interval for which the statistic is desired
#' @param surv_stat String name for the column containing survival status. Requires that deceased is coded as 0, and alive is coded as 1. Defaults to 'mm2', the original DHS variable name.
#' @param DOB_CMC String name for column containing dates of birth in century month code. Defaults to 'mm4', the original DHS variable name.
#' @param obs_time String name for the column containing observation times, as generated by the mortDHS_obs_time function. Defaults to 'obs_time'
#'
#' @seealso \code{\link[mortDHS]{death_prob_cohort}}
#'
#' @return A dataframe with death probabilities. Ages are arranged along the columns, while cohorts(birth years) are arranged along the rows.
#' @export
#' @examples
#' # compute probability of death between ages 0 and 5 for cohorts born between 2000 and 2005
#' death_prob_cohort_range(malawisib, 0,5,2000,2005)
#'
#' # same example as above, with each parameter more explicitly defined
#' death_prob_cohort_range(data = malawisib, lower_age = 0, upper_age = 5,
#' lower_cohort = 2000, upper_cohort = 2005,
#' DOB_CMC = 'mm4', surv_stat = 'mm2', obs_time = 'obs_time')
#'

death_prob_cohort_range <- function(data, lower_age, upper_age, lower_cohort, upper_cohort, surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){

    # create a dataframe with the requisite number of rows and columns
    df <- data.frame(matrix(NA, nrow = (upper_cohort - lower_cohort + 1) , ncol = (upper_age - lower_age + 1)))

    # name each row and column
    rownames(df) <- c(lower_cohort:upper_cohort)
    colnames(df) <- c(lower_age:upper_age)

    # define cohort and age ranges
    cohort_range <- lower_cohort:upper_cohort
    age_range <- lower_age:upper_age

    # iterate death_prob_cohort function over each of the ages and cohorts, populating the data frame
    for (i in cohort_range){
      for (j in age_range){
        df[which(rownames(df)==i), which(colnames(df)== j)] <- death_prob_cohort(data = data, age = j, cohort = i,
                                                                                  surv_stat = surv_stat,
                                                                                  DOB_CMC = DOB_CMC,
                                                                                  obs_time = obs_time )
      }
    }
    return(df)
  }


