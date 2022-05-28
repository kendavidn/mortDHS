
#' @title Merge a Sibling Recode File with Select Variables from the DHS Individual Recode
#'
#' @description This is simply a wrapper for the 'merge' function in base R with handy defaults. It allows the user to easily select which of the variables from the DHS individual recode should be included.
#'
#' @param sib_df The DHS sibling recode file, as generated with the mortDHS_reshape function
#' @param respondent_df The corresponding DHS individual respondent recode dataframe
#' @param respondent_vars String names of the columns in the individual recode file to be merged. Defaults to 'v008', as this is the only additional variable needed to calculate sibling mortality statistics.
#' @param id The name of the ID variable in the dataset, entered as a string. Defaults to 'caseid'.

#' @return Returns the merged dataframe
#' @export
#' @examples
#' # first generate the sibling recode dataframe
#' sib_recode <- mortDHS_reshape(data = malawidhs, sib_cols = c(1:5))
#' # then merge it with the individual recode file
#' merged_file <- mortDHS_merge(sib_df = sib_recode, respondent_df = malawidhs)

mortDHS_merge <- function(sib_df, respondent_df, respondent_vars = c('v008') , id= 'caseid'){

  # select columns that have been asked for in the function
  respondent_columns <-  respondent_df[, which(colnames(respondent_df) %in% respondent_vars
                                               # and the column caseid (needed for merging)
                                               | colnames(respondent_df) == id) ]

  # merge the siblings columns are respondent columns by caseid
  merged_df <- merge(sib_df, respondent_columns, by = id)

  return(merged_df)

}



