
#' @title Reshape a DHS individual recode file into a one-sibling-per-row file
#'
#' @description This function takes a DHS individual recode file, with one respondent per row, and transforms it into a sibling recode file, with one sibling per row.
#'      The user can choose to indicate which of the 15 sibling variables should be included in the reshaped file.
#'
#' @param data The DHS individual recode file.
#' @param id The string name of the ID variable in the dataset, entered as a string. Defaults to 'caseid'.
#' @param sib_cols A vector of numbers, e.g.c(1:8), to indicate which sibling variables to reshape. Defaults to c(1,2,4,8), because "mm1", "mm2", "mm4" and "mm8" are the DHS variables needed for the mortality analysis functions in this package.
#'
#' @return Returns a dataframe with one sibling per row
#' @export
#' @importFrom stringr str_pad str_split_fixed
#' @importFrom reshape2 melt
#' @importFrom dplyr rename
#' @examples
#' mortDHS_reshape(data = malawisib, sib_cols = c(1,2,4,8))
#'

mortDHS_reshape <- function(data, sib_cols = c(1,2,4,8), id= 'caseid'){

  get_sib_vars <-  function(data, sib_cols, id = 'caseid'){

    # generate full list of variables to be selected from the dataframe
    names_of_columns <- paste("mm", c(sib_cols), sep = "")
    subnumbers_of_columns <- stringr::str_pad(1:20, 2, pad="0")
    sibling_columns<- paste(rep(names_of_columns, each = length(subnumbers_of_columns)), subnumbers_of_columns, sep = "_")

    # select caseid and sibling variables
    data <- data[,c(id, sibling_columns)]
    return(data)
  }


  #run the get_sib_vars function on the dataframe. Here, we begin the reshape
  data <- get_sib_vars(data, c(sib_cols))

  # First melt the dataframe
  melted  <- reshape2::melt(data=data , id.vars = id )

  # then break up the sibling variable columns into the variable name and the number of the sibling
  # place those two new columns in an object
  splitoutput <- stringr::str_split_fixed(melted$variable, "_", 2)

  # then add in the object as a new column This is the ID number of the sibling
  melted <- cbind(melted, splitoutput)

  # rename
  colnames(melted)[colnames(melted) == '1'] <- 'actual_var'
  colnames(melted)[colnames(melted) == '2'] <- 'sib_id'

  # drop redundant column
  melted$variable <- NULL

  # split the dataframe into separate dataframes, one for each sibling variable
  store_sep_dfs <- split(melted, with(melted, actual_var))

  # create function to rename actual_var, giving it the column name for each sibling ID
  # I do this by selecting the first value in the column for actual var [1]
  rename_col <- function(df){
    colnames(df)[which(colnames(df) == "value" )]<- as.character(df$actual_var[1])
    return(df)}
  # apply this over the list of data frames
  store_sep_dfs <- Map(rename_col, store_sep_dfs)

  # drop a now-redundant column
  drop_actual_var <- function(df){ df <- df[,-(which(colnames(df) == "actual_var"))]}
  store_sep_dfs <- Map(drop_actual_var, store_sep_dfs)

  # Merge each data frame in list
  f1<- function(x,y)base::merge(x,y, by = c(id,"sib_id"))
  recombined_df <- Reduce(f1, store_sep_dfs)

  #order the data frame
  recombined_df<-recombined_df[order(recombined_df$caseid),]

  # drop all rows for which a DOB of the sibling does not exist, as
  # non-existent siblings are an artifact of reshaping
  not_missing_DOB <- !is.na(recombined_df$mm4)
  recombined_df <- recombined_df[which(not_missing_DOB),]

  return(recombined_df)

}


