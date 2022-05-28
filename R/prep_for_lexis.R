#' @title Prepare Output from death_prob Function for Lexis Plot
#'

#' @description
#' The function reshapes and rearranges the input data frame into a ‘long’ format, ready to be turned into a ggplot geom_tile visualization.
#' @param data The relevant data frame
#' @export
#'
#'
prep_for_lexis <- function(data){

# transpose
transposed_data <- data.table::transpose(data)
colnames(transposed_data) <- rownames(data)
rownames(transposed_data) <- colnames(data)

# put rownames (ages) in first column
transposed_data_labelled <- cbind(rownames(transposed_data), transposed_data)
colnames(transposed_data_labelled)[colnames(transposed_data_labelled) == 'rownames(transposed_data)'] <- 'age'

# melt
melted<- reshape2::melt(transposed_data_labelled, id.vars ='age' )

# order the ages as factors so ggplot orders them properly on the y axis
melted$age <- factor(melted$age)
sorted_labels <- as.character(sort(as.integer(levels(melted$age))))
melted$age <- factor(melted$age, levels = sorted_labels)

# rename variables
colnames(melted)[colnames(melted) == 'variable'] <- 'year'
colnames(melted)[colnames(melted) == 'value'] <- 'death_prob'

# take log of mortality
melted$log_death_prob <- log10(melted$death_prob)

return(melted)

}

