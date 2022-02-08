#' Create a language model for the three warning sections
#'
#' @description Classify every key string as severe, moderate, or mild
#'              and visualize that result
#'
#' @param box_warning contains all boxed warnings of drugs
#' @param contraindications contains all contraindications of drugs
#' @param warnings contains all warnings of drugs
#' @param language contains key strings
#' @param word a word string
#'
#' @return a row vector
#'
#' @export
#'
#' @keywords language model classification warnings
#'
#' @examples
#' data(box_warning_data)
#' data(contraindications_data)
#' data(warning_data)
#' data(keystrings)
#' languageModel(box_warning_data, contraindications_data, warning_data, keystrings, "death")

languageModel <- function(box_warning, contraindications, warnings, language, word) {
	##
	## Begin Yiwen Guo code
    ##
    
	classification_table = langModelTable(box_warning, contraindications, warnings, language)

	# sum the scores in each row
	score_sum = rowSums(classification_table[,2:4])

	# normalize the score for each row
	normalization = classification_table[,2:4] / score_sum
	colnames(normalization) = c("severe", "moderate", "mild")
	rownames(normalization) = classification_table$language

	# find the index associated with the input word
	index = match(word, rownames(normalization))
	result = normalization[index,]
	return (result)
	##
	## End Yiwen Guo code
    ##
}