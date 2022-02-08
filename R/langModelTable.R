#' Create a language model for the three warning sections
#'
#' @description Classify every key string as severe, moderate, or mild
#'              and visualize that result
#'
#' @param box_warning contains all boxed warnings of drugs
#' @param contra contains all contraindications of drugs
#' @param warning contains all warnings of drugs
#' @param language contains key strings
#'
#' @return a table 
#'
#' @export
#'
#' @importFrom tidyr spread 
#' @importFrom dplyr group_by select mutate n
#' @importFrom magrittr %>%
#' @importFrom stats aggregate
#'
#' @keywords language model table
#'
#' @examples
#' data(box_warning_data)
#' data(contraindications_data)
#' data(warning_data)
#' data(keystrings)
#' langModelTable(box_warning_data, contraindications_data, warning_data, keystrings)

langModelTable <- function(box_warning, 
	                       contra,
	                       warning,
	                       language) {
	##
	## Begin Yiwen Guo code
    ##
	
	language = as.character(language)

	bw_keystring_count = processCorpus(box_warning, language)
	contra_keystring_count = processCorpus(contra,language)
	warn_keystring_count = processCorpus(warning,language)

    # bw_keystring_count = countKeyString(bw_corpus, language)
    # contra_keystring_count = countKeyString(contra_corpus, language)
    # warn_keystring_count = countKeyString(warn_corpus, language)

    # Quiet warnings and notes
    category = normalization = total = id = NULL

    # normalize the frequency count of every word and add a column
    # containing its category
	bw_keystring_count = normalizeFrequency(bw_keystring_count) %>% 
	    mutate(category = "box warning")

	contra_keystring_count = normalizeFrequency(contra_keystring_count) %>% 
	  mutate(category = "contraindications")

	warn_keystring_count = normalizeFrequency(warn_keystring_count) %>%
	  mutate(category = "warnings")
	##
	## End Yiwen Guo code
    ##

    ##
	## Begin Yatong Checn code
    ##
    # Combine the three data frames
	test = rbind(bw_keystring_count, contra_keystring_count, warn_keystring_count)
	test$total <- as.numeric(as.character(test$total))

	test = test %>% group_by(category) %>% select(-total) %>% 
	      mutate(id = 1:n()) %>%
	      spread(category, normalization) %>% select(-id)

	test$`box warning` = as.character(test$`box warning`)
	test$`box warning` = as.double(test$`box warning`)

	test$contraindications = as.character(test$contraindications)
	test$contraindications = as.double(test$contraindications)

	test$warnings = as.character(test$warnings)
	test$warnings = as.double(test$warnings)

    # replace NA with zeros and aggregate the rows with the same language
	test[is.na(test)] <- 0
	test <- aggregate(.~language, data = test, FUN = sum)

	return (test)

	##
	## End Yatong Chen code
    ##

}