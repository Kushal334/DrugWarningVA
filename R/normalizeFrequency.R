#' Normalize the frequency in a given data set
#'
#' @description Normalize the frequency in a given data set
#'
#' @param data counts the number of occurrences of keystrings
#'
#' @return a data drame
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @keywords normalize frequency
#'
#' @examples
#' data(box_warning_data)
#' data(keystrings)
#' bw_keystring_count <- processCorpus(box_warning_data, keystrings)
#' normalizeFrequency(bw_keystring_count)

normalizeFrequency <- function(data) {
	##
	## Begin Yiwen Guo code
    ##
	# normalize the frequency count of every word
	new_col = lapply(data, function(x) x/max(data)) %>%
	  unlist() %>%
	  as.matrix()
	
	# add the new column to data
	data = cbind(data, new_col, rownames(data))
	colnames(data) = c("total", "normalization", "language")
	data = as.data.frame(data)
	##
	## End Yiwen Guo code
    ##
}