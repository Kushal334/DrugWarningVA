#' Create a word cloud for the box warning section
#'
#' @description Generate a word cloud with the most frequently appeared words
#'              in the box warning section
#'
#' @param box_warning contains all boxed warnings of all drugs
#' @param language contains key strings
#'
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom wordcloud wordcloud
#'
#' @keywords box warnings word cloud
#'
#' @examples
#' data(box_warning_data)
#' data(keystrings)
#' boxWarnWC(box_warning_data, keystrings)

boxWarnWC <- function(box_warning, language) {
    ##
	## Begin Yiwen Guo code
    ##
    
    language = as.character(language)
    our_stopwords = ourStopWords()

	# convert the data into a corpus, remove punctuations/ numbers/ stopwords
    # turn all words to lower cases, strip any white spaces
    keystring_count = processCorpus(box_warning, language)

    # count for each keystrings
    # keystring_count = countKeyString(bw_corpus, language)

    # generate word cloud
    palette <- brewer.pal(8, "Dark2")
    wordcloud(rownames(keystring_count), keystring_count, min.freq = 5, max.words = 100,
        scale=c(1.8, .35), random.order = FALSE, colors = palette)
    ##
    ## End Yiwen Guo code
    ##

}