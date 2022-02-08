#' Create a word cloud for the warning sections
#'
#' @description Generate a word cloud with the most frequently appeared words
#'              in the warning sections
#'
#' @param warning contains all warnings of drugs
#' @param language contains key strings
#' 
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom wordcloud wordcloud
#'
#' @keywords warnings precautions word cloud
#'
#' @examples
#' data(warning_data)
#' data(keystrings)
#' boxWarnWC(warning_data, keystrings)

warningWC <- function(warning, language) {
    ##
    ## Begin Yiwen Guo code
    ##
	
    language = as.character(language)
    our_stopwords = ourStopWords()
	
	# convert the data into a corpus, remove punctuations/ numbers/ stopwords
    # turn all words to lower cases, strip any white spaces
    warn_keystring_count = processCorpus(warning, language)

    # count for each keystring
    # warn_keystring_count = countKeyString(warn_corpus, language)

    # generate word cloud
    palette <- brewer.pal(8, "Dark2")
    wordcloud(rownames(warn_keystring_count), warn_keystring_count, 
        min.freq = 5, max.words = 150,
        scale=c(1.5, .3), random.order = FALSE, colors = palette)
    ##
    ## End Yiwen Guo code
    ##
}
