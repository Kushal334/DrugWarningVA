#' Create and process a corpus
#'
#' @description Generate a corpus, remove punctuations/ numbers/ stopwords,
#'              turn all words to lower cases and strip any white spaces
#'
#' @param data a list of items to be converted to a corpus item
#' @param language contains key strings
#'
#' @return a matrix that contains the number of occurrences 
#'         in key strings
#'
#' @export
#'
#' @importFrom tm Corpus VectorSource removePunctuation removeNumbers
#'                stopwords removeWords stripWhitespace
#'                PlainTextDocument tm_map
#' @importFrom magrittr %>%
#' @importFrom stringr str_count
#'
#' @keywords corpus
#'
#' @examples
#' data(box_warning_data)
#' data(keystrings)
#' processCorpus(box_warning_data, keystrings)

processCorpus <- function(data, language) {
	##
	## Begin Yiwen Guo code
    ##
    language = as.character(language)
	our_stopwords = ourStopWords()

	# convert the data into a corpus, remove punctuations/ numbers/ stopwords
    # turn all words to lower cases, strip any white spaces
	corpus_item <- Corpus(VectorSource(data)) %>%
    	tm_map(removePunctuation) %>%
    	tm_map(removeNumbers) %>%
    	tm_map(tolower)  %>%
    	tm_map(removeWords, our_stopwords) %>%
    	tm_map(stripWhitespace) %>%
    	tm_map(PlainTextDocument)
    corpus_item <- Corpus(VectorSource(corpus_item))

    keystring_count<-str_count(corpus_item[["1"]][["content"]], language)

    # add row names
    keystring_count<-as.matrix(keystring_count)
    language<-as.matrix(language)
    rownames(keystring_count) = language

    # remove all zero terms in keystring_count
    keystring_count = keystring_count[apply(keystring_count!=0, 1, all),]
    keystring_count<-as.matrix(keystring_count)

    ##
	## End Yiwen Guo code
    ##
}