#' Create a word cloud for the contraindications section
#'
#' @description Generate a word cloud with the most frequently appeared words
#'              in the contraindications section
#'
#' @param contra contains all contraindications of drugs
#' @param language  contains key strings
#'
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom wordcloud wordcloud
#'
#' @keywords contraindications word cloud
#'
#' @examples
#' data(contraindications_data)
#' data(keystrings)
#' contraWC(contraindications_data, keystrings)

contraWC <- function(contra, language) {
    ##
    ## Begin Yiwen Guo code
    ##
    
    language = as.character(language)
    our_stopwords = ourStopWords()

    # convert the data into a corpus, remove punctuations/ numbers/ stopwords
    # turn all words to lower cases, strip any white spaces
    keystring_count = processCorpus(contra, language)
    
    # count for each keystrings
    # keystring_count = countKeyString(contra_corpus, language)

    # generate word cloud
    palette <- brewer.pal(8, "Dark2")
    wordcloud(rownames(keystring_count), keystring_count, min.freq = 5, max.words = 100,
        scale=c(1.8, .45), random.order = FALSE, colors = palette)
    ##
    ## End Yiwen Guo code
    ##

}