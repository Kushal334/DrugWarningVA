#' Create a vector of stop words for generating word cloud
#'
#' @description Create a vector of stop words which are to be removed
#'              from the corpus
#'
#'
#' @importFrom tm stopwords
#'
#' @export
#'
#' @keywords stop words
#'
#' @examples
#' ourStopWords()

ourStopWords <- function() {
    ##
    ## Begin Yiwen Guo code
    ##
    our_stopWords <- union(stopwords(),stopwords('SMART'))
    our_stopWords <- union(our_stopWords, c("study", "studies", "studied", 
                        "dose", "dosage", "reported","report", "form", 
                        "during", "result", "disease", "information",
                        "developed", "development", "number", "activity",
                        "compared", "effect", "usage", "use", "used",
                        "patients", "warnings", "warning", "precautions", 
                        "precaution", "cases", "events", "see", "and",
                        "including", "includes", "therapy", "treatment", 
                        "contraindications", "contraindicated","risk",
                        "symptoms", "risks", "null", "patient", "taking",
                        "days", "time", "drug", "drugs", "doctor", "increase",
                        "increases", "increasing", "occur", "occurrence",
                        "years", "effects", "effect", "increased"))
    ##
    ## End Yiwen Guo code
    ##
}