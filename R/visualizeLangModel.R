#' Create a language model for the three warning sections
#'
#' @description Classify every key string as severe, moderate, or mild
#'              and visualize that result
#'
#' @param box_warning contains all boxed warnings of drugs
#' @param contraindications contains all contraindications of drugs
#' @param warnings contains all warnings of drugs
#' @param language contains key strings
#'
#' @export
#'
#' @importFrom tidyr gather_
#' @importFrom scales percent_format 
#' @importFrom ggplot2 ggplot aes geom_abline geom_jitter geom_text 
#'                     scale_x_continuous scale_y_continuous
#'                     scale_color_gradient facet_wrap theme labs
#'
#' @keywords language model visualization
#'
#' @examples
#' data(box_warning_data)
#' data(contraindications_data)
#' data(warning_data)
#' data(keystrings)
#' visualizeLangModel(box_warning_data, contraindications_data, warning_data, keystrings)

visualizeLangModel <- function(box_warning, contraindications, warnings, language) {
	##
	## Begin Yatong Chen code
    ##
 
    # Quite warnings
    proportion = `box warning` = NULL

	test = langModelTable(box_warning, contraindications, warnings, language)

	keycol <- "type"
	valuecol <- "proportion"
	gathercols <- c("contraindications", "warnings")

	# for plotting purposes, regroup the data as two columns
	# One for box warning (y-axis), one for contraindications and warnings (x-axis)
	test <- gather_(test, keycol, valuecol, gathercols)

    # the following chunk of codes for plotting is adapted from
	# https://www.tidytextmining.com/tidytext.html#word-frequencies
	ggplot(test, aes(x = proportion, y = `box warning`, color = abs(`box warning` - proportion))) +
	  geom_abline(color = "gray40", lty = 2) +
	  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
	  geom_text(aes(label = language), check_overlap = TRUE, vjust = 1.5) +
	  scale_x_continuous(labels = percent_format(), limits = c(NA,NA)) +
	  scale_y_continuous(labels = percent_format(), limits = c(NA,NA)) +
	  scale_color_gradient(limits = c(1e-9, 1), low = "darkslategray4", high = "gray75") +
	  facet_wrap(~type, ncol = 2) +
	  theme(legend.position="none") +
	  labs(y = "box warning", x = NULL)

	##
	## End Yatong Chen code
    ##

}