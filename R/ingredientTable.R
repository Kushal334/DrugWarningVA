#' Genereate the frequency table of different ingredients, which count the number of time each ingredient
#' contained in box warning for a drug, which can be seen as an indicator of danger.
#' 
#' 
#'
#' @description Genereate the frequency table of different ingredients, which count the number of time each ingredient
#'              contained in box warning for a drug, which can be seen as an indicator of danger.
#'
#' @param generic_name contains all ingredients of drugs
#' @param box_warning contains all boxed warnings of drugs
#'
#' @return a table
#'
#' @export 
#'
#' @importFrom dplyr mutate group_by summarize
#' @importFrom stats aggregate
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest spread
#' 
#' 
#' @keywords danger for each ingradient
#'
#' @examples
#' data(generic_name)
#' data(raw_box_warning_data)
#' ingredientTable(generic_name, raw_box_warning_data)

ingredientTable <-function(generic_name, box_warning){
  ##
  ## Begin Yatong Chen code
  ##
  
  # Quite warnings
  indicator = count = NULL
  
  for (i in 1:length(box_warning)){
    if(is.null(box_warning[[i]])==TRUE){
      box_warning[[i]] = "NULL"
    }
  }
  
  # change to lower case
  generic_name <-lapply(generic_name, tolower)
  #replace "and" by ","
  generic_name <- lapply(generic_name, function(x) sub("and",",", x))
  
  # combine generic_name and box_warning into generic_box
  generic_name <- as.matrix(generic_name)
  box_warning<-as.matrix(box_warning)
  generic_box <- cbind.data.frame(generic_name,box_warning)
  # delete all NULL for the generic col
  generic_box <- subset(generic_box, generic_name!="character(0)") 
  rownames(generic_box) <- 1:nrow(generic_box)
  # check whether box_warning is NULL or not
  generic_box <- mutate(generic_box,indicator = (generic_box$box_warning !="NULL"))
  # unlist the generic_name
  generic_box[[1]] = unlist(generic_box[[1]])
  # delete box_warning
  generic_box$box_warning <- NULL
  
  # expand the generic_box
  generic_box <- generic_box%>%  mutate(generic_name = strsplit(generic_name, ",")) %>% unnest(generic_name)
  generic_name_temp <- lapply(generic_box$generic_name, function(x) trimws(x, which = c("both", "left", "right")))
  generic_name_temp1 = unlist(generic_name_temp)
  generic_name_temp2 <- as.matrix(generic_name_temp1)
  generic_box$generic_name <- generic_name_temp2
  
  # made into long and wide table
  summarize_table_long <-generic_box %>% group_by(generic_name, indicator) %>% summarize(count = n()) 
  # only keep those with more than 10 in the count section
  summarize_table_long <- subset(summarize_table_long, count>=5)
  
  summarize_table_wide <- spread(summarize_table_long, indicator, count)
  colnames(summarize_table_wide) = c("generic name","Do not contain Box Warnings","Contain Box Warnings") 
  
  summarize_table_wide[is.na(summarize_table_wide)] <- 0
  # delete all rows with less than three character name
  ingredient_table = summarize_table_wide[(which(nchar(summarize_table_wide$`generic name`) > 3)),]
  ingredient_table$`generic name`  <- gsub("[^0-9A-Za-z///' ]","" , ingredient_table$`generic name` ,ignore.case = TRUE)
  df = ingredient_table
  ingredient_table = aggregate(.~`generic name`, data=df, FUN=max)
  return(ingredient_table)
  ##
  ## End Yatong Chen code
  ##
}
