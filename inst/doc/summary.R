## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show='hold'----------------------------------------------------
library(DrugWarningVA)
boxWarnWC(box_warning_data, keystrings)
fig.cap = "Word Cloud for key words/strings which apprears in the severe warning('box warning') section"

## ---- fig.show='hold'----------------------------------------------------
contraWC(contraindications_data, keystrings)
fig.cap = "Word Cloud for key words/strings which apprears in the moderate warning('contraindication') section"

## ---- fig.show='hold'----------------------------------------------------
warningWC(warning_data, keystrings)
fig.cap = "Word Cloud for key words/strings which apprears in the warning section"

## ---- fig.show='hold'----------------------------------------------------
visualizeLangModel(box_warning_data, contraindications_data, warning_data, keystrings)
fig.cap = "comparison between keywords from different levels of warning"

## ---- fig.show='hold'----------------------------------------------------
table = langModelTable(box_warning_data,  contraindications_data, warning_data, keystrings) 
head(table)
tail(table)
fig.cap = "Unnormalized probabilities for keywords to be in different warning section"

## ---- fig.show='hold'----------------------------------------------------
languageModel(box_warning_data,  contraindications_data, warning_data, keystrings,"coronary artery bypass")
fig.cap = "Probabilities of 'coronary artery bypass' for three different tones(severe/moderate/mild)"

## ---- fig.show='hold'----------------------------------------------------
table2 = ingredientTable(generic_name, raw_box_warning_data)
head(table2)
tail(table2)
fig.cap = "Times for different ingredients to be contained/ not contained in a drug's box warnings section"

