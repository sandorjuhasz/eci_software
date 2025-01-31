# packages
library(fixest)
library(flextable)
library(officer)



save_etable_to_word <- function(etable_output, file_name = "../outputs/etable_output.docx") {
  # convert etable output to a dataframe
  etable_df <- as.data.frame(etable_output)
  
  # ensure it has proper column names
  etable_df <- data.frame(etable_df, check.names = FALSE, stringsAsFactors = FALSE)
  
  # create a flextable
  etable_ft <- flextable(etable_df)
  
  # create a new Word document
  doc <- read_docx()
  
  # add flextable to document
  doc <- body_add_flextable(doc, etable_ft)
  
  # save to file
  print(doc, target = file_name)
  
  message("etable saved to ", file_name)
}
