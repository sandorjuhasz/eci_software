# packages
library(fixest)
library(flextable)
library(officer)






# for the baseline table to produce most regressions
create_baseline_table <- function(input_path) {
  df <- fread(input_path)
  
  # manipulation
  df <- df %>%
    group_by(year) %>%
    mutate(
      log_gdp_usd = log10(gdp_current_USD),
      log_gdp_ppp = log10(gdp_ppp),
      gdp_ppp_pc = gdp_ppp / population,
      log_gdp_ppp_pc = log10(gdp_ppp_pc),
      log_gdp_ppp_pc2 = log_gdp_ppp_pc^2,
      gini_norm = scale(gini_mean),
      gini_2020_2022_norm = scale(gini_mean_2020_2022),
      log_emission = log10(total_ghg_emissions),
      emission_per_gdp = (total_ghg_emissions / gdp_ppp),
      log_emission_per_gdp = log10(total_ghg_emissions / gdp_ppp),
      eci_software_norm = scale(eci_software),
      eci_trade_norm = scale(eci_trade),
      eci_tech_norm = scale(eci_tech),
      eci_research_norm = scale(eci_research),
      log_pop = log10(population),
      log_nat_res = log10(natural_resources)
    ) %>%
    data.table()
  
  return(df)
}






# to save etable regression table as a docx
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
