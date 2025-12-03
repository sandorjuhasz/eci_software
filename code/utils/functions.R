# packages
library(fixest)
library(flextable)
library(officer)






# for the baseline table to produce most regressions -- programming langauge based
create_baseline_table <- function(main_input_path, iv_input_path) {
  df <- fread(main_input_path)
  
  # IVs from 01_data_prep_complexity.ipynb
  df_iv <- fread(iv_input_path) %>%
    select(iso2_code, year, avg_eci_similar_spec) %>%
    unique() %>%
    filter(year == 2020)
  df <- merge(
    df,
    df_iv,
    by = c("iso2_code", "year"),
    all.x = TRUE,
    all.y = FALSE
  )
  
  
  # manipulation
  df <- df %>%
    group_by(year) %>%
    mutate(
      log_gdp_usd = log10(gdp_current_USD),
      log_gdp_ppp = log10(gdp_ppp),
      gdp_ppp_pc = gdp_ppp / population,
      log_gdp_ppp_pc = log10(gdp_ppp_pc),
      log_gdp_ppp_pc2 = log_gdp_ppp_pc^2,
      ln_gdp_ppp_pc = log(gdp_ppp_pc),
      ln_gdp_ppp_pc2 = ln_gdp_ppp_pc^2,
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
      ln_pop = log(population),
      log_nat_res = log10(natural_resources),
      ln_nat_res = log(natural_resources)
    ) %>%
    data.table()
  
  return(df)
}






# add ECI table using co-occurrence clusters -- eci_clusters
# requires baseline table with language based variables already loaded
add_clusters_cooc_variables <- function(main_input_path, iv_input_path) {
  # load and transform cluster cooc based table
  eci_clusters <- fread(main_input_path) %>%
    dplyr::select(iso2_code, year, eci) %>%
    unique() %>%
    rename(eci_clusters = eci) %>%
    group_by(year) %>%
    mutate(eci_clusters_norm = scale(eci_clusters)) %>%
    data.table()
  
  # combine language and cluster based measures
  df <- merge(
    df,
    eci_clusters,
    by = c("iso2_code", "year"),
    all.x = TRUE,
    all.y = FALSE
  )
  
  # for eci_clusters instrument
  iv_clusters <- fread(iv_input_path) %>%
    dplyr::select(iso2_code, year, avg_eci_similar_spec) %>%
    unique() %>%
    rename(sim_eci_clusters = avg_eci_similar_spec) %>%
    group_by(year) %>%
    mutate(sim_eci_clusters_norm = scale(sim_eci_clusters)) %>%
    data.table()
  
  df <- merge(
    df,
    iv_clusters,
    by = c("iso2_code", "year"),
    all.x = TRUE,
    all.y = FALSE
  )
}




# to save etable regression table as a docx
save_etable_to_word <- function(etable_output, file_name = "../../data/outputs/etable_output.docx") {
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
