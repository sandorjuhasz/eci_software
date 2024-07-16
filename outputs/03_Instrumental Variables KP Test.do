*-----------------------------------------------------------------------------------------------------
* Environment
*-----------------------------------------------------------------------------------------------------
clear all
set more off

* Set the working directory manually
cd "/Users/jermainkaminski/Documents/GitHub/git_complexity/outputs/"

* Load necessary packages
cap which estout
if _rc ssc install estout

cap which ivreg2
if _rc ssc install ivreg2

* Import the main dataset
import delimited "eci_comparisons_2020.csv", clear
rename gdpcap_o gdp_cap
rename pop_o population
rename nat_res nat_res
save "eci_comparisons_2020.dta", replace

* Import the IV dataset
import delimited "si_eci_software_2020_2023_ivreg.csv", clear
keep iso2_code year avg_eci_similar_spec
duplicates drop iso2_code year avg_eci_similar_spec, force
keep if year == 2020
save "si_eci_software_2020_2023_ivreg.dta", replace
use "eci_comparisons_2020.dta", clear

* Merge the datasets
merge m:1 iso2_code year using "si_eci_software_2020_2023_ivreg.dta", nogenerate

* Check for duplicates after the merge
duplicates report iso2_code year avg_eci_similar_spec

* Check the number of observations
count
* Transformations
gen log_gdp_pc = log10(gdp_cap)
gen log_gdp_pc2 = log_gdp_pc^2
egen gini_norm = std(gini_mean)
egen emission_norm = std(emissions)
egen software_eci_norm = std(software_eci_2020)
egen trade_eci_norm = std(trade_eci_2020)
egen tech_eci_norm = std(tech_eci_2020)
egen research_eci_norm = std(research_eci_2020)
gen log_pop = log10(population)
gen log_nat_res = log10(nat_res)
egen sim_software_eci_norm = std(avg_eci_similar_spec)

* GDP vs ECI
* Drop rows with NAs in key columns
drop if missing(log_gdp_pc, software_eci_norm, trade_eci_norm, tech_eci_norm, research_eci_norm, log_pop, log_nat_res, sim_software_eci_norm)

ivreg2 log_gdp_pc (software_eci_norm = sim_software_eci_norm) log_pop log_nat_res, robust
ivreg2 log_gdp_pc (software_eci_norm = sim_software_eci_norm) trade_eci_norm log_pop log_nat_res, robust
ivreg2 log_gdp_pc (software_eci_norm = sim_software_eci_norm) tech_eci_norm log_pop log_nat_res, robust
ivreg2 log_gdp_pc (software_eci_norm = sim_software_eci_norm) research_eci_norm log_pop log_nat_res, robust
ivreg2 log_gdp_pc (software_eci_norm = sim_software_eci_norm) trade_eci_norm tech_eci_norm research_eci_norm log_pop log_nat_res, robust

* Gini vs ECI
* Drop rows with NAs in key columns
drop if missing(gini_norm, software_eci_norm, trade_eci_norm, tech_eci_norm, research_eci_norm, log_pop, log_nat_res, sim_software_eci_norm)

ivreg2 gini_norm (software_eci_norm = sim_software_eci_norm) log_gdp_pc log_gdp_pc2 log_pop log_nat_res, robust
ivreg2 gini_norm (software_eci_norm = sim_software_eci_norm) log_gdp_pc log_gdp_pc2 trade_eci_norm log_pop log_nat_res, robust
ivreg2 gini_norm (software_eci_norm = sim_software_eci_norm) log_gdp_pc log_gdp_pc2 tech_eci_norm log_pop log_nat_res, robust
ivreg2 gini_norm (software_eci_norm = sim_software_eci_norm) log_gdp_pc log_gdp_pc2 research_eci_norm log_pop log_nat_res, robust
ivreg2 gini_norm (software_eci_norm = sim_software_eci_norm) log_gdp_pc log_gdp_pc2 trade_eci_norm tech_eci_norm research_eci_norm log_pop log_nat_res, robust

* Emissions vs ECI
* Drop rows with NAs in key columns
drop if missing(emission_norm, software_eci_norm, trade_eci_norm, tech_eci_norm, research_eci_norm, log_pop, log_nat_res, sim_software_eci_norm)

ivreg2 emission_norm (software_eci_norm = sim_software_eci_norm) log_gdp_pc log_pop log_nat_res, robust
ivreg2 emission_norm (software_eci_norm = sim_software_eci_norm) trade_eci_norm log_gdp_pc log_pop log_nat_res, robust
ivreg2 emission_norm (software_eci_norm = sim_software_eci_norm) tech_eci_norm log_gdp_pc log_pop log_nat_res, robust
ivreg2 emission_norm (software_eci_norm = sim_software_eci_norm) research_eci_norm log_gdp_pc log_pop log_nat_res, robust
ivreg2 emission_norm (software_eci_norm = sim_software_eci_norm) trade_eci_norm tech_eci_norm research_eci_norm log_gdp_pc log_pop log_nat_res, robust
