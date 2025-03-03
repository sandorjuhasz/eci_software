*-----------------------------------------------------------------------------------------------------
* Environment Setup
*-----------------------------------------------------------------------------------------------------
clear all
set more off

* Set working directory
cd "/Users/jermainkaminski/Documents/GitHub/eci_software/outputs/"

* Load necessary packages
cap which estout
if _rc ssc install estout

cap which ivreg2
if _rc ssc install ivreg2

*-----------------------------------------------------------------------------------------------------
* Load and Prepare Data
*-----------------------------------------------------------------------------------------------------

* Load baseline table
import delimited "eci_regression_table.csv", clear
duplicates drop iso2_code year, force
tempfile baseline_data
save `baseline_data'

* Load IV dataset
import delimited "si_eci_software_2020_2023_ivreg.csv", clear
keep iso2_code year avg_eci_similar_spec
duplicates drop iso2_code year avg_eci_similar_spec, force
keep if year == 2020

* Merge IV data directly with main dataset (must convert to Stata format)
merge 1:1 iso2_code year using `baseline_data', nogenerate

*-----------------------------------------------------------------------------------------------------
* Data Transformations
*-----------------------------------------------------------------------------------------------------
gen log_gdp_ppp = log10(gdp_ppp)
gen gdp_ppp_pc = gdp_ppp / population
gen log_gdp_ppp_pc = log10(gdp_ppp_pc)
gen log_gdp_ppp_pc2 = log_gdp_ppp_pc^2
egen gini_norm = std(gini_mean)
egen gini_2020_2022_norm = std(gini_mean_2020_2022)
gen log_emission = log10(total_ghg_emissions)
gen emission_per_gdp = total_ghg_emissions / gdp_ppp
gen log_emission_per_gdp = log10(total_ghg_emissions / gdp_ppp)
egen eci_software_norm = std(eci_software)
egen eci_trade_norm = std(eci_trade)
egen eci_tech_norm = std(eci_tech)
egen eci_research_norm = std(eci_research)
gen log_pop = log10(population)
gen log_nat_res = log10(natural_resources)
egen sim_eci_software_norm = std(avg_eci_similar_spec)

* Drop rows with missing values in key variables
drop if missing(log_gdp_ppp_pc, eci_software_norm, eci_trade_norm, eci_tech_norm, eci_research_norm, log_pop, log_nat_res, sim_eci_software_norm)

*-----------------------------------------------------------------------------------------------------
* Baseline Model
*-----------------------------------------------------------------------------------------------------
foreach depvar in log_gdp_ppp_pc gini_norm log_emission_per_gdp {
    
    if "`depvar'" == "log_gdp_ppp_pc" {
        ivreg2 `depvar' (eci_software_norm = sim_eci_software_norm) ///
            log_pop log_nat_res, robust first endog(eci_software_norm)
    }
    else {
        ivreg2 `depvar' (eci_software_norm = sim_eci_software_norm) ///
            log_gdp_ppp_pc log_pop log_nat_res, robust first endog(eci_software_norm)
    }

    est store b_`depvar'
    
    * Store IV diagnostics
    estadd scalar KP_LM = e(idstat)
    estadd scalar KP_pval = e(idp)
    estadd scalar KP_WF = e(rkf)
    estadd scalar hansen = e(j)

    * Store Durbin-Wu-Hausman (DWH) test results
    estadd scalar DWH_chi2 = e(estat)
    estadd scalar DWH_pval = e(estatp)
}

*-----------------------------------------------------------------------------------------------------
* Full Model
*-----------------------------------------------------------------------------------------------------
foreach depvar in log_gdp_ppp_pc gini_norm log_emission_per_gdp {

    if "`depvar'" == "log_gdp_ppp_pc" {
        ivreg2 `depvar' (eci_software_norm = sim_eci_software_norm) ///
            eci_trade_norm eci_tech_norm eci_research_norm log_pop log_nat_res, robust first endog(eci_software_norm)
    }
    else {
        ivreg2 `depvar' (eci_software_norm = sim_eci_software_norm) ///
            eci_trade_norm eci_tech_norm eci_research_norm log_gdp_ppp_pc log_pop log_nat_res, robust endog(eci_software_norm)
    }

    est store f_`depvar'
    
    * Store IV diagnostics
    estadd scalar KP_LM = e(idstat)
    estadd scalar KP_pval = e(idp)
    estadd scalar KP_WF = e(rkf)
    estadd scalar hansen = e(j)

    * Store Durbin-Wu-Hausman (DWH) test results
    estadd scalar DWH_chi2 = e(estat)
    estadd scalar DWH_pval = e(estatp)
}

*-----------------------------------------------------------------------------------------------------
* Export
*-----------------------------------------------------------------------------------------------------
estout b_log_gdp_ppp_pc b_gini_norm b_log_emission_per_gdp ///
       f_log_gdp_ppp_pc f_gini_norm f_log_emission_per_gdp ///
    using "IV_Results_Merged.xls", replace ///
    cells(b(fmt(3) star) se(par fmt(3))) ///
    drop(_cons) ///
    mlabels("Baseline GDP" "Baseline Gini" "Baseline Emissions" ///
            "Full Model GDP" "Full Model Gini" "Full Model Emissions") ///
    stats(N r2 KP_LM KP_pval KP_WF hansen DWH_chi2 DWH_pval, ///
          labels("Observations" "R-squared" "Kleibergen-Paap LM" "Chi2 P-value" "Weak ID F-stat" "Hansen J-stat" ///
                 "Durbin-Wu-Hausman Chi2" "DWH P-value")) ///
    starlevels(* 0.1 ** 0.05 *** 0.01) ///
    title("IV (2SLS) Estimation: Merged Baseline and Full Models")
