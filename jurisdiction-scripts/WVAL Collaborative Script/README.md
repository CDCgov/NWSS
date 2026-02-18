# WVAL R Code Translation

- This code is maintained by CO and WA. Please each out to Kirsten Weisbeck (kirsten.weisbeck@state.co.us) or Bradie Ahern (bradie.ahern@doh.wa.gov) if there are any questions. 


NOTE: Since each jurisdiction's data looks different this code will likely need to be adjusted to fit your data needs.


## Overview: 
- This is the collaborative NWSS CoE R translation of CDC's python code to calculate wastewater viral activity level (WVAL) for SARS-CoV-2, Influenza, and RSV with the NEW updated methodology published in August 2025. 

- WVAL categorizes viral concentration into minimal, low, moderate, high, and very high to help indicate the risk of infection. 


## From CDC's methodology: 

### Calculating the Wastewater Viral Activity Level

### Major Changes: 
  - No longer using normalized data
  - Baseline lookback period is 24 months for all respiratory targets (SARS-CoV-2, Flu A, RSV)
  - Requires 8 weeks of data 
  - Biannual recalculation dates for SARS-CoV-2 changed to April 1 and October 1
  - Updated WVAL level cut off points

### Baseline Calculation:
  - For each combination of site, data submitter, PCR target, lab methods, and normalization method, a baseline is established. The “baseline” is the 10th percentile of the log-transformed concentration data within a specific time frame. Details on the baseline calculation by pathogen are below:
  
  - SARS-CoV-2
    - For site and method combinations (as listed above) with over 6 months of data, baselines are re-calculated every six calendar months (April 1st and October 1st) using the past 24 months of data.
    - For sites and method combinations with less than 6 months of data, baselines are computed every time there is a new sample until reaching six months, after which they remain unchanged until the next April 1st or October 1st, at which time baselines are re-calculated.
    
  - Influenza A and RSV
    - For site and method combinations (as listed above) with over 12 months of data, baselines are re-calculated every August 1st using all available data in the previous 24 months.
    - For sites and method combinations with less than 12 months of data, baselines are computed weekly until reaching twelve months, after which they remain unchanged until the next August 1st, at which time baselines are re-calculated.

- The standard deviation for each site and method combination is calculated using the same time frame as the baseline.

### Wastewater Viral Activity Level Calculation:
  - The number of standard deviations that each log-transformed concentration value deviates from the baseline (positive if above, negative if below) is calculated.
  - This value (x) is then converted back to a linear scale (by calculating e^x) to form the Wastewater Viral Activity Level for the site and method combination.
  - The Wastewater Viral Activity Levels from a site are averaged by week for all figures.

  - This reflects the updated cut points: 
    - Wastewater Viral Activity Level Categories for SARS-CoV-2 (https://www.cdc.gov/nwss/about-data.html#data-method, Feb 2025): 
    - The current Wastewater Viral Activity Level for each state and territory is categorized into very low, low, moderate, high, or very high as follows:
    
    SARS-CoV-2:
      - Very Low: Up to 2
      - Low: Greater than 2 and up to 3.4	
      - Moderate: Greater than 3.4 and up to 5.3	
      - High: Greater than 5.3 and up to 7.8	
      - Very High: Greater than 7.8
    
    Influenza A:
      - Very Low: Up to 2.7
      - Low: Greater than 2.7 and up to 6.5	
      - Moderate: Greater than 6.2 and up to 11.2	
      - High: Greater than 11.2 and up to 17.6	
      - Very High: Greater than 17.6
    
    RSV:
      - Very Low: Up to 2.5	
      - Low: Greater than 2.5 and up to 5.2	
      - Moderate: Greater than 5.2 and up to 8	
      - High: Greater than 8 and up to 11	
      - Very High: Greater than 11

### Aggregation for National, Regional, and State Levels:
  - We calculate the median Wastewater Viral Activity Levels among sites at national, regional, and state levels, excluding data from site/method combinations with less than 8 weeks of data for SARS-CoV-2, Influenza A, and RSV.

### Data Inclusion Criteria – SARS-COV-2, Influenza A, RSV Wastewater Viral Activity Level
  - SARS-CoV-2: New wastewater sampling sites, or sites with a substantial change in laboratory methods are included in national, regional, state, or territorial median values once there are at least 8 weeks of samples reported for that location.
  - Influenza A and RSV: New wastewater sampling sites, or sites with a substantial change in laboratory methods, are included in national and state or territorial median values beginning on August 1st of each year once there are at least 8 weeks of samples reported for that pathogen. Data must be reported by October 1st of that year to be included in national and state or territorial median values for that respiratory virus season. If data are reported after October 1st of that year, they will not be displayed until August 1st of the following year.

## Data Needs

### Input Data
      - 1CDP downloaded MLM analytics file OR 
      - Raw data from your jurisdiction 
  
  - Dataset Variables Needed (using 1CDP-ready column names)
      - wwtp_jurisdiction
      - source: the source of the data submitted to 1CDP (NWSS, WWS, CDC)
      - lab_id: ID assigned to the testing lab
      - site_id: site ID 
      - wwtp_name: name of wastewater treatment facility sample collection location
      - population_served: sewershed population
      - pcr_target: pathogen (sars-cov-2, fluav, rsv)
      - pcr_gene_target: specific pathogen gene target being tested (i.e., n for SARS-CoV-2)
      - pcr_target_avg_conc: SARS-CoV-2 raw concentration in gene copies/L
      - lod_sewage: limit of detection
      - county_names: county containing wastewater treatment facility sample collection location
      - concentration_method: method used to concentrate the sample prior to analysis of the concentrate
      - extraction_method: method used for nucleic acid extraction from the sample
      - major_lab_method: A number used to distinguish major lab methods at the reporting jurisdiction level
      - sample_location: Sample collection location in the wastewater system, whether at a wastewater treatment plant or upstream in the wastewater system.
      - sample_location_specify: If 'sample_location' is "upstream", specify the collection location in the wastewater system; an arbitrary name may be used if you do not wish to disclose the real name. 
      - sample_matrix: Wastewater matrix from which the sample was collected.
      
  - NOTE: make sure the pcr_target for all sars-cov-2 data is 'sars-cov-2'
      

## Depdencies
 - Libraries: pacman, tidyverse, lubridate, here, REDCapR, sf, zoo, ggformula, slider, stringr, ggplot2
      
## Compare your jurisdiction's data to CDC's
  - The last function allows you to compare your WVAL results to CDC's WVAL results and find any discordance. 
  - Some jurisdictions found better concordance when line 522 (currently commented out) was included

## Security
  - Never store sensitive data including passwords and tokens in your code or wastewater datasets in GitHub.
  - If you suspect that sensitive data has been exposed, please notify the maintainers immediately.
  
