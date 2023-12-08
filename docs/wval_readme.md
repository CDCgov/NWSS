# Wastewater Virus Activity Level (WVAL) Calculation Docs

This document provides details on two scripts executing the following:
- Calculation of weekly WVALs at the site level
- Subsequent aggregation of these values to the state, regional, and national levels to support the NWSS dashboards hosted [here](https://www.cdc.gov/nwss/index.html).

Note that the WVAL pipeline is hosted within the [Palantir Foundry Code Repository](https://www.palantir.com/docs/foundry/code-repositories/overview/) development environment and is written using Python v3.8 and the [PySpark](https://spark.apache.org/docs/latest/api/python/index.html) library, an API for using Apache Spark and distributed computing.

## Weekly Site-level WVAL Calculation

### Filtering and Joining Geography Columns

The script joins in the following geographical columns:
- HHS Region
- US Census region. This uses filters found in `src/myproject/utils/filters.py`

The following filters are applied:
- pcr target is sars-cov-2
- certain tribal and institutional data is filtered, consistent with CDT data practices
- rows with null site_ids are filtered
- rows which are not MLM-adjusted are filtered
- upstream sites are removed

### Data Shaping and Outlier Handling

- Through several steps, a `site_id_with_pcr_source_mlm_norm` column is created to ensure the WVALs are only calculated using self-consistent data.
- New natural log transformed concentration variables are created from linear normalized concentration
- Outliers with a Z-score > 4 are filtered
    - z-scores calculated for each `site_id_with_pcr_source_mlm_norm` combination
    - z-score = (sample concentration - mean concentration) / standard_deviation

### WVAL Calculation
*Note - in the code, WVAL is referred to as `ww_index`.*

WVALs are calculated on a `site_id_with_pcr_source_mlm_norm` (hereafter, site-combo) basis for each sample.
- A WVAL is only calculated for a sample if its site-combo has been sampling >= 42 days
- WVAL is preferentially calculated using **flowpop-normalized** concentration, if available, and microbial-normalized concentration otherwise.
- WVAL is first calculated using natural log transformed concentration as:
    - (sample concentration - baseline concentration) / standard deviation
    - see details on baseline calculation in subsection
- Finally, the WVAL is exponentiated by e (back transformed) to get the WVAL in linear units which we have found to be preferable for graphing
```python
  # Calculate ww_index_lin as the number of standard deviations from the 10th percentile baseline
    # Only for samples that are more than 6 weeks from the first sample
    all_final = all_final.withColumn(
        "ww_index_normed_ln",
        F.when(
            F.col("days_since_first_sample") < 42, None
        ).when(
            F.col("pcr_target_flowpop_ln").isNotNull(), (F.col("pcr_target_flowpop_ln") -
                                                         F.col("last_baseline_ln")) / F.col("last_std_ln")
        ).otherwise(
            (F.col("pcr_target_mic_ln") - F.col("last_baseline_ln")) / F.col("last_std_ln")
        )
    )
    # Add linear transformed ww_index
    all_final = all_final.withColumn(
        "ww_index_normed_ln_lin",
        F.pow(math.e, F.col("ww_index_normed_ln"))
    )
```

#### Baseline and Standard Deviation Calculation

Baseline and standard deviation are calculated in the same manner, except for the detail of what statistical measure is calculated.

In the most general sense, when we say "baseline" we are referring to the tenth percentile concentration of the distribution of sample concentrations for one `site_id_with_pcr_source_mlm_norm` combination.

However, the time window / frequency of updating the baseline depends on how long a site-combo has been sampling:
- For the first 6 months a site-combo is online, the baseline is updated each time there is a new sample
- At the 6 month point, baseline is held constant at the last calculated value until the next update check point
- After the 6 month point, the baseline is updated for a site-combo semi-yearly on January 1 and July 1.

After the preceding row to row calculations, the final step is to retrieve the last calculated baseline and standard deviation for a site combo.
We then ensure that these "last values" are used to calculate the WVAL for all samples for a given site-combo.

The motivation for this last step is to enable stable year-to-year comparisons wherein all the WVALs for a site-combo on a graph are referencing the same baseline.

##### Baseline Implementation Details

The baseline calculation has three major steps, each corresponding to 3 different variables (2 intermediate, one final)

1. The `temp_baseline` is calculated for rows that match the update requirements mentioned above:
    ```python
    # First, calculate the baseline where needed (first 6 months or nearest to Jan 1/Jul 1)
        all_final = all_final.withColumn(
            "temp_baseline",
            F.when(
                (F.col("days_since_first_sample") <= 182) |
                (F.col("days_to_nearest_target") == F.col("min_days_to_target")),
                F.percentile_approx("metric_column_norm_ln", 0.1).over(percentile_range_window)
            ).otherwise(F.lit(None))
        )
    ```
    - When the baseline is calculated, it uses the last 12 months of data for the site-combo:
    ```python
      percentile_range_window = Window.partitionBy("site_id_with_pcr_source_mlm_norm")\
        .orderBy(F.col("sample_collect_date").cast("timestamp").cast("long"))\
        .rangeBetween(-365 * 24 * 60 * 60, Window.currentRow)  # 12 months in seconds
    ```
    - The logic first assesses if the site-combo has been online < 182 days and calculates baseline iteratively for rows meeting that condition
    - If the site has been online > 182 days, the logic assesses if the current sample collect date (hereafter, date) is the closest date in time *after* either Jan 1 or Jul 1
        - To do this, the script determines what half of the year the current row's date is in: Jan to Jun or Jul to Dec
        - Then the logic assesses if the number of days from the current date to either Jan 1 or Jul 1 (depending on half of year) matches the minimum distance for the window determined by the relevant site-combo, year, and half-year.
        ```python
        # window to determine the minimum days to the nearest target date for each site in each half year
        window_min_date = Window.partitionBy(
            "site_id_with_pcr_source_mlm_norm",
            "year",
            "half_year"
        )
        ```
        - See code on lines 118 to 162 for helper functions and variables used
    - If neither of above conditions are met (the sample collect date is in between update check points) the baseline is initally left as *null*
2.  The row-to-row `baseline_norm_ln` variable is created by filling in the null values in `temp_baseline` with the first non-null value in previous rows. This satisfies the requirement of holding baseline constant in between updated check points.
    
    ```python
    # Baseline update window: should look back to the first record, not forward
    baseline_update_window = Window.partitionBy("site_id_with_pcr_source_mlm_norm").orderBy(
        "sample_collect_date").rowsBetween(Window.unboundedPreceding, 0)
    ```
    ```python
    # Now carry the last known baseline forward using the baseline_update_window
    all_final = all_final.withColumn(
        "baseline_norm_ln",
        F.last(F.col("temp_baseline"), True).over(baseline_update_window)
    )
    ```
3. To get the `last_baseline_ln`, the dataset is aggregated by site-combo and the last baseline value calculated is retrieved.
    ```python
    # BASELINE AND STD BACK APPLY ###
        # get last calculated baseline and std to apply to all ww index calcs for combo
        last_vals = all_final.groupby("site_id_with_pcr_source_mlm_norm").agg(
            F.last("baseline_norm_ln").alias("last_baseline_ln"),
            F.last("std_norm_ln").alias("last_std_ln"),
        )
    ```

### Edge Case Handling and Preparation for Aggregation

While the baseline, standard deviation, and WVAL are initially calculated on a `site_id_with_pcr_source_mlm_norm` level, the final goal of the script is to deliver weekly aggregated WVALs on the `site_id_with_pcr` level.

To facilitate this goal, certain edge cases need to handled so that the WVALs which are being aggregated within a given week for a site are comparable.

1. In a given week, only WVALs calculated with concentration variables using the same normalization method should be present.
    - WVALs calculated using flowpop-normalized concentration are preferentially retained. WVALs calculated using microbial-normalized concentration are only retained if flowpop-backed WVALs are not present for the week.
2. If more than one source-MLM combination is reporting for a given site in a given week, only WVALs from the most recently onboarded source-MLM are retained for that week.

### Final Aggregation

The dataset is aggregated by `site_id_with_pcr` and week (Sunday to Saturday) and **mean** WVAL is returned.
