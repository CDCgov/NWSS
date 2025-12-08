# This script estimates the bsts model and runs the trend classification on the
# most recent data.  This script should be used to generate the current estimate
# and forecast.
p_load(tidyverse,sf,lubridate,bsts,broom,scales,conflicted,progress)

#Resolves conflicts between functions from different packages
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("last", "dplyr")

#############################
#Parameters:
p_val_threshold=.05 #cutoff above which plateau is defined
n_obs=5 #number of observations necessary to estimate model
baye_iter=2000 #number of iterations for bayesian model - stable at 2000
burnin = 500 # Throw away first 500 iterations - standard practive for bayesian modeling
forecast_horizon=7 #days out to forecast
trend_window=21 #number of days back to include when classifying trend (days not observations)

##############################
#Define list of utilities 
utility_count <- ww_summ_pop %>%
  count(utility) %>%
  arrange(desc(n))

#x <- utility_count$utility[30] #pick one for testing


#Define progress bar
pb <- progress_bar$new(total = nrow(utility_count))

results <- 
  map(utility_count$utility,
      function(x){
        
        pb$tick() #for progress bar
        
        #subset the data to estimate BSTS
        utility_ts <- ww_summ_pop %>%
          filter(utility==x) %>%
          select(measure_date,
                 sars
                 ) %>%
          mutate(measure_date=as_date(measure_date),
                 sars=log10(sars),
                 sars=na_if(sars,-Inf),
                 sars=na_if(sars,Inf)) %>%
          arrange(measure_date) %>%
          drop_na()
        
        #Define max and min dates of time series
        max_date=max(utility_ts$measure_date)
        min_date=min(utility_ts$measure_date)
        
        if(nrow(utility_ts)>5){
          temp_out <- tryCatch(
            {
              ss <- AddLocalLinearTrend(list(), y=utility_ts$sars)  #I have found this best to strike balance between signal and noise
              
              ## Add module for regressors
              
              fit <- bsts(
                utility_ts$sars,
                state.specification = ss,
                family = "student",
                niter = 2000,
                ping=0
              )
              
              #Forecast 20 days with model
              pred_ww <- predict(fit, horizon = forecast_horizon,burn = burnin,quantiles = c(.05,.95))
              
              
              #Build dataframe from model fit results
              plot_trend <- utility_ts %>%
                mutate(trend = colMeans(fit$state.contributions[-(1:burnin),"trend",])) %>%
                bind_rows(
                  tibble(measure_date = as_date(max_date + c(1:length(pred_ww$mean))),
                         trend=pred_ww$mean,
                         sars=NA)
                ) %>%
                mutate(utility=x)
              
              return(plot_trend)},
            error = function(e){
              return(NULL)
            })
          
          return(temp_out)
        } else {
          return(NULL)
        }
        
        
      })

#Remove nulls and bind to dataframe
final <- results %>%
  compact() %>%
  bind_rows()

# Cache data with fit and forecast
write_csv(final,str_c("output/trends/bsts_output/utility_bsts_fit_forecast",today(),"_21d.csv"))

##########################################
#plot_trend=results[[1]]
#Define progress bar
pb <- progress_bar$new(total = nrow(final))

#Iterate over fit and classify points
trend_out <- map_dfr(
  compact(results),  #drops the null values
   function(plot_trend){
     #Set date list to classify - run once with all, then 
     date_list=drop_na(plot_trend,sars) %>%
       distinct(measure_date) %>%
       pull() 
     
     #Construct empty list to populate
     trend_shell <- vector("list",length(date_list))
     
     for(i in 1:length(date_list)){
       pb$tick() #for progress bar
       
       #Set date to classify
       max_date=date_list[i]
       
       #Estimate linear model and classify
       trend_shell[[i]] <- plot_trend %>%
         filter(measure_date %within% (as_date(max_date-trend_window) %--% max_date)) %>%#keep only observations from past 3 weeks
         mutate(num_date=row_number()) %>% #convert date to integer - doesn't matter for single slope model but is convenient for polynomial
         lm(trend ~ num_date,data=.) %>% #fit linear model
         broom::tidy() %>% #results to dataframe
         filter(term=="num_date") %>%
         mutate(classification=case_when(
           estimate>0 & p.value<=p_val_threshold ~ "Increasing",
           estimate<0 & p.value<=p_val_threshold ~ "Decreasing",
           p.value>p_val_threshold ~ "Plateau",
         ),
         utility=na.exclude(unique(plot_trend$utility))[1],
         measure_date=max_date)
     }
     
     return(bind_rows(trend_shell))
              
})


#merge back with ww_summ_pop 
ww_trend_final <- ww_summ_pop %>%
  left_join(trend_out %>% select(utility,measure_date,classification,slope=estimate,p_val=p.value),by = c("measure_date", "utility"))

#cache output
write_csv(ww_trend_final,str_c("output/trends/bsts_output/utility_bsts_lm_",today(),"_21d.csv"))


