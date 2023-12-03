Use the flights data from the nycflights13 package. Use stratafied bootstrapping by dests to estimate the average air_time for flights within each origin and produce a table including the estimates and confidence intervals for each origin.

## Without any parallel processing

library(nycflights13)
library(dplyr)
library(tidyverse)
data(flights)

flight <- flights[c("dest", "origin", "air_time")]
# function for stratified sampling
stratified_sampling <- function(df) {
  sampled_df <- df %>%
    group_by(dest) %>%
    sample_n(size = n(), replace = TRUE) 
  return(sampled_df)
}
# function for stratified sampling n times
sampled_flight <- function(n) {
  sample <- vector("list", n)
  for (i in 1:n) {
    sample[[i]] <- stratified_sampling(flight)
  }
  return(sample)
}

# measure the time taken
time_without_parallel <- system.time({
  # generate samples 1000 times
  bootstrap_sample1 <- sampled_flight(1000)
  # calculate the mean air time
  bootstrap_means <- lapply(bootstrap_sample1, function(sample) {
    sample %>%
      drop_na() %>%
      group_by(origin) %>%
      summarise(mean_time = mean(air_time, na.rm = TRUE)) %>%
      ungroup() %>%
      as.data.frame() 
  })
  # combine the results into a data frame
  bootstrap_means_df <- do.call(rbind, lapply(seq_along(bootstrap_means), function(i) {
    cbind(sample_id = i, bootstrap_means[[i]])
  }))
  # calculate the mean and standard deviation
  bootstrap_means_sd <- bootstrap_means_df %>%
    group_by(origin) %>%
    summarise(mean = mean(mean_time), sd = sd(mean_time)) %>%
    ungroup()
  # calculate 95% confidence intervals
  bootstrap_means_ci <- bootstrap_means_sd %>%
    mutate(lower_ci = mean - 1.96 * sd, upper_ci = mean + 1.96 * sd)
})
bootstrap_means_ci
time_without_parallel


## With some form of parallel processing

library(parallel)
# function for stratified sampling
stratified_sampling_parallel <- function(df) {
  df %>%
    group_by(dest) %>%
    sample_n(size = n(), replace = TRUE)
}

# function for stratified sampling n times in parallel
sampled_flight_parallel <- function(n, df) {
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, library(dplyr))
  clusterExport(cl, varlist = c("stratified_sampling_parallel", "df"), envir = environment())
  sample <- parLapply(cl, 1:n, function(x) stratified_sampling_parallel(df))
  stopCluster(cl)
  return(sample)
}

# measure the time taken
time_with_paralle <- system.time({
  # generate samples 1000 times
  sample1_parallel <- sampled_flight_parallel(1000, flight)
  
  # calculate the mean air time 
  bootstrap_means_parallel <- lapply(sample1_parallel, function(sample) {
    sample %>%
      drop_na() %>%
      group_by(origin) %>%
      summarise(mean_time = mean(air_time, na.rm = TRUE)) %>%
      ungroup() %>%
      as.data.frame()
  })
  
  # combine the results into a data frame
  bootstrap_means_df_parallel <- do.call(rbind, lapply(seq_along(bootstrap_means_parallel), function(i) {
    cbind(sample_id = i, bootstrap_means_parallel[[i]])
  }))
  
  # calculate the mean and standard deviation
  bootstrap_means_ci_parallel <- bootstrap_means_df_parallel %>%
    group_by(origin) %>%
    summarise(mean = mean(mean_time), sd = sd(mean_time)) %>%
    ungroup()
  
  # calculate 95% confidence intervals
  bootstrap_means_ci_parallel <-  bootstrap_means_ci_parallel %>%
    mutate(lower_ci = mean - 1.96 * sd / sqrt(length(sample1_parallel)),
           upper_ci = mean + 1.96 * sd / sqrt(length(sample1_parallel)))
})

time_with_paralle

bootstrap_means_ci_parallel