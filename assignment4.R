## Problem 1 - Tidyverse

# a
```{r}
library(nycflights13)
library(tidyverse)
data(package = "nycflights13")
data(flights,package = "nycflights13")
data(airports,package = "nycflights13")
airports_faa <- airports %>%
  select(faa, name)
departure_delay <- flights %>%
  select(dep_delay,origin)
departure_delay <- departure_delay %>%
  left_join(airports_faa, by = c("origin" = "faa"))
departure_delay <- departure_delay %>% drop_na()
departure_delay <- departure_delay %>%
  group_by(name) %>%
  summarize(mean_dep_delay = mean(dep_delay), 
            median_dep_delay = median(dep_delay)) %>%
  arrange(-mean_dep_delay) %>%
  ungroup()
print(departure_delay)
```

```{r}
arrival_delay <- flights %>%
  select(arr_delay,dest)
arrival_delay <- arrival_delay %>%
  left_join(airports_faa, by = c("dest" = "faa"))
arrival_delay <- arrival_delay %>% drop_na()
arrival_delay <- arrival_delay %>%
  group_by(name) %>%
  filter(n() >= 10) %>%
  summarize(mean_arr_delay = mean(arr_delay), 
            median_arr_delay = median(arr_delay)) %>%
  arrange(-mean_arr_delay) %>%
  ungroup()
print(arrival_delay, n = 200)
```

# b
```{r}
model_speed <- flights %>%
  select(tailnum, distance, air_time)
aircraft_model <- planes %>%
  select(tailnum, model)
model_speed <- model_speed %>%
  left_join(aircraft_model, by = "tailnum")
model_speed <- model_speed %>% drop_na()
model_speed <- model_speed %>%
  group_by(model) %>%
  summarize(total_distance = sum(distance),
            total_time = sum(air_time)/60,
            average_speed = total_distance/total_time,
            number_of_flight = n()) %>%
  select(-c(total_distance, total_time)) %>%
  filter(average_speed == max(average_speed))
model_speed
```
## Problem 2 - get_temp()
```{r}
setwd("/Users/dalaohuhan/Downloads")
nnmaps <- read.csv("chicago-nmmaps.csv")
library(tidyverse)
get_temp <- function(month, year, data, celsius = FALSE, average_fn = mean) {
  #check if the month is a character 
  if(is.character(month)) {
    #convert the full name or abbreviation of the month into a number
    if(tolower(month) %in% tolower(month.name)) {
      month <- match(tolower(month), tolower(month.name))
    } else if(tolower(month) %in% tolower(month.abb)) {
      month <- match(tolower(month), tolower(month.abb))
    } else {
      return("Please input a valid month like 'Apr', 'September' or a number from 1 to 12")
    }
  }
  #check if the month is between 1 and 12
  if(!(month %in% c(1:12))) {
    return("Please input a valid month like 'Apr', 'September' or a number from 1 to 12")
  }
  #check if the year is between 1997 and 2000
  if(!(year %in% c(1997:2000))) {
    return("Please input a valid year from 1997 to 2000")
  }
  #Select Temp, Year, Month, and Day from nnmaps
  filtered_data <- nnmaps %>% 
    select(date, temp) %>%
    mutate(Temp = temp,
           Year = year(date),
           Month = month(date),
           Day = day(date)) %>%
    select(-c(date,temp))
  #calculate the average_fn for the specified date
  temp_mean <- filtered_data %>%
    filter(Year == year, Month == month) %>%
    summarize(temp_mean = average_fn(Temp)) %>%
    pull(temp_mean)
  #convert Fahrenheit to Celsius
  if (celsius) {
    temp_mean <- (temp_mean - 32) * 5 / 9
  }
  return(temp_mean)
  
}
get_temp("Apr", 1999, data = nnmaps)
get_temp("Apr", 1999, data = nnmaps, celsius = TRUE)
get_temp(10, 1998, data = nnmaps, average_fn = median)
get_temp(13, 1998, data = nnmaps)
get_temp(2, 2005, data = nnmaps)
get_temp("November", 1999, data =nnmaps, celsius = TRUE,
         average_fn = function(x) {
           x %>% sort -> x
           x[2:(length(x) - 1)] %>% mean %>% return
         })

```

## Problem 3 - SAS

# a
```{sas, eval = FALSE}
%let in_path = ~/assignment/input_data;
%let out_path = ~/assignment/output_data; 
libname in_lib "&in_path."; 
libname out_lib "&out_path.";

data recs; 
set in_lib.recs2020_public_v5; 

/*select variables*/ 
  proc sql;
create table recs2020 as
select
DOEID,
state_name,
nweight,
DOLLAREL,
TOTROOMS,
PRKGPLC1
from recs;
quit;

/*count the frequency*/
  proc freq 
data = recs2020 order = freq;
weight nweight;
tables state_name / out = freq_recs2020;
run;

/*print the state with the highest percentage of records*/
  proc print
data = freq_recs2020(obs = 1);
run;

/*print percentage of all records correspond to Michigan*/
  proc print
data = freq_recs2020;
where state_name = 'Michigan';
run;
```
# b
```{sas,eval = FALSE}
/*filter the data with strictly positive cost*/
  data recs2020_positive;
set recs2020;
if DOLLAREL > 0;
Run;


/*generate a histogram*/
  proc sgplot
data = recs2020_positive;
histogram DOLLAREL;
run;
```
# c
```{sas, eval = FALSE}
/*caculate log of the total electricity cost*/
  data recs2020_log_DOLLAREL;
set recs2020_positive;
log_DOLLAREL = log(DOLLAREL);
run;

/*generate a log histogram*/
  proc sgplot
data = recs2020_log_DOLLAREL;
histogram log_DOLLAREL;
run;
```
# d
```{sas, eval = FALSE}
/*Fit a linear regression model */
  proc surveyreg
data = recs2020_log_DOLLAREL;
model log_DOLLAREL = TOTROOMS PRKGPLC1;
weight nweight;
output out = recs_reg_results p = predicted_log_DOLLAREL;
run;

```
# e
```{sas, eval = FALSE}
/*generate predicted values*/
  data recs_reg_results;
set recs_reg_results;
predicted_DOLLAREL = exp(predicted_log_DOLLAREL);
actual_DOLLAREL = DOLLAREL;
run;

/*create a scatterplot */
  proc sgplot 
data=recs_reg_results;
scatter x=predicted_DOLLAREL y=actual_DOLLAREL;
run;

```
## Problem 4 - Multiple tools
# b
``` {sas, eval = FALSE}
%let in_path = ~/assignment/input_data;
%let out_path = ~/assignment/output_data; 
libname in_lib "&in_path."; 
libname out_lib "&out_path.";

data public; 
set in_lib.public2022; 

/*select and rename variables */
  proc sql;
create table selected_public as
select
CaseID,
B3 as family_finance_change,
ND2 as chance_of_disaster,
B7_b as economic_conditions,
GH1 as house_own,
educ_4cat as education,
race_5cat as race,
weight_pop
from public;
quit;
```
# c
``` {sas, eval = FALSE}
data out_lib.selected_public;
set selected_public;
run;

/*export data as .csv*/
  proc export data = out_lib.selected_public
outfile = '~/all_selected_public.csv' 
dbms = csv replace;
run;
```
# d
``` {stata, eval = FALSE}

import delimited "/Users/dalaohuhan/Downloads/all_selected_public.csv"

describe
```
# e
``` {stata, eval = FALSE}
generate worse_off_binary = (family_finance_change == 1) | (family_finance_change == 2)
```
# f
``` {stata, eval = FALSE}
svyset caseid [pw=weight_pop]

svy: logistic worse_off_binary chance_of_disaster i.economic_conditions i.house_own i.education i.race
# g
``` {stata, eval = FALSE}
export delimited using "/Users/dalaohuhan/Downloads/selected_public_from_stata.csv", replace
```

```{r}
setwd("/Users/dalaohuhan/Downloads")
public <- read.csv("selected_public_from_stata.csv")
```
# h
```{r}
library(survey)
survey_design <- svydesign(id = ~ caseid, weight = ~ weight_pop, data = public)

logit_model <- svyglm(worse_off_binary ~ chance_of_disaster + as.factor(economic_conditions) + 
                        as.factor(house_own) + as.factor(education) + as.factor(race), design = survey_design, family = quasibinomial)

pseudo_r_sq <- 1 - (logit_model$deviance / logit_model$null.deviance)
pseudo_r_sq
```
