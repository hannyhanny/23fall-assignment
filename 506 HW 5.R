## Problem 1

# a

library(dplyr)
library(tidyr)
library(ggplot2)
nmmaps <- read.csv("chicago-nmmaps 2.csv")
nmmaps_mean_temp <- nmmaps %>%
  select(temp, season, month_numeric) %>%
  mutate(temp_celsius = (temp - 32) * 5 / 9) %>%
  group_by(month_numeric,season) %>%
  summarize(mean_temp = mean(temp_celsius), na.rm = TRUE) %>%
  ungroup()

# order the months for plotting
nmmaps_mean_temp$month <- factor(month.abb[nmmaps_mean_temp$month_numeric], levels = month.abb)

# Plotting
ggplot(nmmaps_mean_temp, aes(x = month, y = mean_temp, color = season)) +
  geom_point() +
  geom_line(aes(group = 1)) + # Add group = 1 to ensure one continuous line
  labs(title = 'Mean Monthly Temperature in Celsius',
       x = 'Month',
       y = 'Average Monthly Temperature in Celsius') +
  theme_minimal()


# b

nmmaps_mean <- nmmaps %>%
  select(temp, o3, pm10, dewpoint, season, month_numeric) %>%
  mutate(temp_celsius = (temp - 32) * 5 / 9) %>%
  group_by(month_numeric,season) %>%
  summarize(Temp = mean(temp_celsius, na.rm = TRUE),
            O3 = mean(o3, na.rm = TRUE),
            PM10 = mean(pm10, na.rm = TRUE),
            Dewpoint = mean(dewpoint), na.rm = TRUE) %>%
  select(-na.rm) %>%
  ungroup()

# order the months for plotting
nmmaps_mean$month <- factor(month.abb[nmmaps_mean$month_numeric], levels = month.abb)

# reshape for plotting
nmmaps_mean <- nmmaps_mean %>%
  pivot_longer(cols = c("Temp", "O3", "PM10", "Dewpoint"),
               names_to = "Variable",
               values_to = "Value")

# Plotting
ggplot(nmmaps_mean, aes(x = month, y = Value, group = Variable)) +
  geom_line(aes(linetype = Variable)) +
  geom_point(aes(color = season)) +
  scale_linetype_manual(values=c("Temp" = "solid", 
                                 "O3" = "twodash", 
                                 "PM10" = "dotted", 
                                 "Dewpoint" = "dotdash")) +
  theme_minimal() +
  labs(title = "Mean Monthly Values for Temperature, O3, PM10, and Dewpoint",
       x = "Month", 
       y = "Average Value")

#The plot indicates that PM10 fluctuates little with the seasons. Therefore, PM10 has the least seasonal trend.

## Problem 2

# a

setClass(
  "PolyExpr", 
  slots = c(
    coefVec = "numeric", 
    expVec = "numeric"
  )
)

setMethod(
  "initialize",
  "PolyExpr",
  function(.Object, expr = NA, coefVec = numeric(0), expVec = numeric(0)) {
    if (!is.na(expr) && is.character(expr)) {
      termList <- unlist(strsplit(expr, split = "\\s*(?=[+-])|(?<=[+-])\\s*", perl = TRUE))
      coefVec <- numeric()
      expVec <- numeric()
      multiplier <- 1
      
      
      for (term in termList) {
        if (term == "+") {
          multiplier <- 1
        } else if (term == "-") {
          multiplier <- -1
        } else {
          term <- gsub("\\s+", "", term)
          if (grepl("^-?x$", term)) {
            coef <- multiplier
            exp <- 1
          } else if (grepl("^-?[0-9]+$", term)) {
            coef <- as.numeric(term)
            exp <- 0
          } else {
            parts <- strsplit(term, split = "x\\^")[[1]]
            coef <- multiplier * as.numeric(parts[1])
            exp <- as.numeric(parts[2])
          }
          coefVec <- c(coefVec, coef)
          expVec <- c(expVec, exp)
        }
      }
    }
    
    .Object@coefVec <- coefVec
    .Object@expVec <- expVec
    
    validObject(.Object)
    return(.Object)
  }
)

make_poly <- function(x, y) {
  new("PolyExpr", coefVec = x, expVec = y)
}

setMethod("show", "PolyExpr", function(object) {
  terms <- character()
  
  for (i in seq_along(object@coefVec)) {
    coef <- object@coefVec[i]
    exp <- object@expVec[i]
    
    if (coef == 0) next
    
    term <- ifelse(exp == 0, 
                   as.character(coef), 
                   ifelse(exp == 1, 
                          paste0(coef, "x"), 
                          paste0(coef, "x^", exp)))
    
    if (i != 1 && coef > 0) {
      term <- paste0("+", term)
    }
    
    terms <- c(terms, term)
  }
  
  polyString <- paste(terms, collapse = " ")
  cat(polyString, "\n")
})

# Addition 
setMethod("+", signature(e1 = "PolyExpr", e2 = "PolyExpr"), function(e1, e2) {
  totalTerms <- rbind(
    cbind(e1@coefVec, e1@expVec),
    cbind(e2@coefVec, e2@expVec)
  )
  totalTerms <- totalTerms[order(totalTerms[, 2], decreasing = TRUE), ]
  combinedTerms <- aggregate(totalTerms[, 1], list(totalTerms[, 2]), sum)
  new("PolyExpr", coefVec = combinedTerms$x, expVec = combinedTerms$Group.1)
})

# Subtraction 
setMethod("-", signature(e1 = "PolyExpr", e2 = "PolyExpr"), function(e1, e2) {
  subtractedTerms <- rbind(
    cbind(e1@coefVec, e1@expVec),
    cbind(-e2@coefVec, e2@expVec)
  )
  subtractedTerms <- subtractedTerms[order(subtractedTerms[, 2], decreasing = TRUE), ]
  combinedTerms <- aggregate(subtractedTerms[, 1], list(subtractedTerms[, 2]), sum)
  new("PolyExpr", coefVec = combinedTerms$x, expVec = combinedTerms$Group.1)
})


# b

p1 <- make_poly(c(3,2), c(2,0))
p2 <- make_poly(c(7,-2,-1,17), c(3,2,1,0))
p1
p2
p1 + p2
p1 - p2

## Problem 3

# a

library(nycflights13)
library(data.table)
data(flights, package = "nycflights13")
data(airports, package = "nycflights13")
flights <- as.data.table(flights)
airports <- as.data.table(airports)
# find the mean and median departure delay per airport 
# and exclude any destination with under 10 flights
departure_delay <- flights[, .(mean_delay = mean(dep_delay, na.rm = TRUE),
                               median_delay = median(dep_delay, na.rm = TRUE),
                               .N), by = origin][N > 10]
departure_delay <- airports[departure_delay, on = .(faa = origin),
                            .(name, mean_delay, median_delay)]
departure_delay <- departure_delay[order(-mean_delay)]
print(departure_delay, n = Inf) 

# find the mean and median arrival delay per airport
arrival_delay <- flights[, .(mean_delay = mean(arr_delay, na.rm = TRUE),
                             median_delay = median(arr_delay, na.rm = TRUE),
                             .N), by = dest][N > 10]
arrival_delay <- airports[arrival_delay, on = .(faa = dest), 
                          .(name, mean_delay, median_delay)]
arrival_delay <- arrival_delay[order(-mean_delay)]
print(arrival_delay, n = Inf)

# b

planes <- as.data.table(planes)
flights_planes<- flights[planes, on = .(tailnum)]
flights_planes[, speed := distance / (air_time / 60)]
flights_planes <- flights_planes[, .(average_speed = mean(speed, na.rm = TRUE), number_of_flights = .N), by = model]
fastest_model <- flights_planes[order(-average_speed)][1]
fastest_model