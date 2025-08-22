## --------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------
# 3 Introduction to the Tidyverse and Core Data Skills
## --------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------

library(tidyverse)



## --------------------------------------------------------------------------------------

summarise(group_by(txhousing, year), avg_sales = mean(sales, na.rm = TRUE))



## --------------------------------------------------------------------------------------

txhousing |>
  group_by(year) |>
  summarise(avg_sales = mean(sales, na.rm = TRUE))
  


## --------------------------------------------------------------------------------------

# read from a downloaded file in your working directory
gssdat <- read.csv("gssdat.csv")



## --------------------------------------------------------------------------------------
gssdat |>
  select(year, age, degree, income) |> 
  slice_sample(n = 10)


## --------------------------------------------------------------------------------------
gssdat |>
  select(year, age, degree, income) |>
  filter(year == 2022, degree %in% c("bachelor's", "graduate")) |> 
  slice_sample(n = 10)


## --------------------------------------------------------------------------------------

# length one vector
5 %in% c(1, 3, 5, 7, 9)

# length three vector
c(1, 3, 5) %in% c(1, 2, 4, 6, 3)

# toy dataset
toydat <- data.frame(letter = c("A", "B", "C", "D", "E", "F", "I", "A")) 

# length two vector against variable
c("A", "F") %in% toydat$letter

# variable against length two vector
toydat$letter %in% c("A", "F")



## --------------------------------------------------------------------------------------

gssdat |>
  mutate(young_adult = if_else(age >= 18 & age <= 30, "Yes", "No")) |>
  select(year, young_adult, degree, income) |> 
  slice_sample(n = 10)
  


## --------------------------------------------------------------------------------------
gssdat |>
  group_by(degree) |>
  summarise(avg_age = mean(age, na.rm = TRUE))


## --------------------------------------------------------------------------------------
gssdat |>
  group_by(degree) |>
  summarise(avg_income = mean(income, na.rm = TRUE)) |>
  arrange(avg_income)


## --------------------------------------------------------------------------------------

unique(gssdat$incomecat)



## --------------------------------------------------------------------------------------

gssdat |>
  group_by(degree) |>
  summarise(avg_income = mean(income, na.rm = TRUE)) |>
  arrange(desc(avg_income))



## --------------------------------------------------------------------------------------

gssdat |>
  filter(str_detect(occ10, regex("teacher|nurse|emergency"))) |>
  select(age, incomecat, occ10) |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

gssdat |>
  filter(str_detect(occ10, "teacher")) |>
  select(age, incomecat, occ10) |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

gssdat |>
  filter(str_detect(occ10, "miscellaneous")) |>
  mutate(occ10 = str_replace(occ10, "miscellaneous", "misc")) |>
  select(occ10)  |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------
# 4 Importing and Cleaning Data
## --------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------


# Number of rows and columns
dim(gssdat)

# First ten variable names
names(gssdat)

# Summary of one variable
summary(gssdat$age)



## --------------------------------------------------------------------------------------

# Count number of missing values in each column
gssdat |> 
  is.na() |> 
  colSums()

# How many rows do not have a missing income bracket
gssdat |> 
  filter(!is.na(income)) |> 
  nrow()



## --------------------------------------------------------------------------------------

# Example with two trust variables
gssdat |>
  select(trustfam, trustdoc) |> 
  pivot_longer(cols = c(trustfam, trustdoc),
               names_to = "trust_type",
               values_to = "trust_level") |> 
  group_by(trust_type, trust_level) |> 
  summarize(count = n())



## --------------------------------------------------------------------------------------

# widen workstatus by year
gssdat |>
  select(id, year, wrkstat) |>  
  pivot_wider(names_from = year,
              values_from = wrkstat) |> 
  select('2021', '2022', '2024') |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

# Combine 'separated' and 'divorced' into 'not married'
gssdat |>
  mutate(marital = recode(marital,
                          "separated" = "not married",
                          "divorced" = "not married")) |> 
  count(marital)

# compare
gssdat |> 
  count(marital)



## --------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------
# 5 Visualizing and Summarizing Data
## --------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = agewed)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Age at First Marriage",
       x = "Age at First Marriage",
       y = "Count")



## --------------------------------------------------------------------------------------

agewed_summary <- gssdat |>
  summarize(mean_age = mean(agewed, na.rm = TRUE),
            med_age = median(agewed, na.rm = TRUE))

agewed_summary



## --------------------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = agewed)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  geom_vline(data = agewed_summary, aes(xintercept = mean_age), color = "red", linewidth = 1) +
  geom_vline(data = agewed_summary, aes(xintercept = med_age), color = "forestgreen", linewidth = 1) +
  labs(title = "Distribution of Age at First Marriage",
       x = "Age at First Marriage",
       y = "Count")



## --------------------------------------------------------------------------------------

gssdat |>
  group_by(year) |>
  mutate(conjudge = recode(conjudge, `1` = 3L, `3` = 1L, `2` = 2L)) |>
  summarize(mean_conf = mean(conjudge, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = mean_conf)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Confidence in the Supreme Court Over Time",
       x = "Year",
       y = "Mean Confidence")



## --------------------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = age, y = realrinc)) +
  geom_point(alpha = 0.3) +
  labs(title = "Scatterplot of Age and Income",
       x = "Age",
       y = "Income (1986 US dollars)")



## --------------------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = marital, y = hrs1)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Hours Worked per Week by Marital Status",
       x = "Marital Status",
       y = "Hours Worked per Week")



## --------------------------------------------------------------------------------------

gssdat |> 
  group_by(marital) |> 
  summarize(avg_hrs = mean(hrs1, na.rm = TRUE)) |> 
  arrange(avg_hrs)



## --------------------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = conpress)) +
  geom_bar(fill = "forestgreen") +
  labs(title = "Confidence in the Press",
       x = "Response Category",
       y = "Count")



## --------------------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = race, fill = conpress)) +
  geom_bar(position = "fill") +
  labs(title = "Confidence in the Press by Race",
       x = "Race",
       y = "Count")



## --------------------------------------------------------------------------------------

gssdat |>
  select(conpress, consci, race) |>
  pivot_longer(cols = c(conpress, consci),
               names_to = "confidence_type",
               values_to = "confidence_level") |> 
  ggplot(aes(x = race, fill = confidence_level)) +
  geom_bar(position = "fill") +
  facet_wrap(~confidence_type) +
  labs(title = "Confidence in Institutions by Race",
       x = "Race",
       y = "Count")



## --------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------
# 7 Practice Solutions
## --------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------

?mean


## --------------------------------------------------------------------------------------

plot(1:5)


## --------------------------------------------------------------------------------------

gssdat |>
  select(fepol, year) |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

gssdat |>
  filter(year == 2018, happy == "very happy") |>
  select(year, happy) |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

gssdat |>
  mutate(above_med_inc = income >= median(income, na.rm = TRUE)) |>
  select(above_med_inc, income) |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

gssdat |>
  group_by(marital) |>
  summarize(mean_child = mean(childs, na.rm = TRUE))



## --------------------------------------------------------------------------------------

gssdat |>
  group_by(marital) |>
  summarize(mean_age = mean(age, na.rm = TRUE)) |>
  arrange(desc(mean_age))



## --------------------------------------------------------------------------------------

gssdat |>
  filter(str_detect(occ10, "nurse")) |>
  select(occ10, income, age) |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

gssdat |> 
  mutate(occ10 = str_replace(occ10, "telephone", "phone")) |> 
  select(occ10, income) |> 
  filter(str_detect(occ10, "phone")) |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

gssdat |> 
  filter((age <= 40 | age >= 25) & str_detect(occ10, regex("manager|teacher|engineer"))) |> 
  select(year, occ10) |> 
  group_by(year) |> 
  summarize(count = n())



## --------------------------------------------------------------------------------------

dim(gssdat)

gssdat |> 
  select(degree) |> 
  glimpse()



## --------------------------------------------------------------------------------------

gssdat |> 
  filter(!is.na(income)) |> 
  select(income, age) |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

gssdat |> 
  select(happy, health) |> 
  pivot_longer(cols = c(happy, health), 
               names_to = "response_type", 
               values_to = "response") |> 
  slice_sample(n = 10)



## --------------------------------------------------------------------------------------

gssdat |>
  mutate(happy = recode(happy,
                          "not too happy" = "unhappy")) |> 
  count(happy)



## --------------------------------------------------------------------------------------

gssdat |> 
  filter((year <= 2020 | year >= 2010) & !is.na(age)) |> 
  group_by(marital) |> 
  summarize(mean_age = mean(age))



## --------------------------------------------------------------------------------------

gssdat |> 
  ggplot(aes(x = realinc)) +
  geom_histogram(fill = "darkorange", color = "black", binwidth = 10000) +
  labs(x = "Family income in 1986 US dollars", y = "Count", title = "Income Distribution")



## --------------------------------------------------------------------------------------

gssdat %>%
  mutate(coneduc = recode(coneduc, `1` = 3L, `3` = 1L, `2` = 2L)) |> 
  group_by(year) %>%
  summarise(mean_coneduc = mean(coneduc, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_coneduc)) +
  geom_line(color = "purple") +
  geom_point(color = "purple") +
  labs(title = "Average Confidence in the Education System Over Time",
       x = "Survey Year",
       y = "Mean Confidence in Education")



## --------------------------------------------------------------------------------------

gssdat %>%
  ggplot(aes(x = race, fill = conlegis)) +
  geom_bar(position = "fill") +
  facet_wrap(~sex) +
  labs(title = "Confidence in Congress by Gender", x = "Confidence in Congress", y = "Count")



## --------------------------------------------------------------------------------------

gssdat %>%
  ggplot(aes(x = race, y = educ, fill = race)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Education by Race",
    x = "Race",
    y = "Years of Education"
  )



