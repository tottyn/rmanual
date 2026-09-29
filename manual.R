## ----setup, include = FALSE---------------------------------------------------

knitr::opts_chunk$set(echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE)
knitr::knit_hooks$set(purl = knitr::hook_purl)


## -----------------------------------------------------------------------------

library(tidyverse)


## -----------------------------------------------------------------------------

summarise(group_by(txhousing, year), avg_sales = mean(sales, na.rm = TRUE))


## -----------------------------------------------------------------------------

txhousing |>
  group_by(year) |>
  summarise(avg_sales = mean(sales, na.rm = TRUE))
  

## ----eval = FALSE-------------------------------------------------------------
#  
#  # read from a downloaded file in your working directory
#  gssdat <- read.csv("gssdat.csv")
#  

## ----echo = FALSE-------------------------------------------------------------

# read from a downloaded file in your working directory
gssdat <- read.csv("files/gssdat.csv")


## -----------------------------------------------------------------------------
gssdat |>
  select(year, age, degree, income) |> 
  slice_sample(n = 10)

## -----------------------------------------------------------------------------
gssdat |>
  select(year, age, degree, income) |>
  filter(year == 2022, degree %in% c("bachelor's", "graduate")) |> 
  slice_sample(n = 10)

## -----------------------------------------------------------------------------

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


## -----------------------------------------------------------------------------

gssdat |>
  mutate(young_adult = if_else(age >= 18 & age <= 30, "Yes", "No")) |>
  select(year, young_adult, degree, income) |> 
  slice_sample(n = 10)
  

## -----------------------------------------------------------------------------
gssdat |>
  group_by(degree) |>
  summarise(avg_age = mean(age, na.rm = TRUE))

## -----------------------------------------------------------------------------
gssdat |>
  group_by(degree) |>
  summarise(avg_income = mean(income, na.rm = TRUE)) |>
  arrange(avg_income)

## -----------------------------------------------------------------------------

gssdat |>
  count(incomecat)


## -----------------------------------------------------------------------------

gssdat |>
  group_by(degree) |>
  summarise(avg_age = mean(age, na.rm = TRUE)) |>
  arrange(avg_age)


## -----------------------------------------------------------------------------

gssdat |>
  group_by(degree) |>
  summarise(avg_age = mean(age, na.rm = TRUE)) |>
  arrange(desc(avg_age))


## -----------------------------------------------------------------------------

gssdat |>
  filter(str_detect(occ10, regex("teacher|nurse|emergency"))) |>
  select(age, incomecat, occ10) |> 
  slice_sample(n = 10)


## -----------------------------------------------------------------------------

gssdat |>
  filter(str_detect(occ10, "teacher")) |>
  select(age, incomecat, occ10) |> 
  slice_sample(n = 10)


## -----------------------------------------------------------------------------

gssdat |>
  filter(str_detect(occ10, "miscellaneous")) |>
  mutate(occ10 = str_replace(occ10, "miscellaneous", "misc")) |>
  select(occ10)  |> 
  slice_sample(n = 10)


## ----eval = FALSE-------------------------------------------------------------
#  
#  # load package
#  library(tidyverse)
#  
#  # Load the GSS dataset from your working directory
#  gssdat <- read.csv("gssdat.csv")
#  

## -----------------------------------------------------------------------------

# Number of rows and columns
dim(gssdat)

# First ten variable names
names(gssdat)

# Summary of one variable
summary(gssdat$age)


## -----------------------------------------------------------------------------

# Count number of missing values in each column
gssdat |> 
  is.na() |> 
  colSums()

# How many rows do not have a missing income bracket
gssdat |> 
  filter(!is.na(income)) |> 
  nrow()


## -----------------------------------------------------------------------------

# Example with two trust variables
gssdat |>
  select(trustfam, trustdoc) |> 
  pivot_longer(cols = c(trustfam, trustdoc),
               names_to = "trust_type",
               values_to = "trust_level") |> 
  group_by(trust_type, trust_level) |> 
  summarize(count = n())


## -----------------------------------------------------------------------------

# widen workstatus by year
gssdat |>
  select(id, year, wrkstat) |>  
  pivot_wider(names_from = year,
              values_from = wrkstat) |> 
  select(`2021`, `2022`, `2024`) |> 
  slice_sample(n = 10)


## -----------------------------------------------------------------------------

# Combine 'separated' and 'divorced' into 'not married'
gssdat |>
  mutate(marital = recode(marital,
                          "separated" = "not married",
                          "divorced" = "not married")) |> 
  count(marital)

# compare
gssdat |> 
  count(marital)


## ----eval = FALSE-------------------------------------------------------------
#  
#  # load the package
#  library(tidyverse)
#  
#  # load the data
#  gssdat <- read.csv("gssdat.csv")
#  

## -----------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = agewed)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Age at First Marriage",
       x = "Age at First Marriage",
       y = "Count")


## -----------------------------------------------------------------------------

agewed_summary <- gssdat |>
  summarize(mean_age = mean(agewed, na.rm = TRUE),
            med_age = median(agewed, na.rm = TRUE))

agewed_summary


## -----------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = agewed)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  geom_vline(data = agewed_summary, aes(xintercept = mean_age), color = "red", linewidth = 1) +
  geom_vline(data = agewed_summary, aes(xintercept = med_age), color = "forestgreen", linewidth = 1) +
  labs(title = "Distribution of Age at First Marriage",
       x = "Age at First Marriage",
       y = "Count")


## -----------------------------------------------------------------------------

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


## -----------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = age, y = realrinc)) +
  geom_point(alpha = 0.3) +
  labs(title = "Scatterplot of Age and Income",
       x = "Age",
       y = "Income (1986 US dollars)")


## -----------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = marital, y = hrs1)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Hours Worked per Week by Marital Status",
       x = "Marital Status",
       y = "Hours Worked per Week")


## -----------------------------------------------------------------------------

gssdat |> 
  group_by(marital) |> 
  summarize(avg_hrs = mean(hrs1, na.rm = TRUE)) |> 
  arrange(avg_hrs)


## -----------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = conpress)) +
  geom_bar(fill = "forestgreen") +
  labs(title = "Confidence in the Press",
       x = "Response Category",
       y = "Count")


## -----------------------------------------------------------------------------

gssdat |>
  ggplot(aes(x = race, fill = conpress)) +
  geom_bar(position = "fill") +
  labs(title = "Confidence in the Press by Race",
       x = "Race",
       y = "Count")


## -----------------------------------------------------------------------------

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


## -----------------------------------------------------------------------------

gssdat_sub <- gssdat |>
  filter(year == 2024)


## -----------------------------------------------------------------------------
gssdat_sub |>
  ggplot(aes(x = hrs1)) +
  geom_histogram(fill = "cornflowerblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 80, 10))

## ----eval = FALSE-------------------------------------------------------------
#  ?t.test

## -----------------------------------------------------------------------------

t.test(gssdat_sub$hrs1, alternative = "two.sided", mu = 40, conf.level = 0.95)


## -----------------------------------------------------------------------------

abs(mean(gssdat_sub$hrs1, na.rm = TRUE) - 40)


## -----------------------------------------------------------------------------
educ_pair <- gssdat_sub |>
  select(educ, paeduc) |>
  filter(!is.na(educ), !is.na(paeduc)) |>
  mutate(educ_diff = educ - paeduc)

## -----------------------------------------------------------------------------
educ_pair |>
  ggplot(aes(x = educ_diff)) +
  geom_histogram(binwidth = 1, fill = "cornflowerblue", color = "white") +
  labs(title = "Differences in Education Level",
       x = "Respondent - Father (years)",
       y = "Count")

## -----------------------------------------------------------------------------

educ_pair |>
  summarize(
    mean_educ = mean(educ),
    median_educ = median(educ),
    mean_paeduc = mean(paeduc),
    median_paeduc = median(paeduc),
    mean_difference = mean(educ_diff),
    median_difference = median(educ_diff),
    sd_difference = sd(educ_diff)
  )

## -----------------------------------------------------------------------------

educ_pair |>
  select(educ, paeduc) |>
  pivot_longer(cols = c(educ, paeduc),
               names_to = "response_type",
               values_to = "years") |>
  ggplot(aes(x = response_type, y = years)) +
  geom_boxplot(fill = "lightblue") +
  scale_x_discrete(labels = c("Respondent", "Father")) +
  labs(title = "Educational Attainment",
       x = "Response Type",
       y = "Years of Education")

## -----------------------------------------------------------------------------

t.test(educ_pair$educ,
       educ_pair$paeduc,
       paired = TRUE,
       alternative = "two.sided",
       mu = 0,
       conf.level = 0.95)

## -----------------------------------------------------------------------------

age_marital <- gssdat_sub |>
  filter(marital %in% c("married", "never married"),
         !is.na(age))

age_marital |>
  ggplot(aes(x = marital, y = age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Age by Marital Status",
       x = "Marital Status",
       y = "Age")

## -----------------------------------------------------------------------------

age_marital |>
  ggplot(aes(x = age, fill = marital)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity", color = "black") +
  labs(title = "Age Distributions by Marital Status",
       x = "Age",
       y = "Count",
       fill = "Marital Status")

## -----------------------------------------------------------------------------

age_marital |>
  group_by(marital) |>
  summarize(
    n = n(),
    mean_age = mean(age),
    median_age = median(age),
    sd_age = sd(age)
  )

## -----------------------------------------------------------------------------

t.test(age ~ marital,
       data = age_marital,
       alternative = "two.sided",
       var.equal = FALSE,
       conf.level = 0.95)

## -----------------------------------------------------------------------------

age_wed_sub <- gssdat |>
  filter(year == 2006, !is.na(agewed), !is.na(degree)) |>
  mutate(degree = factor(degree, levels = c("less than high school", "high school",
                                               "associate/junior college", "bachelor's",
                                               "graduate")))

age_wed_sub |>
  ggplot(aes(x = degree, y = agewed)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Age First Married by Highest Degree",
       x = "",
       y = "Age When First Married (years)") +
  coord_flip()

## -----------------------------------------------------------------------------

age_wed_sub |>
  group_by(degree) |>
  summarize(n = n(),
            mean_age = mean(agewed),
            median_age = median(agewed),
            sd_age = sd(agewed))

## -----------------------------------------------------------------------------

wed_anova <- aov(agewed ~ degree, data = age_wed_sub)

summary(wed_anova)


## -----------------------------------------------------------------------------
pairwise.t.test(x = age_wed_sub$agewed, g = age_wed_sub$degree,
                alternative = "two.sided", p.adjust.method = "bonferroni")

## -----------------------------------------------------------------------------
gssdat_sub |>
  filter(!is.na(fepol)) |>
  count(fepol) |>
  mutate(proportion = n / sum(n))

## -----------------------------------------------------------------------------
prop.test(x = 164, n = 164+728, p = 0.20, alternative = "less",
          correct = FALSE, conf.level = 0.95)

## -----------------------------------------------------------------------------
prop.test(x = 164, n = 164+728, p = 0.20, alternative = "two.sided",
          correct = FALSE, conf.level = 0.95)

## -----------------------------------------------------------------------------

gssdat_sub |>
  filter(!is.na(sex), !is.na(fepol)) |>
  group_by(sex) |>
  summarize(n = n(),
            count_agree = sum(fepol == "agree"),
            prop_agree = mean(fepol == "agree"))


## -----------------------------------------------------------------------------

gssdat_sub |>
  filter(!is.na(sex), !is.na(fepol)) |>
  ggplot(aes(x = sex, fill = fepol)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Most Men Are Better Suited Emotionally For Politics Than Most Women",
       x = "", y = "Proportion")

## -----------------------------------------------------------------------------

prop.test(x = c(84, 80),
          n = c(509, 383),
          alternative = "two.sided",
          correct = FALSE,
          conf.level = 0.95)

## -----------------------------------------------------------------------------

income_sub <- gssdat_sub |>
  filter(!is.na(realrinc))

income_sub |>
  ggplot(aes(x = realrinc)) +
  geom_histogram(binwidth = 5000,
                 fill = "steelblue",
                 color = "white") +
  labs(title = "Distribution of Respondent Income",
       x = "Income (1986 dollars)",
       y = "Count")

## -----------------------------------------------------------------------------

income_sub |>
  summarize(n = n(),
            mean_income = mean(realrinc),
            median_income = median(realrinc),
            sd_income = sd(realrinc),
            min_income = min(realrinc),
            max_income = max(realrinc))


## -----------------------------------------------------------------------------

income_sign <- income_sub |>
  mutate(comparison = case_when(
      realrinc > 20000 ~ "Above",
      realrinc < 20000 ~ "Below",
      realrinc == 20000 ~ "Equal"))

income_sign |>
  count(comparison)


## -----------------------------------------------------------------------------

income_sign_test <- income_sign |>
  filter(comparison != "Equal")

income_sign_test |>
  summarize(
    n = n(),
    above = sum(comparison == "Above"),
    below = sum(comparison == "Below"),
    proportion_above = mean(comparison == "Above"))


## -----------------------------------------------------------------------------

prop.test(x = 781, n = 1876, p = 0.50, alternative = "two.sided",
          correct = FALSE, conf.level = 0.95)


## -----------------------------------------------------------------------------

income_educ <- gssdat_sub |>
  select(educ, realrinc) |>
  filter(!is.na(educ), !is.na(realrinc))

income_educ |>
  ggplot(aes(x = educ, y = realrinc)) +
  geom_point(alpha = 0.2) +
  labs(title = "Educational Attainment and Respondent Income",
       x = "Years of Education",
       y = "Income (1986 dollars)")

## -----------------------------------------------------------------------------

cor(income_educ$realrinc, income_educ$educ)


## -----------------------------------------------------------------------------

income_model <- lm(realrinc ~ educ, data = income_educ)

summary(income_model)


## -----------------------------------------------------------------------------

income_educ |>
  ggplot(aes(x = educ, y = realrinc)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Educational Attainment and Respondent Income",
       x = "Years of Education",
       y = "Income (1986 dollars)")

## -----------------------------------------------------------------------------

income_educ |>
  mutate(predicted_income = predict(income_model),
         residual = realrinc - predicted_income) |>
  ggplot(aes(x = predicted_income, y = residual)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals from the Income-Education Model",
       x = "Predicted Income",
       y = "Residual")

## -----------------------------------------------------------------------------

income_multiple <- gssdat_sub |>
  select(realrinc, educ, age, hrs1) |>
  filter(!is.na(realrinc),
         !is.na(educ),
         !is.na(age),
         !is.na(hrs1))

## -----------------------------------------------------------------------------

pairs(income_multiple, lower.panel = NULL)


## -----------------------------------------------------------------------------

cor(income_multiple)


## -----------------------------------------------------------------------------

income_multiple_model <- lm(realrinc ~ educ + age + hrs1, data = income_multiple)

summary(income_multiple_model)


## -----------------------------------------------------------------------------

prediction_data <- data.frame(educ = 0:20,
                              age = mean(income_multiple$age),
                              hrs1 = mean(income_multiple$hrs1))

prediction_data$predicted_income <- predict(income_multiple_model,
                                            newdata = prediction_data)


## -----------------------------------------------------------------------------

ggplot(prediction_data, aes(x = educ, y = predicted_income)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Predicted Income by Educational Attainment",
       subtitle = "Age and weekly work hours held at their sample means",
       x = "Years of Education",
       y = "Predicted Income (1986 dollars)")

## -----------------------------------------------------------------------------

income_multiple |>
  mutate(predicted_income = predict(income_multiple_model),
         residual = realrinc - predicted_income) |>
  ggplot(aes(x = predicted_income, y = residual)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals from the Multiple Regression Model",
       x = "Predicted Income",
       y = "Residual")

## -----------------------------------------------------------------------------

income_multiple |>
  mutate(predicted_income = predict(income_multiple_model),
         residual = realrinc - predicted_income) |>
  ggplot(aes(x = residual)) +
  geom_histogram(fill = "cornflowerblue", color = "white") +
  labs(title = "Residuals from the Multiple Regression Model",
       x = "Residual")

## ----eval = F-----------------------------------------------------------------
#  gssdat |>
#    filter(year == 2022) |>
#    summarize(avg_age = mean(age, na.rm = TRUE))

## ----eval = F-----------------------------------------------------------------
#  gssdat |>
#    select(age, income) |>
#    filter(age > 30)

## ----eval = FALSE-------------------------------------------------------------
#  gssdat <- read.csv("files/gssdat.csv")
#  
#  dim(gssdat)
#  names(gssdat)
#  summary(gssdat$age)

## ----eval = FALSE-------------------------------------------------------------
#  import pandas as pd
#  
#  gssdat = pd.read_csv("files/gssdat.csv")
#  
#  gssdat.shape
#  gssdat.columns
#  gssdat["age"].describe()

## ----eval = FALSE-------------------------------------------------------------
#  gssdat |>
#    select(year, age, income) |>
#    filter(year == 2022)

## ----eval = FALSE-------------------------------------------------------------
#  gssdat[["year", "age", "income"]][gssdat["year"] == 2022]

## ----eval = FALSE-------------------------------------------------------------
#  gssdat |>
#    mutate(young_adult = age >= 18 & age <= 30)

## ----eval = FALSE-------------------------------------------------------------
#  gssdat["young_adult"] = (gssdat["age"] >= 18) & (gssdat["age"] <= 30)

## ----eval = FALSE-------------------------------------------------------------
#  gssdat |>
#    group_by(degree) |>
#    summarise(avg_age = mean(age, na.rm = TRUE))

## ----eval = FALSE-------------------------------------------------------------
#  gssdat.groupby("degree")["age"].mean()

## ----eval = FALSE-------------------------------------------------------------
#  gssdat |>
#    filter(year == 2022) |>
#    group_by(degree) |>
#    summarise(avg_age = mean(age, na.rm = TRUE))

## ----eval = FALSE-------------------------------------------------------------
#  gssdat[gssdat["year"] == 2022] \
#      .groupby("degree")["age"] \
#      .mean()

## ----eval = FALSE-------------------------------------------------------------
#  gssdat |>
#    ggplot(aes(x = age)) +
#    geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
#    labs(title = "Age Distribution",
#         x = "Age",
#         y = "Count")

## ----eval = FALSE-------------------------------------------------------------
#  import seaborn as sns
#  import matplotlib.pyplot as plt
#  
#  sns.histplot(data=gssdat, x="age", binwidth=5, color="steelblue", edgecolor="white")
#  
#  plt.title("Age Distribution")
#  plt.xlabel("Age")
#  plt.ylabel("Count")
#  plt.show()

## ----eval = FALSE-------------------------------------------------------------
#  gssdat |>
#    ggplot(aes(x = age, y = realrinc)) +
#    geom_point(alpha = 0.3) +
#    labs(title = "Age and Income",
#         x = "Age",
#         y = "Income")

## ----eval = FALSE-------------------------------------------------------------
#  gssdat |>
#    ggplot(aes(x = race, fill = conpress)) +
#    geom_bar(position = "fill") +
#    labs(title = "Confidence in the Press by Race",
#         x = "Race",
#         y = "Proportion")

## ----eval = FALSE-------------------------------------------------------------
#  import pandas as pd
#  
#  prop_data = (
#      gssdat
#      .groupby(["race", "conpress"])
#      .size()
#      .groupby(level=0)
#      .apply(lambda x: x / x.sum())
#      .reset_index(name="proportion")
#  )
#  
#  sns.barplot(data=prop_data, x="race", y="proportion", hue="conpress")
#  
#  plt.title("Confidence in the Press by Race")
#  plt.xlabel("Race")
#  plt.ylabel("Proportion")
#  plt.show()

