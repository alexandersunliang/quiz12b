library(dplyr)
library(ggplot2)

set.seed(42123123) 
years <- rep(2004:2023, times=5)
hospital <- rep(paste("Hospital", 1:5), each=20)
deaths <- sample(100:250, 100, replace=TRUE)
marital_status <- sample(c("Married", "Not Married"), 100, replace=TRUE, prob=c(0.6, 0.4))
race <- sample(c("White", "Not White"), 100, replace=TRUE, prob=c(0.7, 0.3))

data <- data.frame(year=years, hospital=hospital, deaths=deaths, marital_status=marital_status, race=race)

head(data)

# test 1: Compare mean deaths between Married and Not Married using a t-test
t.test(deaths ~ marital_status, data=data)

# test 2: Chi-squared test of independence between marital status and race
table_data <- table(data$marital_status, data$race)
chisq.test(table_data)

# Check years and deaths are numeric
is_integer_year <- data$year |> class() == "integer"
is_numeric_deaths <- data$deaths |> class() == "numeric"

# Check no negative deaths
no_negative_deaths <- data$deaths |> min() >= 0

# Check years are within expected range (2004-2023)
years_within_range <- all(data$year >= 2001 & data$year <= 2020)

# Check hospital names are right
expected_hospitals <- c("Hospital 1", "Hospital 2", "Hospital 3", "Hospital 4", "Hospital 5")
correct_hospital_names <- all(data$hospital %in% expected_hospitals)

# Check for any missing values in the 'year' column
no_missing_year <- data$year |> is.na() |> any() |> Negate()

# Check for any missing values in the 'deaths' column
no_missing_deaths <- data$deaths |> is.na() |> any() |> Negate()

# Check marital status only has "Married" or "Not Married"
valid_marital_status <- all(data$marital_status %in% c("Married", "Not Married"))

# Ensure marital status only contains "Married" or "Not Married"
valid_marital_status <- all(data$marital_status %in% c("Married", "Not Married"))

ggplot(data, aes(x=year, y=deaths, color=hospital)) +
  geom_line() +
  theme_minimal() +
  labs(title="Cancer Deaths in Sydney Hospitals (2001-2020)", x="Year", y="Number of Deaths")
