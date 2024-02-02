library(tidyverse)
library(lubridate)
library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)
library(forcats)
library(GGally)
library(stringr)
library(magrittr)

setwd("~/workbook")
con <- dbConnect(RSQLite::SQLite(), "Disaster_Data.db")
dbListTables(con)
declarations_2023 <- as_tibble(dbGetQuery(
  con,
  "SELECT
  disasterNumber,
  state,
  declarationType,
  incidentType,
  declarationDate
  FROM US_Declarations_2023
ORDER BY declarationDate;"
))
dbDisconnect(con)
head(declarations_2023)
colnames(declarations_2023)

# Assuming your data is stored in a tibble named 'declarations_2023'
# Convert declarationDate to POSIXct format
declarations_2023 <- declarations_2023 %>%
  mutate(
    declarationDateTime = ymd_hms(declarationDate),
    Date = as.Date(declarationDateTime),
    Time = format(declarationDateTime, "%H:%M:%S")
  ) %>%
  select(-declarationDateTime)
declarations_2023$Year <- year(declarations_2023$Date)
declarations_2023$Month <- month(declarations_2023$Date)
declarations_2023$Day <- day(declarations_2023$Date)
print(declarations_2023)
head(declarations_2023)
tail(declarations_2023)
colnames(declarations_2023)



annual_disasters <- declarations_2023 %>%
  filter(
    !Year %in% c(2020, 2005, 2024),
    declarations_2023$incidentType != "Biological"
  ) %>%
  group_by(Year) %>%
  summarise(DisasterCount = n())
# Replace 'DisasterCount' with the actual column name

annual_disasters %>%
  filter(DisasterCount < 3000) %>%
  ggplot(aes(x = Year, y = DisasterCount)) +
  geom_point(color = "#00BFC4", size = 1.2) +
  scale_x_continuous(breaks = unique(annual_disasters$Year)) +
  labs(
    title = "Number of Natural Disasters in the Uninted States",
    x = "Year", y = "Disasters Per Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    size = 8
  ))

annual_disasters %>%
  colnames()

head(annual_disasters)

# Rotate and adjust the size of x-axis labels
model_simple <- lm(DisasterCount ~ Year, data = annual_disasters)
future_years <- tibble(Year = c(
  2024, 2025, 2026, 2027, 2028,
  2029, 2030, 2031, 2032, 2033, 2034, 2035
))
predict(model_simple, future_years) %>% round(1)

annual_disasters %>%
  lm(DisasterCount ~ Year, data = .) %>%
  predict(future_years) %>%
  round(2)
summary(lm(DisasterCount ~ Year, data = annual_disasters))


# Plot the data points
plot(annual_disasters$Year, annual_disasters$DisasterCount,
  xlab = "Year", ylab = "Disaster Count",
  main = "Disaster Count Over Years"
)
# Add the linear model regression line
abline(model_simple, col = "red")


# library(knitr)
# library(highr)
# library(evaluate)
# library(xfun)
# setwd("~/Lab5")
# knit("declaration.Rnw")
