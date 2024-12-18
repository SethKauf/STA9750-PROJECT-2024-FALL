if (!require("dplyr")) install.packages("dplyr")
if (!require("DT")) install.packages("DT")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("glue")) install.packages("glue")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotly")) install.packages("plotly")
if (!require("psych")) install.packages("psych")
if (!require("readr")) install.packages("readr")
if (!require("scales")) install.packages("scales")
if (!require("tidyverse")) install.packages("tidyverse")

library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(glue)
library(lubridate)
library(plotly)
library(psych)
library(readr)
library(scales)
library(tidyverse)








# read in data from cars csv
cars <- read_csv("../data/vehicle_regs.csv")

# filter cars on Vehicles, check only for counties in NYC
# select only cols we want and rename them to something more useful for our purposes
cars <- cars |>
  filter(
    `Record Type` == "VEH",
    `County` %in% c("KINGS", "QUEENS", "BRONX", "NEW YORK")
  ) |>
  select(
    class = `Registration Class`,
    city = `City`,
    state = `State`,
    zip = `Zip`,
    county = `County`,
    mdlYear = `Model Year`,
    make = `Make`,
    bodyType = `Body Type`,
    fuelType = `Fuel Type`,
    unlWeight = `Unladen Weight`,
    maxGrossWeight = `Maximum Gross Weight`,
    passengers = `Passengers`,
    regDate = `Reg Valid Date`,
    regExpDate = `Reg Expiration Date`,
    color = `Color`,
    scfwInd = `Scofflaw Indicator`,
    spsInd = `Suspension Indicator`,
    revInd = `Revocation Indicator`
  ) |>
  filter(!is.na(regDate)) |> # Remove nulls
  mutate(
    Reg_Date = as.Date(regDate, format = "%m/%d/%Y"), # Convert to Date
    year = year(Reg_Date) # Extract year
  ) |>
  filter(class %in% c("PAS", "OMT", "MOT")) # Passenger, Omnibus Taxi, Motorcycle

head(cars)

# registration per county per year
summary_table <- cars |>
  group_by(county, year) |>
  summarise(
    Reg_Count = n(), # Count of records for each county-year group
    Avg_Mdl_Year = mean(mdlYear, na.rm = TRUE), # Average mdlYear, excluding NULLs
    Ct_Pas = sum(class == "PAS", na.rm = TRUE), # Count where class is "PAS"
    Ct_Omt = sum(class == "OMT", na.rm = TRUE), # Count where class is "OMT"
    Ct_Mot = sum(class == "MOT", na.rm = TRUE) # Count where class is "MOT"
  ) |>
  ungroup()


car_registration_plot_data <- cars |>
  filter(
    year >= 2014
  ) |>
  group_by(county, year) |>
  summarize(registration_count = n(), .groups = "drop") # Count registrations by county and year

# Create the line plot for Vehicle Registration by County
ggplot(car_registration_plot_data, aes(x = year, y = registration_count, color = county, group = county)) +
  geom_line() +
  labs(
    title = "Vehicle Registrations by County per Year (2014+)",
    x = "Year",
    y = "Count of Vehicle Registrations"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# a major problem with this data was that it updates and removes old registrations,
# this means most registrations from before mid-2021 have expired and therefore been removed from the table
ggplot(car_registration_plot_data, aes(x = year, y = registration_count, color = county, group = county)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(labels = comma) + # Remove scientific notation
  labs(
    title = "Vehicle Registrations by County per Year (2014-2024)",
    x = "Year",
    y = "Count of Vehicle Registrations"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

#### Car Regs -- US Census
# Using Census Data
regs <- readxl::read_xlsx("../data/Vehicle_Registrations_By_Boro.xlsx", sheet = "Data")

# Only want in 5 boros
filtered_regs <- regs |>
  filter(Label %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS"))


# Occupied Homes by County
ggplot(filtered_regs, aes(x = Year, y = OCC_T, fill = Label)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Occupied Count", title = "Number of Occupied Homes per Year by County") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray95"),
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    scale_fill_brewer(palette = "Set2")
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(min(regs$Year), max(regs$Year), by = 1))


# Vehicle Registrations by County
ggplot(regs_tidy_filtered, aes(x = Year, y = Value, fill = Label)) +
  geom_bar(stat = "identity") + # stat="identity" tells ggplot to use the actual data
  labs(
    title = "Vehicle Registrations by County",
    x = "Year",
    y = "Registrations",
    fill = "County"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# pivot registration table
# make the year the columns and county the Label/Index
regs_pivot <- regs |>
  select(
    Label,
    Year,
    V_H
  ) |>
  pivot_wider(
    names_from = Year,
    values_from = V_H
  )

# put data back in a more useable format
regs_tidy <- regs_pivot |>
  pivot_longer(
    cols = -Label,
    names_to = "Year",
    values_to = "Value"
  ) |>
  mutate(Year = as.numeric(Year)) # Ensure Year is numeric for plotting


# Filter for the counties of interest and reshape the data for plotting
regs_tidy_filtered <- regs_tidy |>
  filter(Label %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS"))


# Get registration peak and valley for each borough
regs_high_low <- regs_tidy_filtered |>
  group_by(Label) |>
  summarize(
    High_Year = Year[which.max(Value)],
    High_Value = max(Value),
    Low_Year = Year[which.min(Value)],
    Low_Value = min(Value)
  )


# Merge high and low points back into the tidy dataset for annotations
regs_tidy_filtered <- regs_tidy_filtered |>
  left_join(regs_high_low, by = "Label")

# making a quick linear model to try and find the
# line of best fit for registrations
lm_model <- lm(Value ~ Year, data = regs_tidy_filtered)
slope <- coef(lm_model)["Year"]
intercept <- coef(lm_model)["(Intercept)"]

# Create a formatted trendline equation
trend_eq <- paste0("TREND: y = ", round(slope, 2), "x + ", round(intercept, 2))

# Calculate Pearson correlation
correlation <- cor(regs_tidy_filtered$Year, regs_tidy_filtered$Value)
cor_label <- paste0("TREND: ", round(correlation, 2))

# Plot with correlation in the legend
ggplot(regs_tidy_filtered, aes(x = Year, y = Value, color = Label, group = Label)) +
  geom_line(size = 1) +
  geom_point() +

  # Highlight high and low points
  geom_point(aes(x = High_Year, y = High_Value), color = "#00BFC4", size = 3, shape = 17) +
  geom_text(aes(x = High_Year, y = High_Value, label = paste0("High: ", comma(High_Value))),
    vjust = -1, color = "#00BFC4", size = 3
  ) +
  geom_point(aes(x = Low_Year, y = Low_Value), color = "#F8766D", size = 3, shape = 17) +
  geom_text(aes(x = Low_Year, y = Low_Value, label = paste0("Low: ", comma(Low_Value))),
    vjust = 1.5, color = "#F8766D", size = 3
  ) +

  # Add a linear regression trendline (overall trend)
  geom_smooth(
    method = "lm", se = FALSE, aes(group = 1, color = cor_label),
    linetype = "dashed", size = 1
  ) +

  # Add the correlation call-out near the trendline
  annotate("text",
    x = 2020.25,
    y = predict(lm_model, newdata = data.frame(Year = 2021)) + 10000,
    label = paste0("r = ", round(correlation, 2)),
    color = "black",
    size = 4
  ) +
  labs(
    x = "Year",
    y = "Registered Vehicles in NYC",
    color = "County/State"
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2017, 2023, by = 1)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray85"),
    panel.background = element_rect(fill = "gray85", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  ggtitle("Vehicle Registrations Over Time") +
  theme(plot.title = element_text(hjust = 0.5))