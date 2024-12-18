library(DT)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(glue)
library(scales)
library(psych)
library(plotly)
library(readr)
library(ggrepel)
library(lubridate)

setwd("C:/Users/sethk/OneDrive/Baruch Classes/Fall 2024/STA 9750 - Basic Software Tools - NON PROJECT/Group Project")

# read in data from cars csv
cars <- read_csv("Data/vehicle_regs.csv")

# filter cars on Vehicles, check only for counties in NYC
# select only cols we want and rename them to something more useful for our purposes
cars <- cars |>
  filter(`Record Type` == "VEH",
         `County` %in% c("KINGS", "QUEENS", "BRONX", "NEW YORK")) |>
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
  filter(!is.na(regDate)) |>                # Remove nulls
  mutate(
    Reg_Date = as.Date(regDate, format = "%m/%d/%Y"),   # Convert to Date
    year = year(Reg_Date)                                  # Extract year
  ) |>
  filter(class %in% c("PAS","OMT","MOT")) #Passenger, Omnibus Taxi, Motorcycle

head(cars)

# registration per county per year
summary_table <- cars |>
  group_by(county, year) |>
  summarise(
    Reg_Count = n(),  # Count of records for each county-year group
    Avg_Mdl_Year = mean(mdlYear, na.rm = TRUE),  # Average mdlYear, excluding NULLs
    Ct_Pas = sum(class == "PAS", na.rm = TRUE),  # Count where class is "PAS"
    Ct_Omt = sum(class == "OMT", na.rm = TRUE),  # Count where class is "OMT"
    Ct_Mot = sum(class == "MOT", na.rm = TRUE)   # Count where class is "MOT"
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
  labs(title = "Vehicle Registrations by County per Year (2014+)",
       x = "Year",
       y = "Count of Vehicle Registrations") +
  theme_minimal() +
  theme(legend.title = element_blank())

# a major problem with this data was that it updates and removes old registrations,
# this means most registrations from before mid-2021 have expired and therefore been removed from the table
ggplot(car_registration_plot_data, aes(x = year, y = registration_count, color = county, group = county)) +
  geom_line(size=2) +
  scale_y_continuous(labels = comma) +   # Remove scientific notation
  labs(title = "Vehicle Registrations by County per Year (2014-2024)",
       x = "Year",
       y = "Count of Vehicle Registrations") +
  theme_minimal() +
  theme(legend.title = element_blank())

#### Car Regs -- US Census
# Using Census Data
regs <- readxl::read_xlsx("Data/Vehicles/Vehicle_Registrations_By_Boro.xlsx",sheet = "Data")

# Calculate percentages and midpoints for vehicle registrations
filtered_regs <- regs |>
  filter(Label %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS")) |>
  group_by(Year) |>
  arrange(Label, .by_group = TRUE) |>
  mutate(
    Percent_Occ = OCC_T / sum(OCC_T) * 100,
    Bar_Midpoint_Occ = cumsum(OCC_T) - OCC_T / 2,
    Percent_Veh = V_Ct / sum(V_Ct) * 100,
    Bar_Midpoint_Veh = cumsum(V_Ct) - V_Ct / 2 
  )


# barplot for occupied homes in NYC
ggplot(filtered_regs, aes(x = Year, y = OCC_T, fill = Label)) +
  geom_bar(stat = "identity") +
  
  geom_text(
    aes(
      label = paste0(round(Percent_Occ, 1), "%"),
      y = Bar_Midpoint_Occ
    ),
    size = 4, fontface = "bold", color = "white"
  ) +
  
  labs(x = "", y = "Occupied Homes Count", title = "Number of Occupied Homes per Year by County") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray95"),
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(min(filtered_regs$Year), max(filtered_regs$Year), by = 1))


# Vehicle Registration by County Plot
ggplot(filtered_regs, aes(x = Year, y = V_Ct, fill = Label)) +
  geom_bar(stat = "identity") +
  
  # # Add percentage labels
  # geom_text(
  #   aes(
  #     label = paste0(round(Percent_Veh, 1), "%"),
  #     y = Bar_Midpoint_Veh
  #   ),
  #   size = 4, fontface = "bold", color = "white"
  # ) +
  
  labs(x = "", y = "Vehicle Registrations Count", title = "Vehicle Registrations by County") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray95"),
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(min(filtered_regs$Year), max(filtered_regs$Year), by = 1))


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
  mutate(Year = as.numeric(Year))  # Ensure Year is numeric for plotting

regs_tidy_filtered <- regs_tidy_filtered %>%
  group_by(Year) %>%
  arrange(Label, .by_group = TRUE) %>%  # Ensure proper ordering
  mutate(
    Cumulative_Value = cumsum(Value),  # Cumulative value for stacking
    Bar_Midpoint = lag(Cumulative_Value, default = 0) + (Value / 2),  # Midpoint for each bar section
    Percent_Value = Value / sum(Value) * 100  # Percentage of total
  )


ggplot(regs_tidy_filtered, aes(x = Year, y = Value, fill = Label)) +
  geom_bar(stat = "identity") +
  
  geom_text(
    aes(
      label = paste0(round(Percent_Value, 1), "%"),
      y = Bar_Midpoint
    ),
    size = 4, fontface = "bold", color = "white"
  ) +
  
  labs(x = "", y = "Vehicle Registrations Count", title = "Vehicle Registrations by County") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray95"),
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(min(regs_tidy_filtered$Year), max(regs_tidy_filtered$Year), by = 1))





# Plot with corrected percentage placement
ggplot(regs_tidy_filtered, aes(x = Year, y = Value, fill = Label)) +
  geom_bar(stat = "identity") +
  
  # Add percentages centered in their respective bar sections
  geom_text(
    aes(
      label = paste0(round(Percent_Value, 1), "%"),
      y = Bar_Midpoint  # Use corrected midpoint for text placement
    ),
    size = 4, fontface = "bold", color = "white"
  ) +
  
  labs(
    title = "Vehicle Registrations by County",
    x = "",
    y = "Vehicle Registrations Count",
    fill = "County"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray95"),
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(min(regs_tidy_filtered$Year), max(regs_tidy_filtered$Year), by = 1))



#####################
# Calculate CAGR for each county
cagr_df <- regs_tidy_filtered %>%
  group_by(Label) %>%
  summarize(
    Starting_Value = Value[Year == 2017],
    Ending_Value = Value[Year == 2023],
    CAGR = ((Ending_Value / Starting_Value)^(1 / (2023 - 2017)) - 1) * 100  # Convert to percentage
  )

# Calculate the average CAGR across counties
average_cagr <- mean(cagr_df$CAGR, na.rm = TRUE) / 100  # Convert to decimal form

# Calculate the midpoint value for 2017
midpoint_2017 <- regs_tidy_filtered %>%
  filter(Year == 2017) %>%
  summarize(Midpoint = mean(Value, na.rm = TRUE)) %>%
  pull(Midpoint)

# Generate the growth line using the calculated average CAGR
growth_line <- data.frame(
  Year = seq(2017, 2023, by = 1),
  Value = midpoint_2017 * (1 + average_cagr)^(seq(0, 6))  # Compound growth
)

# Plot the data with the growth line
ggplot(regs_tidy_filtered, aes(x = Year, y = Value, color = Label, group = Label)) +
  geom_line(size = 1) +
  geom_point() +
  
  # Highlight high and low points
  geom_point(aes(x = High_Year, y = High_Value), color = "darkgreen", size = 3, shape = 17) +
  geom_text(aes(x = High_Year, y = High_Value, label = paste0("High: ", comma(High_Value))), 
            vjust = -1, color = "darkgreen", size = 3) +
  geom_point(aes(x = Low_Year, y = Low_Value), color = "darkred", size = 3, shape = 17) +
  geom_text(aes(x = Low_Year, y = Low_Value, label = paste0("Low: ", comma(Low_Value))), 
            vjust = 1.5, color = "darkred", size = 3) +
  
  # Add the growth line
  geom_line(
    data = growth_line,
    aes(x = Year, y = Value, linetype = "YoY Trendline"), 
    color = "darkblue", size = 1, inherit.aes = FALSE
  ) +
  
  # Annotate the average CAGR
  annotate(
    "text",
    x = 2020,
    y = max(growth_line$Value) * 1.05,
    label = paste0("Trendline: ", round(average_cagr * 100, 2), "%"),
    color = "darkblue",
    size = 5,
    fontface = "bold"
  ) +
  
  # Plot labels and theming
  labs(x = "",
    y = "Registered Vehicles",
    color = "County",
    linetype = "",
    title = "Vehicle Registrations in NYC\n2017 - 2023"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2017, 2023, by = 1)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray85"),
    panel.background = element_rect(fill = "gray85", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_linetype_manual(
    values = c("YoY Trendline" = "dashed"),
    labels = c("YoY Trendline")
  )











##############################
# Produce Ridership v Revenue chart

df <- readxl::read_xlsx("Data/ridership_costs_summary_stats.xlsx")

df$Avg_Age_Iss_DL

df


ggplot(df, aes(x = Year)) +
  # Line for Yearly Subway Ridership
  geom_line(aes(y = Yearly_Subway_Ridership / 1e9, color = "Subway Ridership"), size = 1) +
  geom_point(aes(y = Yearly_Subway_Ridership / 1e9), color = "darkblue", size = 2) +
  
  # Yearly Revenue
  geom_line(aes(y = MTA_Subway_Total_Revenue / 1e9, color = "Revenue"), size = 1) +
  geom_point(aes(y = MTA_Subway_Total_Revenue / 1e9), color = "darkgreen", size = 2) +
  
  # Expenses
  geom_line(aes(y = (MTA_Subway_Total_Expenses * -1) / 1e9, color = "Expenses"), size = 1) +
  geom_point(aes(y = (MTA_Subway_Total_Expenses * -1) / 1e9), color = "black", size = 2) +
  
  # Revenue Less Expenses
  geom_line(aes(y = Revenue_Less_Expenses / 1e9, color = "Revenue Less Expenses"), size = 1) +
  geom_point(aes(y = Revenue_Less_Expenses / 1e9), color = "#d35400", size = 2) +
  
  # X-axis adjustments
  scale_x_continuous(
    breaks = seq(2017, 2023, 1),
    limits = c(2016.5, 2023.5)
  ) +
  
  # Y-axis adjustments
  scale_y_continuous(
    name = "",  # Remove left axis title
    labels = label_number(suffix = "B", accuracy = 0.1),
    sec.axis = sec_axis(~ . * 1, labels = label_number(suffix = "B", accuracy = 0.1))
  ) +
  
  # Vertical dashed lines
  geom_vline(xintercept = c(2017, 2020, 2023), linetype = "dashed", color = "grey") +
  
  # Horizontal line at y = 0 (not in legend)
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  
  # Annotations for Yearly Subway Ridership
  geom_text(data = df[df$Year %in% c(2017, 2020, 2023), ],
            aes(y = Yearly_Subway_Ridership / 1e9, label = paste0(round(Yearly_Subway_Ridership / 1e9, 1), "B")),
            color = "darkblue", hjust = -0.2, vjust = 1.5, size = 3.5) +
  
  # Annotations for Revenue
  geom_text(data = df[df$Year %in% c(2017, 2020, 2023), ],
            aes(y = MTA_Subway_Total_Revenue / 1e9, label = scales::dollar(MTA_Subway_Total_Revenue, scale = 1e-9, prefix = "$", suffix = "B")),
            color = "darkgreen", hjust = -0.2, vjust = -0.5, size = 3.5) +
  
  # Annotations for Expenses
  geom_text(data = df[df$Year %in% c(2017, 2020, 2023), ],
            aes(y = (MTA_Subway_Total_Expenses * -1) / 1e9, label = scales::dollar((MTA_Subway_Total_Expenses * -1), scale = 1e-9, prefix = "$", suffix = "B")),
            color = "darkred", hjust = -0.2, vjust = -0.5, size = 3.5) +
  
  # Annotations for Revenue Less Expenses
  geom_text(data = df[df$Year %in% c(2017, 2020, 2023), ],
            aes(y = Revenue_Less_Expenses / 1e9, label = scales::dollar(Revenue_Less_Expenses, scale = 1e-9, prefix = "$", suffix = "B")),
            color = "#d35400", hjust = -0.2, vjust = -0.5, size = 3.5) +
  
  # Titles and labels
  labs(
    x = "",  # Remove x-axis label
    color = "Legend",
    title = "MTA Subway Ridership and Revenue\n2017-2023"
  ) +
  
  # Theme adjustments
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray85"),  
    panel.background = element_rect(fill = "gray85", color = NA),  
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    axis.title.y = element_text(color = "black"),  
    axis.title.y.right = element_text(color = "black"),  
    axis.text.y.left = element_text(angle = 45, hjust = 1),
    axis.text.y.right = element_text(angle = 45),
    panel.grid.major = element_line(color = "gray15", size = 0.25),
    panel.grid.minor = element_line(color = "gray15", size = 0.25)
  ) +
  scale_x_continuous(
    breaks = seq(2017, 2023, 1),  # Display breaks from 2017 to 2023
    limits = c(2017, 2023)        # Restrict the axis to 2017–2023
  ) +
  scale_x_continuous(
    breaks = seq(2017, 2023, 1),
    limits = c(2017, 2023.5)  # Add padding to the upper limit
  ) +
  
  # Explicit color mapping
  scale_color_manual(
    values = c(
      "Subway Ridership" = "#00BFC4",
      "Revenue" = "#52be80",
      "Expenses" = "#943126",
      "Revenue Less Expenses" = "#F8766D"  # Light orange
    ),
    breaks = c(  # Specify the order of the legend items
      "Subway Ridership",
      "Revenue",
      "Expenses",
      "Revenue Less Expenses"
    )
  )

# ggtitle("Use theme(plot.title = element_text(hjust = 0.5)) to center") +
#   theme(plot.title = element_text(hjust = 0.5))

################## Bike Ridership Data
bike_data <- read_csv("Data/Bicycle_Counts_20241106.csv")


# Convert the 'date' column to date-time format
bike_data$date <- as.POSIXct(bike_data$date, format = "%m/%d/%Y %I:%M:%S %p")

bike_data_filtered <- bike_data |>
  filter(status %in% c(4, 8, 16)) |>
  mutate(year = year(date)) |>
  filter(year >= 2017 & year <= 2023)

#Group by year and sum the `counts` column
yearly_counts <- bike_data_filtered |>
  group_by(year) |>
  summarize(total_counts = sum(counts, na.rm = TRUE))

ggplot(yearly_counts, aes(x = year, y = total_counts)) +
  geom_line(color = "#52be80", size = 1) +
  geom_point(color = "darkblue", size = 3) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  labs(
    title = "NYC Bike Counts (Status: Modified, Validated, Certified)",
    subtitle = "Yearly Bike Volume from 2017 to 2023",
    x = "Year",
    y = "Total Bike Volume"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "gray95"),  
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "black", size = 0.25),
    panel.grid.minor = element_line(color = "black", size = 0.25)
  )
