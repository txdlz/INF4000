library(tidyverse)
library(cluster)
library(ggplot2)
library(corrplot)
library(factoextra)
library(dplyr)
library(ggridges)


### Read Files
# Data folder
data_path <- "data/"
file_names <- c("2014_Financial_Data.csv", 
                "2015_Financial_Data.csv", 
                "2016_Financial_Data.csv", 
                "2017_Financial_Data.csv", 
                "2018_Financial_Data.csv")

# Empty list to store data
financial_data <- list()
for (file in file_names) {
  file_path <- file.path(data_path, file)
  year <- as.integer(gsub("_Financial_Data.csv", "", file))
  df <- suppressWarnings(
    read_csv(file_path, col_names = TRUE, show_col_types = FALSE) %>%
      rename(StockCode = `...1`) %>%
      mutate(Year = year) 
  )
  financial_data[[as.character(year)]] <- df
}
```

### Filtering Relevant Columns
# Define required columns
required_columns <- c(
  "StockCode",           # Stock Code
  "Sector",              # Industry
  "Year",                # Year information
  "Revenue",             # Revenue
  "Net Income",          # Net Income
  "Gross Profit",        # Gross Profit
  "Operating Income",    # Operating Income
  "R&D Expenses",        # R&D Expenses
  "SG&A Expense",        # Selling, General and Administrative Expenses
  "EBIT",                # Earnings Before Interest and Taxes
  "Profit Margin",       # Profit Margin
  "Return on Equity",    # Return on Equity
  "Return on Assets"     # Return on Assets
)

filtered_data <- list()
# Filter data for each year
for (year in names(financial_data)) {
  df <- financial_data[[year]]
  valid_columns <- required_columns[required_columns %in% colnames(df)]
  filtered_data[[year]] <- df %>%
    select(all_of(valid_columns))
}


### Handling Missing Values
# Handle missing values
cleaned_data <- list()
for (year in names(filtered_data)) {
  df <- filtered_data[[year]]
  # Remove rows with missing values
  df_clean <- df %>% drop_na()
  cleaned_data[[year]] <- df_clean
  cat("Original rows: ", nrow(filtered_data[[year]]), "\n")
  cat("Rows after cleaning: ", nrow(df_clean), "\n")
  cat("\n")
}

df_2014 <- cleaned_data[["2014"]] %>% mutate(Year = 2014)
df_2015 <- cleaned_data[["2015"]] %>% mutate(Year = 2015)
df_2016 <- cleaned_data[["2016"]] %>% mutate(Year = 2016)
df_2017 <- cleaned_data[["2017"]] %>% mutate(Year = 2017)
df_2018 <- cleaned_data[["2018"]] %>% mutate(Year = 2018)

# Combine all data
combined_df <- do.call(rbind, cleaned_data)

# Add sorting by year and stock code
combined_df <- combined_df %>%
  mutate(Year = as.integer(Year)) %>%  # Ensure Year is an integer
  arrange(Year, StockCode)             # Sort by Year and StockCode

combined_df





### Visualization of Revenue vs. Net Income
ggplot(combined_df, aes(x = Revenue, y = `Net Income`)) +
  geom_point(aes(size = `Gross Profit`, color = Sector), alpha = 0.6) +
  facet_wrap(~ Year) +
  theme_minimal() +
  scale_color_viridis_d(option = "C") +
  labs(
    title = "Revenue vs. Net Income by Sector Across Years",
    subtitle = "Bubble size indicates Gross Profit",
    x = "Revenue (USD)",
    y = "Net Income (USD)",
    color = "Sector",
    size = "Gross Profit"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#Ridgeline Plot - Faceted Version
plot_ridgeline_facet <- function(data) {
  ggplot(data, aes(x = `SG&A Expense`, y = fct_reorder(Sector, `SG&A Expense`), fill = Sector)) +
    geom_density_ridges(
      alpha = 0.8, # Transparency
      scale = 1,
      rel_min_height = 0.01 # Minimum height for density
    ) +
    scale_fill_viridis_d(option = "C", guide = "none") +
    labs(
      title = "SG&A Expense Distribution by Sector (Faceted by Year)",
      x = "SG&A Expense (USD)",
      y = "Sector"
    ) +
    theme_ridges(grid = TRUE) +
    theme(
      axis.text.y = element_text(size = 6),
      axis.text.x = element_text(size = 8),
      axis.title.x = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1.5, "lines")
    ) +
    coord_cartesian(xlim = c(-1e10, 1e10)) +
    facet_wrap(~Year, ncol = 2) 
}

#Generate the faceted ridgeline plot
plot_ridgeline_facet(combined_df)


#Select representative sectors
selected_sectors <- c("Technology", "Communication Services", "Energy", "Consumer Defensive", "Financial Services")

#Filter and calculate the average net income per year for each sector
sector_trends <- combined_df %>%
  filter(Sector %in% selected_sectors) %>%
  group_by(Year, Sector) %>%
  summarise(Avg_Net_Income = mean(`Net Income`, na.rm = TRUE), .groups = "drop")

#Convert Avg_Net_Income to billions of USD
sector_trends <- sector_trends %>%
  mutate(Avg_Net_Income = Avg_Net_Income / 1e8)

#Identify extreme points
extreme_points <- sector_trends %>%
  filter(
    Avg_Net_Income == max(Avg_Net_Income) | 
    Avg_Net_Income == min(Avg_Net_Income)
  )

#Plot the multi-line chart
ggplot(sector_trends, aes(x = Year, y = Avg_Net_Income, color = Sector, group = Sector)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_text_repel(
    data = extreme_points,
    aes(label = paste(Sector, "\n", round(Avg_Net_Income, 2))),
    size = 4, color = "black", fontface = "bold", show.legend = FALSE
  ) + 
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")) +
  labs(
    title = "Average Net Income by Sector (2014-2018)",
    subtitle = "Selected Sectors with Highlighted Extremes",
    x = "Year",
    y = "Average Net Income (Billion USD)",
    color = "Sector"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#### clustering
perform_clustering <- function(df, year, k = 3, cluster_columns) {
  cat("Year:", year, "\n")
  cluster_data <- df %>%
    select(all_of(cluster_columns)) %>%
    scale()

  final_clustering <- kmeans(cluster_data, centers = k, nstart = 25)
  df$Cluster <- as.factor(final_clustering$cluster)
  cat("Distribution of cluster：\n")
  print(table(df$Cluster))
  cat("\nCluster centre：\n")
  print(final_clustering$centers)
  #Visualisation
  plot <- fviz_cluster(final_clustering, data = cluster_data, 
                        geom = "point", ellipse.type = "convex", 
                        main = paste("K-means Clustering (k =", k, ") for", year), 
                        ggtheme = theme_minimal())
  print(plot)
  
  #Mean
  cluster_summary <- df %>%
    group_by(Cluster) %>%
    summarise(
      Revenue = mean(Revenue, na.rm = TRUE),
      Net_Income = mean(`Net Income`, na.rm = TRUE),
      Gross_Profit = mean(`Gross Profit`, na.rm = TRUE),
      Operating_Income = mean(`Operating Income`, na.rm = TRUE),
      EBIT = mean(EBIT, na.rm = TRUE)
    )
  print(cluster_summary)
  
  return(df)
}
cluster_columns <- c("Revenue", "Net Income", "Gross Profit", "Operating Income", "EBIT")

clustered_2014 <- perform_clustering(df_2014, 2014, k = 3, cluster_columns)
clustered_2015 <- perform_clustering(df_2015, 2015, k = 3, cluster_columns)
clustered_2016 <- perform_clustering(df_2016, 2016, k = 3, cluster_columns)
clustered_2017 <- perform_clustering(df_2017, 2017, k = 3, cluster_columns)
clustered_2018 <- perform_clustering(df_2018, 2018, k = 3, cluster_columns)
