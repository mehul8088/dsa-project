# SUPERSTORE SALES ANALYSIS — READY-TO-RUN SCRIPT
# Paste into RStudio and run. Adjust file path at read_csv() if needed.

# --- Libraries ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readr)
library(zoo)
library(forecast)
library(ggplot2)

# --- Config -------------------------------------------------------------------
data_path <- "./data/superstore_dataset2011-2015.csv"   # <-- change if needed
output_dir <- "./output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# --- Load & sanity checks -----------------------------------------------------
if (!file.exists(data_path)) stop("Data file not found: ", data_path)

sales_raw <- read_csv(data_path, show_col_types = FALSE)

# List required columns (adjust names if your CSV has different headers)
expected_cols <- c("Order Date", "Ship Date", "Sales", "Profit", "Shipping Cost",
                   "Category", "Region", "Discount")

missing_cols <- setdiff(expected_cols, names(sales_raw))
if (length(missing_cols) > 0) {
  warning("The following expected columns are missing from the CSV: ",
          paste(missing_cols, collapse = ", "),
          "\nPlease verify your file. Script will continue but may fail where those columns are needed.")
}

# --- Parse dates and add time fields -----------------------------------------
# Use lubridate::dmy safely; fallback to as.Date if parse fails
safe_dmy <- function(x) {
  parsed <- suppressWarnings(lubridate::dmy(x))
  if (all(is.na(parsed)) && !all(is.na(x))) {
    # try common alternative format
    parsed2 <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
    if (all(is.na(parsed2))) {
      warning("Date parsing produced only NA values. Check date formats in your CSV.")
    }
    return(parsed2)
  }
  return(parsed)
}

sales <- sales_raw %>%
  mutate(
    Order.Date = safe_dmy(`Order Date`),
    Ship.Date  = safe_dmy(`Ship Date`),
    Year       = year(Order.Date),
    Month      = lubridate::month(Order.Date, label = TRUE, abbr = TRUE),  # explicit lubridate::month
    YearMonth  = floor_date(Order.Date, "month")
  )

# Remove rows with NA Order.Date (can't use them for time analyses)
na_dates <- sum(is.na(sales$Order.Date))
if (na_dates > 0) message("Dropping ", na_dates, " rows with missing Order.Date.")
sales <- filter(sales, !is.na(Order.Date))

# --- Monthly aggregates -------------------------------------------------------
monthly_revenue <- sales %>%
  group_by(YearMonth) %>%
  summarise(
    total_sales = sum(Sales, na.rm = TRUE),
    total_profit = sum(Profit, na.rm = TRUE),
    total_shipping = sum(`Shipping Cost`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YearMonth)

# Save monthly data
write_csv(monthly_revenue, file.path(output_dir, "monthly_revenue.csv"))

# --- Create time series safely -----------------------------------------------
if (nrow(monthly_revenue) < 12) stop("Not enough monthly observations to create a 12-frequency ts object.")

sales_ts <- ts(
  monthly_revenue$total_sales,
  frequency = 12,
  start = c(year(min(monthly_revenue$YearMonth)),
            month(min(monthly_revenue$YearMonth)))
)

# Plot time series and save
p_ts <- autoplot(sales_ts) +
  ggtitle("Monthly Sales Time Series") +
  xlab("Year") + ylab("Sales") +
  theme_minimal()
ggsave(filename = file.path(output_dir, "monthly_sales_ts.png"), plot = p_ts, width = 10, height = 5)

# --- Train/test split (safe) --------------------------------------------------
n <- length(sales_ts)
h <- min(12, floor(n / 3))  # safe forecast horizon (at most 12 months or 1/3 of data)

start_year <- start(sales_ts)[1]
start_month <- start(sales_ts)[2]

train_ts <- ts(sales_ts[1:(n - h)],
               frequency = 12,
               start = c(start_year, start_month))

# compute test start properly
test_start_index <- n - h + 1
test_start_year <- start_year + floor((start_month + test_start_index - 2) / 12)
test_start_month <- ((start_month + test_start_index - 2) %% 12) + 1

test_ts <- ts(sales_ts[(n - h + 1):n],
              frequency = 12,
              start = c(test_start_year, test_start_month))

# --- ARIMA model --------------------------------------------------------------
set.seed(123)
model <- auto.arima(train_ts, seasonal = TRUE)
fc <- forecast(model, h = h)

# Plot forecast vs actual
p_fc <- autoplot(fc) +
  autolayer(test_ts, series = "Actual") +
  ggtitle("ARIMA Forecast vs Actual") +
  xlab("Year") + ylab("Sales") +
  theme_minimal()
ggsave(filename = file.path(output_dir, "arima_forecast_vs_actual.png"), plot = p_fc, width = 10, height = 5)

# Print forecast accuracy
acc <- accuracy(fc, test_ts)
print(acc)
write_csv(as.data.frame(acc), file.path(output_dir, "forecast_accuracy.csv"))

# --- Rolling averages (daily) -------------------------------------------------
daily_sales <- sales %>%
  group_by(Order.Date) %>%
  summarise(
    daySales = sum(Sales, na.rm = TRUE),
    dayProfit = sum(Profit, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Order.Date)

daily_sales <- daily_sales %>%
  mutate(
    sales_7da = rollmean(daySales, 7, fill = NA, align = "right"),
    profit_7da = rollmean(dayProfit, 7, fill = NA, align = "right")
  )

p_roll <- ggplot(daily_sales, aes(x = Order.Date)) +
  geom_line(aes(y = sales_7da), linewidth = 1) +
  geom_line(aes(y = profit_7da), linewidth = 1, linetype = "dashed") +
  labs(title = "7-Day Rolling Average (Sales & Profit)",
       x = "Date", y = "Amount ($)") +
  theme_minimal()
ggsave(filename = file.path(output_dir, "rolling_7day_sales_profit.png"), plot = p_roll, width = 10, height = 5)

# --- Category performance ----------------------------------------------------
category_performance <- sales %>%
  group_by(Category) %>%
  summarise(
    total_sales = sum(Sales, na.rm = TRUE),
    total_profit = sum(Profit, na.rm = TRUE),
    profit_margin = if_else(total_sales == 0, NA_real_, (total_profit / total_sales) * 100),
    num_orders = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

print(category_performance)
write_csv(category_performance, file.path(output_dir, "category_performance.csv"))

# --- Regional analysis -------------------------------------------------------
regional_sales <- sales %>%
  group_by(Region) %>%
  summarise(
    total_sales = sum(Sales, na.rm = TRUE),
    total_profit = sum(Profit, na.rm = TRUE),
    avg_discount = mean(Discount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

print(regional_sales)
write_csv(regional_sales, file.path(output_dir, "regional_sales.csv"))

# --- Yearly trends -----------------------------------------------------------
yearly_trends <- sales %>%
  group_by(Year) %>%
  summarise(
    total_sales = sum(Sales, na.rm = TRUE),
    total_profit = sum(Profit, na.rm = TRUE),
    num_orders = n(),
    .groups = "drop"
  ) %>%
  arrange(Year) %>%
  mutate(
    yoy_sales_growth = (total_sales / lag(total_sales) - 1) * 100,
    yoy_profit_growth = (total_profit / lag(total_profit) - 1) * 100
  )

print(yearly_trends)
write_csv(yearly_trends, file.path(output_dir, "yearly_trends.csv"))

# --- Quick visuals: top categories & region barplots --------------------------
p_cat <- ggplot(category_performance, aes(x = reorder(Category, total_sales), y = total_sales)) +
  geom_col() +
  coord_flip() +
  labs(title = "Total Sales by Category", x = "", y = "Total Sales") +
  theme_minimal()
ggsave(filename = file.path(output_dir, "sales_by_category.png"), plot = p_cat, width = 8, height = 5)

p_reg <- ggplot(regional_sales, aes(x = reorder(Region, total_sales), y = total_sales)) +
  geom_col() +
  coord_flip() +
  labs(title = "Total Sales by Region", x = "", y = "Total Sales") +
  theme_minimal()
ggsave(filename = file.path(output_dir, "sales_by_region.png"), plot = p_reg, width = 8, height = 5)

# --- End ---------------------------------------------------------------------
message("Analysis complete. Outputs (PNGs and CSVs) saved to: ", normalizePath(output_dir))
