library(MyFunctions)
library(tidyverse)
library(tidyquant)
# Retrieving the data from CSPR
DATA <- 
  CRSP.MERGE(freq = 'm',
             START_date = '20150101',
             END_date = '20250101',
             vars = c('permno',#CRSP’s permanent security identifier
                      'prc',# Price
                      'altprc',# alternative price measure
                      'ret', # Holding period return
                      'vol', # Share volume
                      'shrout', # Number of shares outstanding
                      'exchcd', # Exchange code
                      'siccd',# SIC code 
                      'comnam'#company name
             ),  
             usnm = "user",
             pwd = "password", pgpass = FALSE)


#Filtering the required companies from the database
DATA_Filtered <- DATA |> filter(permno %in% c(14691,18257,14555))

#Create a new column with exchange names
DATA_with_Exchange <- DATA_Filtered |> mutate(
  Exchange = case_when(
    exchcd == 1 ~ "NYSE", exchcd == 2 ~ "AMEX", exchcd ==
      3 ~ "NASDAQ"))

print(DATA_with_Exchange, n = 295, width = Inf )

#Create a new column with Industry Name
DATA_with_Industry <- DATA_with_Exchange |>
  mutate(
    industry = case_when(
      siccd >= 1 & siccd <= 999 ~ "Agriculture", siccd >=
        1000 & siccd <= 1499 ~ "Mining", siccd >= 1500 &
        siccd <= 1799 ~ "Construction", siccd >= 2000 &
        siccd <= 3999 ~ "Manufacturing", siccd >= 4000 &
        siccd <= 4899 ~ "Transportation", siccd >= 4900 &
        siccd <= 4999 ~ "Utilities", siccd >= 5000 &
        siccd <= 5199 ~ "Wholesale", siccd >= 5200 &
        siccd <= 5999 ~ "Retail", siccd >= 6000 & siccd <=
        6799 ~ "Finance", siccd >= 7000 & siccd <= 8999 ~
        "Services", siccd >= 9000 & siccd <= 9999 ~ "Public",
      TRUE ~ "Missing"
    )
  )

print(DATA_with_Industry, n = 295, width = Inf)

#Create a new column with Market cap
DATA_with_MktCap <- DATA_with_Industry |>
  # take care of missing values in altprc
  drop_na(altprc) |>
  # taking care of 0s in shrout
  filter(shrout != 0) |>
  # taking absolute value of altprc and divide by 1000 to
  # get the MktCap in millions
  mutate(
    MktCap = abs(altprc) *
      shrout/1000
  )


print(DATA_with_MktCap, n = 295, width = Inf)

#VERSARTIS  was merged with Aravive in October 2018,
#So, we have to adjust for that to make the graph more easy to analyse.
DATA_with_MktCap_adjusted_for_merger<- DATA_with_MktCap|>
  mutate(
    comnam = case_when(
      comnam == "VERSARTIS INC"  ~ "ARAVIVE INC", 
      comnam=="ARAVIVE INC" ~ "ARAVIVE INC",
      comnam=="CYMABAY THERAPEUTICS INC" ~ "CYMABAY THERAPEUTICS INC",
      comnam=="ETON PHARMACEUTICALS INC" ~ "ETON PHARMACEUTICALS INC"
    )
  ) 


#Creating a graph of the market cap of each company
DATA_with_MktCap_adjusted_for_merger|>
  ggplot() + geom_line(
    linewidth = 1, mapping = aes(x = date, y = MktCap, colour = comnam)
  ) + theme_test() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(y = "Market Capitalisation (in $ millions)",x = "Date",color = "Company Name" )


#Graph showing value of $1 invested in the companies
DATA_with_MktCap_adjusted_for_merger|>
  group_by(comnam) |>
  arrange(date) |> 
  mutate(GrossRet = ifelse(is.na(ret), 0, ret) + 1,#Since Eton IPO'D in 2018,replacing
         CumRet = cumprod(GrossRet)) |>#its unobserved returns with 0.
  ggplot() +
  geom_line(linewidth = 1, mapping = aes(x = date, y = CumRet, colour = comnam)) +
  theme_test() +
  labs(y="Value of $1 invested in the Stock", x = "Date",color = "Company Name") 

#Graph showing Stock trading volume of the stocks
DATA_with_MktCap_adjusted_for_merger|>
  group_by(comnam) |>
  arrange(date) |> 
  ggplot() +
  geom_line(linewidth = 1, mapping = aes(x = date, y = vol, colour = comnam)) +
  theme_test() +
  labs(y="Stock trading volume",x = "Date",color = "Company Name" ) +
  scale_y_log10() #to make data more perceptible 


#Filtering out companies in the Pharmaceutical Preparation industry from the database
DATA_For_PharmaceuticalPreparation <- DATA |> filter(siccd == 2834)

#Calculating return of industry
Return_of_PharmaceuticalPreparation= DATA_For_PharmaceuticalPreparation |>
  group_by(date) |>
  summarise(IndustrytRet = mean(ret, na.rm = TRUE))

library("timetk")
library("ROI")
library("ROI.plugin.glpk")
library("ROI.plugin.quadprog")
library("ROI.plugin.symphony")
library("PortfolioAnalytics")

#Comparing returns of Stocks to the Pharmaceutical Industry
Benchmarking_against_industry <- 
  DATA_with_MktCap_adjusted_for_merger |> 
  # convert DATA to a tibble using the tk_tbl() function
  tk_tbl() |> 
  # left join the return of industry to returns of stocks
  left_join(Return_of_PharmaceuticalPreparation, by = "date") |> 
  select(date, comnam, ret,IndustrytRet) |> 
  pivot_wider(names_from = comnam, values_from = ret) |>  # Make wide format
  # Convert it back to xts, which is required by charts.PerformanceSummary()
  tk_xts()

# Comparing the performance of Pharmaceutical Industry with Stocks
charts.PerformanceSummary(Benchmarking_against_industry,main = "Pharmaceutical Industry vs Stocks")


#Getting returns of the broader market
SPY <- 
  tq_get('SPY',
         from = '2015-01-01',
         to = '2025-01-01') |> 
  group_by(Year = year(date),
           Month = month(date)) |> 
  summarise(adjusted = adjusted[date == max(date)],
            date = max(date),
            # .groups argument specifies what to do with the grouping structure
            .groups = 'drop') |> 
  # arrange the dataset by date to make sure the observations are ranked in a
  # chronological order before applying the lag() function.
  arrange(date) |> 
  mutate(SPY = adjusted/lag(adjusted) - 1) |> 
  select(date,
         SPY)

#Comparing returns to the broader market
Benchmarking_against_market <- 
  DATA_with_MktCap_adjusted_for_merger |> 
  tk_tbl() |> 
  left_join(SPY, by = "date") |> 
  select(date, comnam, ret,SPY) |> 
  pivot_wider(names_from = comnam, values_from = ret) |>  # Make wide format
  tk_xts()
# plot the performance
charts.PerformanceSummary(Benchmarking_against_market,main = "Market vs Stocks")

#Graph showing value of $1 dollar invested in 1/N strategy
DATA_with_MktCap_adjusted_for_merger|>
  group_by(date) |>
  summarise(PortRet = mean(ret, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    GrossRet = PortRet + 1, CumRet = cumprod(GrossRet),
    .keep = "unused"
  ) |>
  ggplot(aes(x = date, y = CumRet)) +
  geom_line() + theme_test() + ylab("Value of $1 invested in the simple 1/N strategy") +
  xlab("Date")

#Computing portfolio variance of 1/N portfolio
Portfolio_Variance_of_1_N_portfolio <- DATA_with_MktCap_adjusted_for_merger|>
  group_by(date) |>
  summarise(PortRet = mean(ret, na.rm = TRUE)) |>
  summarise(Portfolio_Var = var(PortRet, na.rm = TRUE))

print(Portfolio_Variance_of_1_N_portfolio)

#creating a new data with monthly return of 1/n portfolio
monthly_return_of_1_N_portfolio<- DATA_with_MktCap_adjusted_for_merger|>
  group_by(date) |>
  summarise(PortRet = mean(ret, na.rm = TRUE))

#Getting risk-free rate
rf <- tq_get(
  "TB3MS", get = "economic.data", from = "2015-01-01", to = "2025-01-01"
) |>
  mutate(rf = price/12/100, date = floor_date(date, "month")) |>
  select(-price, -symbol)

#Creating a new column excess return
Excess_Monthly_Return_for_1_N_portfolio<- monthly_return_of_1_N_portfolio |>
  mutate(date = floor_date(date, "month")) |>
  left_join(rf, by = "date") |>
  mutate(exret=PortRet - rf)


#calculating mean excess return
Mean_Excess_Monthly_Return_for_1_N_portfolio<- mean(Excess_Monthly_Return_for_1_N_portfolio$exret, na.rm = TRUE)

#Computing Sharpe Ratio of 1/N Portfolio
Sharpe_Ratio_for_1_N_portfolio <- Mean_Excess_Monthly_Return_for_1_N_portfolio/sqrt(Portfolio_Variance_of_1_N_portfolio)

print(Sharpe_Ratio_for_1_N_portfolio)

#Creating new Year and Month Columns
DATA_with_MktCap_adjusted_for_merger <- 
  DATA_with_MktCap_adjusted_for_merger |> 
  mutate(
    Year = year(date),
    Month = month(date)
  ) 

#Creating a new column with lagged market capitalisation
# in the previous month.
DATA_with_lagged_MktCap <- 
  DATA_with_MktCap_adjusted_for_merger |> 
  left_join(
    DATA_with_MktCap_adjusted_for_merger |> 
      mutate(MktCap_lag = MktCap,
             date = date %m+% months(1)) |> 
      select(MktCap_lag,
             permno,
             Year,
             Month),
    by = c('permno',
           'Year',
           'Month')
  )

#Computing return of the portfolio
VWPortRet <- 
  DATA_with_lagged_MktCap |> 
  group_by(date) |> 
  summarise(VWPortRet = weighted.mean(ret,
                                      MktCap_lag,
                                      na.rm = TRUE),.groups = 'drop')
#Graph showing value of $1 dollar invested in Value weighted strategy
VWPortRet |> 
  mutate(
    GrossRet = VWPortRet + 1,
    CumRet = cumprod(GrossRet),
    .keep = 'unused'
  ) |> 
  ggplot(aes(x = date,
             y = CumRet)) +
  geom_line() +
  theme_test() +
  ylab('Value of $1 invested in the value-weighted strategy')+
  xlab('Date')   

#Comparing returns of value weighted and equal weighted portfolios 
#from 2022 to 2024
DATA_with_MktCap_adjusted_for_merger |>
  mutate(date = as.Date(date)) |>
  filter(date > as.Date("2022-01-01")) |>
  group_by(date) |>
  summarise(PortRet = mean(ret, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    GrossRet = PortRet + 1, 
    CumRet = cumprod(GrossRet)
  ) |>
  ggplot(aes(x = date,y=CumRet)) +
  geom_line(aes(color = "Equal Weighted"), linewidth = 1) + 
  geom_line(
    data = VWPortRet |> 
      mutate(date = as.Date(date)) |>
      filter(date > as.Date("2022-01-01")) |>
      mutate(GrossRet = VWPortRet + 1, CumRet = cumprod(GrossRet)), 
    aes(x = date,y=CumRet, color = "Value Weighted"), linewidth = 1  
  ) +
  theme_test() +
  labs(
    title = "Equal Weighted vs Value Weighted", 
    x = "Date",
    y = "Value of $1 invested in the Stock",
    color = "Portfolio Type"
  )


# Computing portfolio variance of value weighted portfolio
Portfolio_Variance_MktCap <-  var(VWPortRet$VWPortRet,na.rm = T)


print(Portfolio_Variance_MktCap)


#Computing Excess Monthly Return of value weighted Portfolio
Excess_Monthly_Return_of_value_weighted_portfolio <- VWPortRet |>
  mutate(date = floor_date(date, "month")) |>
  left_join(rf, by = "date") |>
  mutate(exret=VWPortRet - rf)

#calculating mean excess return of value weighted portfolio
Mean_Excess_Return_of_value_weighted_portfolio<-
  mean(Excess_Monthly_Return_of_value_weighted_portfolio$exret, na.rm = TRUE)

#Sharpe ratio of value weighted portfolio
Sharpe_Ratio_for_value_weighted_portfolio <- 
  Mean_Excess_Return_of_value_weighted_portfolio/sqrt(Portfolio_Variance_MktCap)

print(Sharpe_Ratio_for_value_weighted_portfolio)
