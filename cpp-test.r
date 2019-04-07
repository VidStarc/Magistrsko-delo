import_or_install <- function(paket) {
    if (!is.element(paket, .packages(all.available = TRUE))) {
      install.packages(paket)
    } 
    library(paket, character.only = TRUE)
}

import_or_install("readr")
import_or_install("dplyr")
import_or_install("anytime")
import_or_install("lubridate")
import_or_install("ggplot2")
import_or_install("tidyquant")
import_or_install("Rcpp")
import_or_install("tidyr")

# library(devtools)
# install_version("ggplot2",version = "3.0.0")

btc_min_import <- read_csv('podatki/BTC_USD_min.csv') %>%
  mutate(Timestamp=anytime(Timestamp)) %>%
  rename(
    Volume_Base=`Volume_(BTC)`,
    Volume_Quote=`Volume_(Currency)`
  ) 

# cleaning repetitiona on volume

cleanRepetitions <- function(data) {
  start.time <- Sys.time()
  res <- data %>%
    mutate(
      LOpen=lag(Open),
      LClose=lag(Close),
      LHigh=lag(High),
      LLow=lag(Low),
      LVolume_Base=lag(Volume_Base),
      LVolume_Quote=lag(Volume_Quote),
      LWeighted_Price=lag(Weighted_Price)
    ) %>% 
    mutate(
      repeated = (Open == LOpen) & (Close == LClose) & (High == LHigh) & 
        (Low == LLow) & (Volume_Base == LVolume_Base) & 
        (Volume_Quote == LVolume_Quote) & (Weighted_Price == LWeighted_Price)
    ) %>% 
    mutate(
      repeated = ifelse(is.na(repeated), FALSE, repeated)
    ) %>%
    mutate(
      Volume_Base = ifelse(repeated, 0 , Volume_Base),
      Volume_Quote = ifelse(repeated, 0 , Volume_Quote)
    ) %>% 
    select(
      Timestamp, Open, Close, High, Low, Volume_Base, Volume_Quote, Weighted_Price
    )
  print(sprintf("cleanRepetitions duration: %ss", Sys.time() - start.time))
  res
}

calculateCandles <- function(data, yyear = 1, mmonth = 1, dday = 0, hhour=0, mminute = 0) {
  start.time <- Sys.time()
  toAdd <- list()
  timeStampMut <- list()
  if(yyear > 0) {
    toAdd[["Year"]] <- quo(year(Timestamp) %/% (!! yyear) * (!! yyear))
    if(mmonth == 0) {
      timeStampMut[["Timestamp"]] <- quo(Year)
    }
  }  
  if(yyear == 1 && mmonth > 0) {
    toAdd[["Month"]]<- quo(month(Timestamp) %/% (!! mmonth) * (!! mmonth))
    if(dday == 0) {
      timeStampMut[["Timestamp"]] <- quo(make_datetime(Year, Month, 1))
    }
  }
  if(yyear == 1 && mmonth == 1 && dday > 0) {
    toAdd[["Day"]] <- quo(day(Timestamp) %/% (!! dday) * (!! dday))
    if(hhour == 0) {
      timeStampMut[["Timestamp"]] <- quo(make_datetime(Year, Month, Day))
    }
  }
  if(yyear == 1 && mmonth == 1 && dday == 1 && hhour > 0) {
    toAdd[["Hour"]] <- quo(hour(Timestamp) %/% (!! hhour) * (!! hhour))
    if(mminute == 0) {
      timeStampMut[["Timestamp"]] <- quo(make_datetime(Year, Month, Day, Hour))
    }
  }
  if(yyear == 1 && mmonth == 1 && dday == 1 && hhour == 1 && mminute > 0) {
    toAdd[["Minute"]] <- quo(minute(Timestamp) %/% (!! mminute) * (!! mminute))
    timeStampMut[["Timestamp"]] <- quo(make_datetime(Year, Month, Day, Hour, Minute))
  }
  res <- data %>% mutate(
    !!! toAdd
  ) %>%
  rename(OldTimestamp=Timestamp) %>%
  mutate(
    !!! timeStampMut
  ) %>%
  group_by(Timestamp) %>%
  arrange(OldTimestamp) %>%
  summarise(
      Open=first(Open),
      Close=last(Close),
      High=max(High),
      Low=min(Low),
      Volume_Base=sum(Volume_Base),
      Volume_Quote=sum(Volume_Quote)
  ) %>%
  ungroup() %>%
  select(Timestamp, Open, Close, High, Low, Volume_Base, Volume_Quote)
  print(sprintf("Candles duration: %ss", Sys.time() - start.time))
  res
}

btc_1day <- btc_min_import %>% 
  cleanRepetitions %>% 
  calculateCandles(1,1,1) 

# btc_data <- btc_min_import %>%
#   mutate(
#     LOpen=lag(Open),
#     LClose=lag(Close),
#     LHigh=lag(High),
#     LLow=lag(Low),
#     LVolume_Base=lag(Volume_Base),
#     LVolume_Quote=lag(Volume_Quote),
#     LWeighted_Price=lag(Weighted_Price)
#   ) %>% 
#   mutate(
#     repeated = (Open == LOpen) & (Close == LClose) & (High == LHigh) & 
#                 (Low == LLow) & (Volume_Base == LVolume_Base) & 
#                 (Volume_Quote == LVolume_Quote) & (Weighted_Price == LWeighted_Price)
#   ) %>% 
#   mutate(
#     repeated = ifelse(is.na(repeated), FALSE, repeated)
#   ) %>%
#   mutate(
#     Volume_Base = ifelse(repeated, 0 , Volume_Base),
#     Volume_Quote = ifelse(repeated, 0 , Volume_Quote),
#     Year=year(Timestamp),
#     Month=month(Timestamp),
#     Day=day(Timestamp),
#     Hour=hour(Timestamp),
#     Min5=minute(Timestamp) %% 5,
#     Minute=minute(Timestamp)
#   ) %>% 
#   select(
#     Timestamp, Open, Close, High, Low, Volume_Base, Volume_Quote, Weighted_Price, Year, Month, Day, Hour, Min5, Minute
#   )

# Daily Candles

# btc_1day <- btc_data %>%
#     group_by(Year, Month, Day) %>%
#     arrange(Timestamp) %>%
#     summarise(
#       Open=first(Open),
#       Close=last(Close),
#       High=max(High),
#       Low=min(Low),
#       Volume_Base=sum(Volume_Base),
#       Volume_Quote=sum(Volume_Quote)
#     ) %>%
#     mutate(
#       Timestamp=ymd(paste(Year, Month, Day))
#     ) %>%
#     ungroup() %>%
#     select(Timestamp, Open, Close, High, Low, Volume_Base, Volume_Quote)


####### Lineplot

start_date <- as_date("2018-05-01")
end_date <- as_date("2018-10-30")

btc_1day %>% 
  filter(Timestamp >= start_date & Timestamp <= end_date) %>%
  ggplot(aes(x=Timestamp, y=Close)) + 
  geom_line() + 
  labs(title = "BTCUSD", y = "Closing Price", x = "") +
  theme_tq()

# https://github.com/business-science/tidyquant/issues/112
btc_1day %>%
  filter(Timestamp >= start_date & Timestamp <= end_date) %>%
  ggplot(aes(x=Timestamp, y=Close, open = Open, high = High, low = Low, close = Close)) + 
  geom_candlestick() +
  labs(title = "BTCUSD", y = "Price", x = "") +
  theme_tq()  

cppFunction('double cmax(double x, double y, double z) {
  if(x >= y) {
    if(z >= x) return z;
    return x;
  } else {
    if(z >= y) return z;
    return y;
  }
}')

TrueRange <- function(data) {
  data %>% 
    mutate(
      PrevDayClose=lag(Close)
    ) %>%
    mutate(
      hl = High-Low,
      hpdc = High-PrevDayClose, 
      pdcl = PrevDayClose-Low
    ) %>%
    mutate(
      TR=mapply(cmax, hl, hpdc, pdcl)
    ) %>% select(-hl, -hpdc, -pdcl, -PrevDayClose)
}

emaOld <- function(data, n = 20){
  prvi_N <- mean(data$TR[1:n],na.rm = TRUE)
  N <- c(rep(0,n - 1), prvi_N, rep(0, nrow(data)-20))
  for(i in (n + 1):nrow(data)){
    N[i] <- ((n - 1)*N[i-1] + data$TR[i])/n
  }
  data$NFOR <- N
  data
}

N <- function(data, n = 20) {
  data %>%
    mutate(
      N=EMA(TR, n, wilder = TRUE)
    )
}

# entry_s1_old <- function(tabela, n=20){
#   entry <- rep(0, nrow(tabela))
#   RRMIN <- rep(0, nrow(tabela))
#   RRMAX <- rep(0, nrow(tabela))
#   for(i in (n + 1):nrow(tabela)){
#     RRMAX[i] <- max(tabela$Close[(i-n):(i-1)])
#     RRMIN[i] <- min(tabela$Close[(i-n):(i-1)])
#     if(tabela$Close[i] > max(tabela$Close[(i-n):(i-1)])){entry[i] <- 1}
#     if(tabela$Close[i] < min(tabela$Close[(i-n):(i-1)])){entry[i] <- 2}
#   }
#   tabela %>% mutate(
#     entryS1Old=entry,
#     RRMIN=RRMIN,
#     RRMAX=RRMAX
#   )
# }

# markiraj n denvni min in max
SMA_max_min <- function(data, n=20, minName="shortEntry", maxName="longEntry") {
  data %>% 
    mutate(
      rmax=lag(runMax(Close, n = n)),
      rmin=lag(runMin(Close, n = n))
    ) %>%
    mutate(
      !! maxName:=ifelse(Close > rmax, TRUE, FALSE),
      !! minName:=ifelse(Close < rmin, TRUE, FALSE)
    )
}

periodic_N <- function(data, n = 7) {
  Nused <- data$N 
  mask <- !c(TRUE, logical(n - 1))
  Nused[mask] <- NA
  data %>% mutate(
    Nused = Nused
  ) %>% fill(Nused)
}

position_size <- function(data, priceColumn, capital=1e6, percentage=0.01) {
    priceCol <- enquo(priceColumn)
    data %>% mutate(
      Investment=(percentage*capital)/Nused*(!! priceCol)
    )
}

sourceCpp("turtle.cpp")

run_analysis_R <- function(data, skip=0, start=-1L, accountSize=1e6, riskPercent=0.01, stoplossFactor=2, 
                           additionFactor=0.5, longsAndShorts=0L, maxUnits=4, steps=360, allowOverLimit=1L, syncStoploss=1L) {
  nms <- c("long_profit", "short_profit", "long_position_count", "short_position_count", "avg_capital_utilization");
  if(start >= 0) {
    position_nms <- c("position_cnt", "units", "profit")
    long_nms <- paste0("long_", position_nms)
    short_nms <- paste0("short_", position_nms)
    long_stoploss_names <- paste("long_stoploss_", 1:maxUnits)
    short_stoploss_names <- paste("short_stoploss_", 1:maxUnits)
    nms <- c("mark", "capital_utilization", long_nms, long_stoploss_names, short_nms, short_stoploss_names)
  }
  result <- run_analysis(
    data$longEntry, data$longExit,
    data$shortEntry, data$shortExit,
    data$Close, 
    data$Nused,
    accountSize,
    riskPercent,
    stoplossFactor,
    additionFactor,
    longsAndShorts,
    maxUnits,
    steps,
    allowOverLimit,
    syncStoploss,
    skip,
    start
  ) %>% 
    data.frame() %>% 
    setNames(nms)
  data %>% bind_cols(result)  
}

entry_breakout <- 20
exit_breakout <- 10
skip <- max(entry_breakout, exit_breakout) + 1
start_index <- -1519
steps <- 360
accountSize <- 1e6

btc_1day %>%
  TrueRange %>% 
  N %>%
  SMA_max_min(n=entry_breakout, minName="shortEntry", maxName="longEntry") %>%
  SMA_max_min(n=exit_breakout, minName="longExit", maxName="shortExit") %>%
  periodic_N %>%
  position_size(Close) %>%
  run_analysis_R(skip=skip, longsAndShorts = 0L, start=start_index, steps=steps, accountSize=accountSize) %>%
  data.frame %>% 
  mutate(
    total_profit = (long_profit + short_profit)/accountSize,
    long_profit = long_profit/accountSize,
    short_profit = short_profit/accountSize
  )%>%
  select(Timestamp, long_profit, short_profit, total_profit) %>%
  filter(!is.nan(long_profit) & !is.nan(short_profit)) %>% 
  gather("profit_type", "profit", -Timestamp) %>% 
  ggplot(aes(x=Timestamp, y=profit, color=profit_type)) +
  geom_line()


