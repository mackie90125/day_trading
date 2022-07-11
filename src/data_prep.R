######################################################################
### prep raw data to create dashboard
######################################################################

# required libraries
library(tidyverse)
library(lubridate)

# read in raw data
file.original <- "/Users/ryan/test_analysis/day_trading/data/raw_data_strategy.csv"
d.original <- read.csv(file.original)

# data structure
str(d.original)

# filter out entry rows
d.entry <- filter(d.original, grepl("^Entry.+", Type)) %>%

  # select columns of interest
  select(Trade.., Strategy, Type, Signal, Date.Time, Profit..) %>%

  # rename columns
  rename(Trade_num = Trade.., Book = Type, Signal_entry = Signal, Time_entry = Date.Time, Profit = Profit..) %>%

  # remove entry from Book
  mutate(Book = str_extract(Book, "(Short|Long)"),

         # add seconds to date.time
         Time_entry = paste0(Time_entry, ":01")) %>%

  # convert to date/time class
  mutate(Time_entry = as_datetime(Time_entry))

# exit rows
d.exit <- filter(d.original, grepl("^Exit.+", Type)) %>%

  # select columns of interest
  select(Trade.., Signal, Date.Time) %>%

  # rename columns
  rename(Trade_num = Trade.., Signal_exit = Signal, Time_exit = Date.Time) %>%

  # add seconds to date.time
  mutate(Time_exit = paste0(Time_exit, ":01")) %>%

  # convert to date/time class
  mutate(Time_exit = as_datetime(Time_exit))

# merge data
d.all <- full_join(d.entry, d.exit, by = "Trade_num") %>%

  # arrange columns
  select(Trade_num, Strategy, Book, Signal_entry, Signal_exit, Time_entry, Time_exit, Profit) %>%

  # weekdays column
  mutate(Weekdays = weekdays(Time_entry),

         # time in trade
         Trade_time = round((Time_exit - Time_entry) / 60),

         # trade hour
         Hour = as.numeric(substr(Time_entry, 12, 13))) %>%

  # sort by trade number
  arrange(Trade_num)

# unique values for strategy combinations
sort(unique(d.all$Strategy))

# create flag variables for each strategy
d.all <- mutate(d.all,
                Strategy_2 = if_else(grepl("(^2$|^2_.+)", Strategy), 1, 0),
                Strategy_9 = if_else(grepl("(^9.*|.+_9.*)", Strategy), 1, 0),
                Strategy_24 = if_else(grepl("(^24.*|.+_24.*)", Strategy), 1, 0),
                Strategy_36 = if_else(grepl("(^36.*|.+_36.*)", Strategy), 1, 0),
                Strategy_37 = if_else(grepl("(^37.*|.+_37.*)", Strategy), 1, 0),
                Strategy_38 = if_else(grepl("(^38.*|.+_38.*)", Strategy), 1, 0),

                # win/lose column
                win_lose = if_else(Profit > 0, "Win", "Lose"),

                # cumulative profit
                Profit_cum = cumsum(Profit))

# calculate draw-down
max.profit <- 0
v.drawdown <- NULL
for(i in seq_len(nrow(d.all))){

  profit.current <- d.all$Profit_cum[i]

  if(profit.current >= max.profit){
    v.drawdown <- c(v.drawdown, 0)
    max.profit <- profit.current
  }else{
    v.drawdown <- c(v.drawdown, max.profit - profit.current)
  }
}
d.all$Drawdown <- v.drawdown

# save data
write.csv(d.all, "/Users/ryan/test_analysis/day_trading/data/output_data.csv", row.names = FALSE)