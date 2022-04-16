
library(BatchGetSymbols)

first.date <- Sys.Date()-365
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

print(l.out$df.control)
print(l.out$df.tickers)



df.out <- quantmod::getSymbols(Symbols = c("SPX"),
                               src = "yahoo", from = Sys.Date()-365, to = Sys.Date(), auto.assign = F)





#https://365financialanalyst.com/data/sp500/

# https://github.com/business-science/tidyquant
library(dplyr)
library(tidyquant)

sp_500 <- tq_index("SP500") %>%
  tq_get(get="stock.prices")



tq_transmute_fun_options()

library(plotly)
sp_500_nav <- sp_500 %>% group_by(date) %>% summarise(NAV = sum(volume))

plot_ly(sp_500_nav, x=~date, y=~NAV, type = 'scatter', mode = 'lines')


sp_500_return <- sp_500 %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, mutate_fun = dailyReturn) %>%
  left_join(., sp_500 %>% select(symbol, date, weight), by=c("symbol", "date"))

sp_500_return_index <- sp_500_return %>%
  filter(date > "2022-01-01") %>%
  group_by(date) %>%
  summarise(daily.returns = sum(daily.returns*weight)) %>%
  #mutate(daily.returns = if_else(date == min(date), 0, daily.returns)) %>%
  rbind(data.frame(date=as.Date("2022-01-01"), daily.returns=0),.) %>%
  data.frame() %>%
  mutate(cumret = cumprod(daily.returns+1)*100) %>%
  arrange(date)

plot_ly(sp_500_return_index, x=~date, y=~cumret, type = 'scatter', mode = 'lines')

plot_ly(sp_500_return %>% filter(date >= "2022-01-01") %>% mutate(cumret = cumprod(daily.returns+1)*100), x=~date, y=~cumret, colors = ~symbol, type = 'scatter', mode = 'lines')

library(PortfolioAnalytics)

tbl.SP500Recent <-  tq_get("^SP500TR", get = "stock.prices",
                           from = "1988-01-04", to = "2020-10-31")



sp_500_return_index <- sp_500_return %>%
  group_by(date) %>%
  summarise(daily.returns = sum(daily.returns*weight)) %>%
  mutate(year=substr(date,1,4)) %>%
  group_by(year) %>%
  summarise(year_ret = prod(daily.returns+1)-1)



#https://advantage.factset.com/multi-asset-class-risk-model-ii-white-paper
















#!/usr/bin/Rscript
# get-sp500-data.R
# Author: Curtis Miller
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(quantmod))
# Argument parser for command line use
parser <- ArgumentParser()
parser$add_argument("-v", "--verbose", action = "store_true", default = TRUE,
                    help = "Print extra output [default]")
parser$add_argument("--quietly", action = "store_false",
                    dest = "verbose", help = "Print little output")
parser$add_argument("-f", "--file", type = "character", dest = "csv_name",
                    default = "sp-500.csv",
                    help = "CSV file to save data to [default: sp-500.csv]")
parser$add_argument("-s", "--sleep", type = "integer", dest = "sleeptime",
                    default = 2,
                    help = paste("Time (seconds) between fetching symbols",
                                 "[default: 2] (don't flood websites with",
                                 "requests!)"))
parser$add_argument("--inner", action = "store_true", default = FALSE,
                    dest = "inner",
                    help = paste("Inner join; only dates where all symbols",
                                 "have data will be included"))
parser$add_argument("--start", type = "character", dest = "start",
                    default = "1997-01-01",
                    help = paste("Earliest date (YYYY-MM-DD) to include",
                                 "[default: 1997-01-01]"))
parser$add_argument("--end", type = "character", dest = "end",
                    default = "today",
                    help = paste('Last date (YYYY-MM-DD or "today") to',
                                 'include [default: "today"]'))
parser$add_argument("-k", "--key", type = "character", dest = "api_key",
                    default = NULL,
                    help = "Quandl API key, needed if getting Quandl data")
parser$add_argument("-q", "--quandl", action = "store_true", default = FALSE,
                    dest = "use_quandl", help = "Get data from Quandl")
parser$add_argument("-a", "--adjust", action = "store_true", default = FALSE,
                    dest = "adjust", help = "Adjust prices (Quandl only)")
parser$add_argument("--about", action = "store_true", default = FALSE,
                    dest = "about",
                    help = paste("Print information about the script and its",
                                 "usage, then quit"))
args <- parser$parse_args()
join <- "outer"
if (args$inner) {
  join <- "inner"
}
verbose <- args$verbose
start <- args$start
if (args$end == "today") {
  end <- Sys.Date()
} else {
  end <- args$end
}
sleeptime <- args$sleeptime  # In seconds
csv_name <- args$csv_name
api_key <- args$api_key
use_quandl <- args$use_quandl
adjust <- args$adjust
about <- args$about
if (about) {
  # Display a message, then quit
  comm_name <- substring(commandArgs(trailingOnly = FALSE)[4], 8)
  cat(comm_name, "\n(c) 2017 Curtis Miller\n",
      "Licensed under GNU GPL v. 3.0 available at ",
      "https://www.gnu.org/licenses/gpl-3.0.en.html \n",
      "E-mail: cgmil@msn.com\n\n",
      "This script fetches closing price data for ticker symbols included ",
      "in the S&P 500 stock index. A list of symbols included in the index ",
      "is fetched from this webpage:",
      "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies  The list ",
      "is parsed and the symbols included in the list are fetched from ",
      "either Google Finance (the default) or Quandl (which requires a ",
      "Quandl API key). If Quandl is the data source, adjusted data can be ",
      "fetched instead. The resulting data set is then saved to a CSV",
      "file in the current working directory.\n\n",
      "This package requires the following R packages be installed in order ",
      "to work (all of which are available from CRAN and can be downloaded ",
      "and installed automatically from R via the command ",
      "'install.packages(\"package_name\")'):\n\n",
      "* rvest\n",
      "* magrittr\n",
      "* quantmod\n",
      "* Quandl\n",
      "* tibble\n",
      "* argparse (used only for the command line interface)\n\n",
      "This script was written by Curtis Miller and was made available on ",
      "his website: https://ntguardian.wordpress.com\n\n",
      "You can read more about this script in the following article: ",
      "https://ntguardian.wordpress.com/blog\n\n", sep = "")
  quit()
}
start %<>% as.Date
end %<>% as.Date
options("getSymbols.warning4.0"=FALSE)
sp500_wiki <- read_html(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
symbols_table <- sp500_wiki %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()
symbols_table <- symbols_table[[1]]
symbols <- as.character(symbols_table$`Ticker symbol`)
if (use_quandl) {
  suppressPackageStartupMessages(library(Quandl))  # This will be needed
  Quandl.api_key(api_key)
}
sp500 <- NULL
for (s in symbols) {
  Sys.sleep(sleeptime)
  if (verbose) {
    cat("Processing:", s, "...")
  }
  tryCatch({
    if (use_quandl) {
      s_data <- Quandl.datatable("WIKI/PRICES", ticker = c(s),
                                 date.gte = start, date.lte = end)
      rownames(s_data) <- as.Date(s_data$date)
      if (adjust) {
        s_data <- s_data[, "adj_close", drop = FALSE]
      } else {
        s_data <- s_data[, "close", drop = FALSE]
      }
    } else {
      s_data <- Cl(getSymbols(s, src="google", from = start, to = end,
                              env = NULL))
    }
    names(s_data) <- s
    s_data %<>% as.xts
    if (length(unique(s_data)) > 1) {    # Don't allow what is effectively
      # empty data
      if (is.null(sp500)) {
        sp500 <- as.xts(s_data)
      } else {
        sp500 %<>% merge(s_data, join = join)
      }
      if (verbose) {
        cat(" Got it! From", start(s_data) %>% as.character, "to",
            end(s_data) %>% as.character, "\n")
      }
    } else if (verbose) {
      cat("Sorry, but not this one!\n")
    }
  }, error = function(e) {
    if (verbose) {
      cat("Sorry, but not this one!\n")
    }
  })
}
badsymbols <- setdiff(symbols, names(sp500))
if (verbose & (length(badsymbols) > 0)) {
  cat("There were", length(badsymbols),
      "symbols for which data could not be obtained.\nThey are:", badsymbols,
      "\n")
}
write.csv(rownames_to_column(as.data.frame(sp500), "Date"), file=csv_name)

















#https://www.tradersinsight.news/ibkr-quant-news/how-to-get-historical-sp-500-constituents-data-for-free/


# Load dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rvest)
wikispx <- read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')
currentconstituents <- wikispx %>%
  html_node("#constituents") %>%
  html_table(header = TRUE)
currentconstituents

spxchanges <- wikispx %>%
  html_node("#changes") %>%
  html_table(header = FALSE, fill = TRUE) %>%
  filter(row_number() > 2) %>% # First two rows are headers
  `colnames<-`(c('Date','AddTicker','AddName','RemovedTicker','RemovedName','Reason')) %>%
  mutate(Date = as.Date(Date, format = "%B %d, %Y"),
         year = year(Date),
         month = month(Date))
spxchanges




# Start at the current constituents…
currentmonth <- as.Date(format(Sys.Date(), '%Y-%m-01'))
monthseq <- seq.Date(as.Date('1990-01-01'), currentmonth, by = 'month') %>% rev()

spxstocks <- currentconstituents %>% mutate(Date = currentmonth) %>% select(Date, Ticker = Symbol, Name = Security)
lastrunstocks <- spxstocks

# Iterate through months, working backwards
for (i in 2:length(monthseq)) {
  d <- monthseq[i]
  y <- year(d)
  m <- month(d)
  changes <- spxchanges %>%
    filter(year == year(d), month == month(d))

  # Remove added tickers (we’re working backwards in time, remember)
  tickerstokeep <- lastrunstocks %>%
    anti_join(changes, by = c("Ticker" = "AddTicker")) %>%
    mutate(Date = d)

  # Add back the removed tickers…
  tickerstoadd <- changes %>%
    filter(!RemovedTicker == "") %>%
    transmute(Date = d,
              Ticker = RemovedTicker,
              Name = RemovedName)

  thismonth <- tickerstokeep %>% bind_rows(tickerstoadd)
  spxstocks <- spxstocks %>% bind_rows(thismonth)

  lastrunstocks <- thismonth
}
spxstocks





uni_spx_ticker <- spxstocks$Ticker %>% unique()


#https://www.alphavantage.co/documentation/
#https://www.r-bloggers.com/2017/09/alphavantager-an-r-interface-to-the-free-alpha-vantage-financial-data-api/
#O4AMN6UP1CNSQ13Z
install.packages("alphavantager")

api_keys <- c("O4AMN6UP1CNSQ13Z","IKMF81RA3V5LQFDZ")

library(alphavantager)
av_api_key("O4AMN6UP1CNSQ13Z")
print(av_api_key())
args(av_get)
av_get(symbol = "MSFT", av_fun = "TIME_SERIES_DAILY", outputsize = "compact")
av_get(symbol = "IBM", av_fun = "OVERVIEW")


library("jsonlite")
btc <- data.frame(jsonlite::fromJSON("https://www.alphavantage.co/query?function=OVERVIEW&symbol=MSFT&apikey=O4AMN6UP1CNSQ13Z"))


res <- NULL
for(i in 1:length(uni_spx_ticker)){
  print(i)

  #res <- bind_rows(res, av_get(symbol = uni_spx_ticker[i], av_fun = "TIME_SERIES_DAILY", outputsize = "compact"))
  x = data.frame(jsonlite::fromJSON(paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=",uni_spx_ticker[i],"&outputsize=compact&apikey=O4AMN6UP1CNSQ13Z")))
  print(paste0("size: ",dim(x)[1]," ",dim(x)[2]))
  # try({
  #   res <- bind_rows(res, data.frame(jsonlite::fromJSON(paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=",uni_spx_ticker[i],"&apikey=O4AMN6UP1CNSQ13Z"))))
  # })
  Sys.sleep(1)
}

x = data.frame(jsonlite::fromJSON(paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=AAP&outputsize=compact&apikey=O4AMN6UP1CNSQ13Z")))


k=2
system.time({
  for(i in 1:2){
    x = data.frame(jsonlite::fromJSON(paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=",uni_spx_ticker[i],"&outputsize=compact&apikey=",api_keys[k])))
    y = data.frame(jsonlite::fromJSON(paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=",uni_spx_ticker[i],"&apikey=",api_keys[k])))
    print(paste0(c(dim(x),dim(y)), collapse = " "))
  }
})



"W59FBR90IRDZ8LVX"



