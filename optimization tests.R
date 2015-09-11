require(nloptr)
require(data.table)
require(quantmod)
require(TTR)
require(tidyr)
require(sqldf)
require(dplyr)

setwd("C:/Users/presc/Dropbox/Analytics Projects/Quantifan/quantifan_dfs_optimizer")


# UDF ---------------------------------------------------------------------

source("iterative_solve.r")

playerCov <- function(x, max_date, players){
  
  #filter on date
  x <- x[x$date < max_date, ]
  
  #filter on players playing that day
  x <- x[x$name %in% players, ]
  
  #selct required variables and aggregate across FP
  #using mean as i need to create unique identifiers for all players
  x <- x %>%
    select(name, date, fp)
  x <- dcast(x,  "date ~ name", value.var = "fp", fun.aggregate = mean)
  
  #calculate covariance matrix for optimization
  covar_df <- cov(x[ , -1], use = "pairwise.complete.obs")
  
  #set NA values to 0 as players haven't played together and thus
  #their performance is assumed to be uncorrelated with other players
  covar_df[is.na(covar_df)] <- 0
  
  covar_df
}

playerCor <- function(x, max_date, players){
  
  #filter on date
  x <- x[x$date < max_date, ]
  
  #filter on players playing that day
  x <- x[x$name %in% players, ]
  
  #selct required variables and aggregate across FP
  #using mean as i need to create unique identifiers for all players
  x <- x %>%
    select(name, date, fp)
  x <- dcast(x,  "date ~ name", value.var = "fp", fun.aggregate = mean)
  
  #calculate covariance matrix for optimization
  cor_df <- cor(x[ , -1], use = "pairwise.complete.obs")
  
  #set NA values to 0 as players haven't played together and thus
  #their performance is assumed to be uncorrelated with other players
  cor_df[is.na(cor_df)] <- 0
  
  cor_df
}

# UDV ---------------------------------------------------------------------

ma_n <- 3
min_date <- "2015-01-14"

# Data Load & Setup -------------------------------------------------------

df <- readRDS("player_data.rds")
df[is.na(df)] <- 0

df <- df %>%
  arrange(date) %>%
  group_by(name) %>% 
  mutate(
    obs = length(minutes)
  ) %>%
  filter(obs > 3) %>%
  mutate(
    fp_3 = lag(SMA(fp, n = ma_n)),
    min_3 = lag(SMA(minutes, n = ma_n)),
    proj_fp <- fp_3 * min_3
  ) %>%
  ungroup() %>%
  arrange(name)

#setup dummy positions as it doesn't really matter for analysis
names <- unique(df$name)
pos <- c("C", "PG", "PF", "SG", "SF")
pos <- sample(pos, length(names), replace = T)
names <- data.frame(names, pos)
df <- sqldf("select
            df.*,
            names.pos
            from
            df
            join names on df.name = names.names")


#setup starting date
dates <- unique(df$date)
dates <- dates[dates > min_date]
