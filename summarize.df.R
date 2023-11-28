attach(obesity.income.prices.11.27)

library(dplyr)
library(stringr)
library(tidyr)

df <- mutate(full.df, "perc.obese.per.1000.dollars" = (median.income/1000)/obesity.perc)
#test <- select(df, year, state, perc.obese.per.1000.dollars, income.level)

df <- mutate(df, "income.level" = "")

for(i in 1:nrow(df)) {
  if(is.na(df$median.income[i])) {
    df$income.level[i] <- NA
  } else if(df$median.income[i] <= 56261) {
    df$income.level[i] <- "Low income"
  } else if(df$median.income[i] <= 64827) {
    df$income.level[i] <- "Medium income"
  } else if(df$median.income[i] > 90203) {
    df$income.level[i] <- "High income"
  }
}

df.regions <- summarize(group_by(obesity.income.prices.11.27, Division), 'avg.obesity.region' = mean(obesity.perc, na.rm = TRUE))
df <- select(df.regions, avg.obesity.region)

write.csv(df, file = "obesity.income.prices.11.27.csv", row.names = FALSE)

