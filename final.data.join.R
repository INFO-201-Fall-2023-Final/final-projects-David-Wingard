attach(avg.price)
attach(obesity_region)
attach(income)

library(dplyr)
library(stringr)
library(tidyr)

us.prices <- avg.price[avg.price$area_name == "U.S. city average", ]
us.prices <- us.prices[us.prices$year > 2010, ]
us.prices <- us.prices[us.prices$period == "M01", ]
us.prices <- select(us.prices, -period)

#Income trnadformation

gather.cols <- list("Median.Income.2021", "Median.Income.2019", "Median.Income.2018",
                "Median.Income.2017", "Median.Income.2016", "Median.Income.2015", 
                "Median.Income.2014", "Median.Income.2013", "Median.Income.2012", 
                "Median.Income.2011")

income_year <- gather(income, "Year", -State)

for(i in 1:nrow(income_year)) {
  income_year$year[i] <- sub("Median.Income.", "", income_year$year[i])
}

income_year$year <- as.integer(income_year$year)
colnames(income_year) <- c("state", "year", "median.income")

# JOIN OBESITY + INCOME

obesity.income <- left_join(obesity_region, income_year, by = c('Name' = 'state', 'year' = 'year'))

# WIDEN GROCERY PRIOCES

grocery_wide <- pivot_wider(us.prices, names_from = item_name, values_from = value)


#JOIN OBESITY W/ PRICES

df <- left_join(obesity.income, grocery_wide, by = c('year' = 'year'))

write.csv(df, file = "obesity.income.prices.csv", row.names = FALSE)
