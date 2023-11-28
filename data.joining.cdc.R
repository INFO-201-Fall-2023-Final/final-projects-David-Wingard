library(dplyr)

o2021 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (1).csv")
o2020 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (2).csv")
o2019 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (3).csv")
o2018 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (4).csv")
o2017 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (5).csv")
o2016 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (6).csv")
o2015 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (7).csv")
o2014 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (9).csv")
o2013 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (10).csv")
o2012 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (11).csv")
o2011 <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/CDC.data/ExportCSV (13).csv")

#View(o2011)

filter.df <- function(df) {
  filtered_df <- filter(df, !is.na(YearStart))
  filtered_df <- filter(filtered_df, !(LocationAbbr %in% c("PR", "NE", "GU", "VI")))
  select_df <- select(filtered_df, YearStart, LocationAbbr, Data_Value, 
                      Low_Confidence_Limit, High_Confidence_Limit, Sample_Size, )
    
  return(select_df)
}

# o2011 <- filter.df(o2011)
# o2012 <- filter.df(o2012)
# o2013 <- filter.df(o2013)
# o2014 <- filter.df(o2014)
# o2015 <- filter.df(o2015)
# o2016 <- filter.df(o2016)
# o2017 <- filter.df(o2017)
# o2018 <- filter.df(o2018)
# o2019 <- filter.df(o2019)
# o2020 <- filter.df(o2020)
# o2021 <- filter.df(o2021)

aggr.data.unclean <- list(o2011, o2012, o2013, o2014, o2015, o2016, o2017, o2018, o2019, o2020, o2021)
aggr.data <- lapply(aggr.data.unclean, filter.df)

# obesity.year <- do.call(rbind, aggr.data)
colnames(obesity.year) <- c("year", "state", "obesity.perc", "low.conf.limit", "high.conf.limit", "sample.size")

View(obesity.year)

# obesity_data <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/obesity_data.csv")
