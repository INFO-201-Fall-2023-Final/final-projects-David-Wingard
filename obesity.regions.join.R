attach(obesity_data)
attach(regions)

library(dplyr)

obesity.region <- left_join(obesity_data, regions, by = c('state' = 'Postal.Code'))
View(obesity.region)
nrow(obesity_data)
nrow(obesity.region)

obesity.region <- obesity.region[!is.na(obesity.region$high.conf.limit), ]
nrow(obesity.region)
View(obesity.region)

for(i in 1:nrow(obesity.region)) {
  if(obesity.region$Name[i] == "United States") {
    obesity.region$Region.Name[i] <- "United States"
  }
}

write.csv(obesity.region, file = "obesity_region.csv", row.names = FALSE)

# obesity_region <- read.csv("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/obesity_region.csv")
# setwd("C:/Users/HomeLaptop/OneDrive/Desktop/WITTYSCHOOL/")
# read.csv("obesity_region.csv")
