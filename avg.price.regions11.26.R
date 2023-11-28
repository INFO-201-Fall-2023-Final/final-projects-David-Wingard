attach(obesity_region)
attach(avg.price)
attach(region)
attach(cen.div)

library(dplyr)
library(stringr)

ap.recent <- avg.price[avg.price$year > 2010, ]
View(ap.recent)

unique(obesity_region$Region.Name)

# MAKE STRING VECTORS FOR REGIONS

          # se.df <- region[region$Region.Name == "South East", ]
          # se <- unique(se.df$Postal.Code)
          # 
          # fw.df <- region[region$Region.Name == "Far West", ]
          # fw <- unique(fw.df$Postal.Code)
          # 
          # sw.df <- region[region$Region.Name == "South West", ]
          # sw <- unique(sw.df$Postal.Code)
          # 
          # rk.df <- region[region$Region.Name == "Rockies", ]
          # rk <- unique(rk.df$Postal.Code)
          # 
          # ne.df <- region[region$Region.Name == "North East", ]
          # ne <- unique(ne.df$Postal.Code)
          # 
          # ma.df <- region[region$Region.Name == "Mid-Atlantic", ]
          # ma <- unique(ma.df$Postal.Code)
          # 
          # gl.df <- region[region$Region.Name == "Great Lakes", ]
          # gl <- unique(gl.df$Postal.Code)
          # 
          # pl.df <- region[region$Region.Name == "Plains", ]
          # pl <- unique(pl.df$Postal.Code)

pac.df <- cen.div[cen.div$Division == "Pacific", ]
pac <- unique(pac.df$State.Code)

esc.df <- cen.div[cen.div$Division == "East South Central", ]
esc <- unique(esc.df$State.Code)

wsc.df <- cen.div[cen.div$Division == "West South Central", ]
wsc <- unique(wsc.df$State.Code)

mtn.df <- cen.div[cen.div$Division == "Mountain", ]
mtn <- unique(mtn.df$State.Code)

neg.df <- cen.div[cen.div$Division == "New England", ]
neg <- unique(neg.df$State.Code)

sat.df <- cen.div[cen.div$Division == "South Atlantic", ]
sat <- unique(sat.df$State.Code)

wnc.df <- cen.div[cen.div$Division == "West North Central", ]
wnc <- unique(wnc.df$State.Code)

enc.df <- cen.div[cen.div$Division == "East North Central", ]
enc <- unique(enc.df$State.Code)

mat.df <- cen.div[cen.div$Division == "Middle Atlantic", ]
mat <- unique(mat.df$State.Code)

    # pac.df <- region[cen.div$Division == "Pacific", ]
    # pac <- unique(pac.df$Postal.Code)

# MAKE TESTER DF

small <- ap.recent[seq(1, nrow(ap.recent), by = 100), ]

# Create regions for DF

avg.price <- mutate(avg.price, "Region.Name" = 0)

# for(i in 1:nrow(avg.price)) {
#   
#   if(any(str_detect(avg.price$area_name[i], se)) == TRUE) {
#     avg.price$Region.Name[i] = "South East"
#     
#   } else if ((any(str_detect(avg.price$area_name[i], fw)) == TRUE)) {
#     avg.price$Region.Name[i] = "Far West"
#     
#   } else if ((any(str_detect(avg.price$area_name[i], sw)) == TRUE)) {
#     avg.price$Region.Name[i] = "South West"
#     
#   } else if ((any(str_detect(avg.price$area_name[i], rk)) == TRUE)) {
#     avg.price$Region.Name[i] = "Rockies"
#     
#   } else if ((any(str_detect(avg.price$area_name[i], ne)) == TRUE)) {
#     avg.price$Region.Name[i] = "North East"
#     
#   } else if ((any(str_detect(avg.price$area_name[i], ma)) == TRUE)) {
#     avg.price$Region.Name[i] = "Mid-Atlantic"
#     
#   } else if ((any(str_detect(avg.price$area_name[i], gl)) == TRUE)) {
#     avg.price$Region.Name[i] = "Great Lakes"
#     
#   } else if ((any(str_detect(avg.price$area_name[i], pl)) == TRUE)) {
#     avg.price$Region.Name[i] = "Plains"
#     
#   } else {
#     avg.price$Region.Name[i] <- NA
#   }
# }

# CENSUS REGIONS

avg.price <- mutate(avg.price, "Census.Division" = 0)
#avg.price <- select(avg.price, -series_id)

for(i in 1:nrow(avg.price)) {
  
  if((any(str_detect(avg.price$area_name[i], pac)) == TRUE) || avg.price$area_name[i] == "Pacific") {
    avg.price$Census.Division[i] = "Pacific"
    
  } else if ((any(str_detect(avg.price$area_name[i], esc)) == TRUE) || avg.price$area_name[i] == "East South Central") {
    avg.price$Census.Division[i] = "East South Central"
    
  } else if ((any(str_detect(avg.price$area_name[i], wsc)) == TRUE) || avg.price$area_name[i] == "West South Central") {
    avg.price$Census.Division[i] = "West South Central"
    
  } else if ((any(str_detect(avg.price$area_name[i], mtn)) == TRUE) || avg.price$area_name[i] == "Mountain") {
    avg.price$Census.Division[i] = "Mountain"
    
  } else if ((any(str_detect(avg.price$area_name[i], neg)) == TRUE) || avg.price$area_name[i] == "New England") {
    avg.price$Census.Division[i] = "New England"
    
  } else if ((any(str_detect(avg.price$area_name[i], sat)) == TRUE) || avg.price$area_name[i] == "South Atlantic") {
    avg.price$Census.Division[i] = "South Atlantic"
    
  } else if ((any(str_detect(avg.price$area_name[i], wnc)) == TRUE) || avg.price$area_name[i] == "West North Central") {
    avg.price$Census.Division[i] = "West North Central"
    
  } else if ((any(str_detect(avg.price$area_name[i], enc)) == TRUE) || avg.price$area_name[i] == "East North Central") {
    avg.price$Census.Division[i] = "East North Central"
    
  } else if ((any(str_detect(avg.price$area_name[i], mat)) == TRUE) || avg.price$area_name[i] == "Middle Atlantic") {
    avg.price$Census.Division[i] = "Middle Atlantic"
    
  } else {
    avg.price$Census.Division[i] <- "AGGREGATE VALUE"
  }
}

# ASSIGN REGION TO UNIQUE NAMES

unique.area <- avg.price[avg.price$Region.Name == 0, ]
unique(unique.area$area_name)

  # AGGREAGATES

aggregate <- c("U.S. city average", "Size Class D", "Size Class B/C", "South - Size Class B/C", 
               "Northeast - Size Class A", "West - Size Class A", "Midwest - Size Class D", 
               "Northeast - Size Class B/C", "West - Size Class B/C", "Midwest - Size Class A",
               "South - Size Class D", "Midwest - Size Class B/C", "Size Class A", 
               "South - Size Class A")



for(i in 1:nrow(avg.price)) {
  if(((any(str_detect(avg.price$area_name[i], aggregate)) == TRUE))) {
    avg.price$Region.Name[i] <- "AGGREGATE VALUE"
  }
}

  # RANDOS

randoms.df <- unique.area[unique.area$Region.Name != "AGGREGATE VALUE", ]
unique(randoms.df$area_name)

    # for(i in 1:nrow(avg.price)) {
    #   if(avg.price$area_name[i] == "Northeast") {
    #     avg.price$Region.Name[i] <- "North East"
    #   }
    # }
    # 
    # for(i in 1:nrow(avg.price)) {
    #   if(avg.price$area_name[i] == "East North Central") {
    #     avg.price$Region.Name[i] <- "Great Lakes"
    #   }
    # }
    # 
    # for(i in 1:nrow(avg.price)) {
    #   if(avg.price$area_name[i] == "East South Central") {
    #     avg.price$Region.Name[i] <- "South East"
    #   }
    # }
    # 
    # for(i in 1:nrow(avg.price)) {
    #   if(avg.price$area_name[i] == "Pacific") {
    #     avg.price$Region.Name[i] <- "Far West"
    #   }
    # }
    # 
    # for(i in 1:nrow(avg.price)) {
    #   if(avg.price$area_name[i] == "New England") {
    #     avg.price$Region.Name[i] <- "North East"
    #   }
    # }
    
    # for(i in 1:nrow(avg.price)) {
    #   if(avg.price$area_name[i] == "West North Central") {
    #     avg.price$Region.Name[i] <- "Great Lakes"
    #   }
    # }

# OBESITY CENSUS REGIONS JOIN

obesity_region <- left_join(obesity_region, cen.div, by = c('Name' = 'State'))
obesity_region <- select(obesity_region, -State.Code)

for(i in 1:nrow(obesity_region)) {
  
  if(obesity_region$Region.Name[i] == "United States"){
    obesity_region$Region[i] <- "United States"
    obesity_region$Division[i] <- "United States"
  }
}

#write.csv(obesity_region, "obesity_region11.20.csv", row.names = FALSE)
avg.price <- select(avg.price, -Region.Name)
write.csv(avg.price, "avg.price11.26.csv", row.names = FALSE)

# 
# 
# 
# ########
# #OBESITY + GROCERY JOIN
# ########
# 
# 
# obesity.price <- left_join(avg.price, obesity_region, by = c('year' = 'year',
#                                                              'Census.Division' = 'Division'))
# 
# obesity.price <- obesity.price[obesity.price$year > 2010 && obesity.price$year < 2022, ]
# nrow(obesity.price)
# 
# min.max.t <- function(df) {
#   print(min(df$year))
#   print(max(df$year))
#   #print(unique(item_name))
#   
# }
# 
# df <- obesity.price[obesity.price$Census.Division != "AGGREGATE VALUE", ]

# debugs

# View(avg.price)
# pls <- avg.price[avg.price$tester != 0, ]
# View(pls)
# 
# fl.df <- small[small$area_name == "Miami-Fort Lauderdale-West Palm Beach, FL", ]
# se
# fl.df$area_name[3]
# (str_detect(fl.df$area_name[3], se))


