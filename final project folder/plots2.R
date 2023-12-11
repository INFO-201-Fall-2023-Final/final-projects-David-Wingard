library(dplyr)
library(ggplot2)
library(RColorBrewer)

df <- read.csv("df.12.4.4")


#   
# scatter plots
#

meats.scatter <- function(df){
  
  ggplot(df, aes(x = year, y = meats.avg)) + 
  geom_point(fill="peachpuff", col="black", shape=21, size=4) +
  geom_smooth(col="black", fill="salmon4") +
  theme(
    panel.border = element_rect(color="black", fill=NA, size=.5),
    plot.background = element_rect(fill = "#E1EFFF", color = "black"),
    panel.background = element_rect(fill="linen", color=NA),
    panel.grid.major = element_line(size = 0.1, linetype = 'twodash', colour = "salmon4"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'twodash', colour = "salmon4"),
    axis.title = element_text(size=15),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust=.5, size=15)) +
  labs(title = "Relative Price of Meat Over Time", x = "Year", y = "Relative Aggregate Cost of Meat") + 
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2017, 2019, 2021))
}

#meats.scatter

produce.scatter <- function(df){
  
  ggplot(df, aes(x = year, y = produce.avg)) + 
  geom_point(fill="palegreen2", col="black", shape=21, size=4) +
  geom_smooth(col="black", fill="seagreen3") +
  theme(
    panel.border = element_rect(color="black", fill=NA, size=.5),
    plot.background = element_rect(fill = "#E1EFFF", color = "black"),
    panel.background = element_rect(fill="#EFFFEF", color=NA),
    panel.grid.major = element_line(size = 0.1, linetype = 'twodash', colour = "seagreen"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'twodash', colour = "seagreen"),
    axis.title = element_text(size=15),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust=.5, size=15)) +
  labs(title = "Relative Price of Produce Over Time", x = "Year", y = "Relative Aggregate Cost of Produce") + 
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2017, 2019, 2021))
}

#produce.scatter

processed.scatter <- function(df){
  
  ggplot(df, aes(x = year, y = processed.avg)) + 
  geom_point(fill="lightsteelblue2", col="black", shape=21, size=4) +
  geom_smooth(col="black", fill="steelblue4") +
  theme(
    panel.border = element_rect(color="black", fill=NA, size=.5),
    plot.background = element_rect(fill = "#E1EFFF", color = "black"),
    panel.background = element_rect(fill="aliceblue", color=NA),
    panel.grid.major = element_line(size = 0.1, linetype = 'twodash', colour = "lightsteelblue2"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'twodash', colour = "lightsteelblue2"),
    axis.title = element_text(size=15),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust=.5, size=15)) +
  labs(title = "Relative Price of Processed Foods Over Time", x = "Year", y = "Relative Aggregate Cost of Processed Foods") + 
  scale_x_continuous(breaks = c(2011, 2013, 2015, 2017, 2019, 2021))
}
   
#processed.scatter    
#
# Obesity over time - by region
#

df$division <- factor(df$division, levels = c("United States", unique(df$division[df$division != "United States"])))

ggplot(df, aes(x = year, y = mean.obesity.region.year, color = division)) + 
  geom_line(linetype = "twodash", linewidth = 1) +
  geom_point(size = 3) +
  theme(
    panel.border = element_rect(color="black", fill= NA, size=.5),
    panel.background = element_rect(fill="aliceblue", color=NA),
    panel.grid.major = element_line(size = 0.1, linetype = 'twodash', colour = "lightsteelblue2"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'twodash', colour = "lightsteelblue2"),
    axis.title = element_text(size=20),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust=.5, size=20),
    legend.title = element_text(size=20),
    legend.key = element_rect(fill = "white"),
    legend.background = element_rect(fill="aliceblue", color="black"),
    legend.text = element_text(size=10)) +
  labs(title = "Obesity Prevalance Over Time", x = "Year", y = "Percent of Population Obese") +
  scale_color_manual(values = c("United States" = "black", "East North Central" = "mediumaquamarine",
                                "Pacific" = "paleturquoise4", "Mountain" = "red", "New England" = "purple",
                                "East South Central" = "deeppink3", "West South Central" = "orange",
                                "South Atlantic" = "maroon4", "West North Central" = "palegreen3",
                                "Middle Atlantic" = "green"))


obesity.lineplot <- function(df){
  
  ggplot(df, aes(x = year, y = mean.obesity.region.year, color = division)) + 
    geom_line(linetype = "twodash", linewidth = 1) +
    geom_point(size = 3) +
    theme(
      panel.border = element_rect(color="black", fill=NA, size=.5),
      panel.background = element_rect(fill="aliceblue", color="black"),
      plot.background = element_rect(fill = "#E1EFFF", color = "black"),
      panel.grid.major = element_line(size = 0.1, linetype = 'twodash', colour = "lightsteelblue2"),
      panel.grid.minor = element_line(size = 0.1, linetype = 'twodash', colour = "lightsteelblue2"),
      axis.title = element_text(size=20),
      axis.text = element_text(size = 15),
      plot.title = element_text(hjust=.5, size=20),
      legend.title = element_text(size=20),
      legend.key = element_rect(fill = "white", color = "black"),
      legend.background = element_rect(fill="aliceblue", color="black"),
      legend.text = element_text(size=10)) +
    labs(title = "Obesity Prevalance Over Time", x = "Year", y = "Percent of Population Obese", color = "Division") +
    scale_color_brewer(palette = "Paired")
}

#US Obesity perc + wealth bracket

df$income.level <- ifelse(df$median.income < 62190, "Low Income",
                          ifelse(df$median.income < 77751, "Medium Income",
                                 ifelse(df$median.income < 9999999, "High Income", NA)))

#hist.df <- df[df$year == 2021, ]
#hist.df <- hist.df[!is.na(hist.df$income.level), ]

income.hist <- function(df){
  
  ggplot(df, aes(x = obesity.perc, fill = income.level)) +
  geom_histogram(binwidth = 1, size=.55, color = "black") +
  theme(
    panel.border = element_rect(color="black", fill=NA, size=.6),
    plot.background = element_rect(fill = "#E1EFFF", color = "black"),
    panel.background = element_rect(fill="aliceblue"),
    panel.grid.major = element_line(linewidth = .4, linetype = 'dotted', colour = "slategray3"),
    panel.grid.minor = element_line(linewidth = .4, linetype = 'dotted', colour = "slategray3"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 15, color="black"),
    axis.text = element_text(size=10, color="black"),
    legend.text = element_text(size=15, color="black"),
    plot.title = element_text(hjust=.5),
    legend.background = element_rect(fill="aliceblue", color="black"),
    legend.title = element_text(hjust=.5, size=15)) +
  labs(x="Percent of Population Obese", y="Quantity of States that Fall Within Range", title='Obesity and Income', fill = "Income Level") +
  scale_fill_brewer(palette = "Greens") +
  scale_x_continuous(breaks = seq(25, 41, 1))
}

# obesity and price of food type

obesity.food <- function(product){

  processed <- 
    ggplot(df, aes(x = processed.avg, y = obesity.perc)) + 
    geom_smooth(col="black", fill="steelblue4") +
    theme(
      panel.border = element_rect(color="black", fill=NA, size=.5),
      plot.background = element_rect(fill = "#E1EFFF", color = "black"),
      panel.background = element_rect(fill="aliceblue", color=NA),
      panel.grid.major = element_line(size = 0.1, linetype = 'twodash', colour = "lightsteelblue2"),
      panel.grid.minor = element_line(size = 0.1, linetype = 'twodash', colour = "lightsteelblue2"),
      axis.title = element_text(size=20),
      axis.text = element_text(size = 10),
      plot.title = element_text(hjust=.5, size=25)) +
    labs(title = "Cost of Processed Foods and Obesity", x = "Relative Aggregate Cost of Processed Foods", y = "Percent of Population Obese")
  
  meat <- 
    ggplot(df, aes(x = meats.avg, y = obesity.perc)) + 
    geom_smooth(col="black", fill="salmon4") +
    theme(
      panel.border = element_rect(color="black", fill=NA, size=.5),
      plot.background = element_rect(fill = "#E1EFFF", color = "black"),
      panel.background = element_rect(fill="linen", color=NA),
      panel.grid.major = element_line(size = 0.1, linetype = 'twodash', colour = "salmon4"),
      panel.grid.minor = element_line(size = 0.1, linetype = 'twodash', colour = "salmon4"),
      axis.title = element_text(size=20),
      axis.text = element_text(size = 10),
      plot.title = element_text(hjust=.5, size=25)) +
    labs(title = "Cost of Meats and Obesity", x = "Relative Aggregate Cost of Meat", y = "Percent of Population Obese")
  
  produce <-
    ggplot(df, aes(x = produce.avg, y = obesity.perc)) + 
    geom_smooth(col="black", fill="seagreen3") +
    theme(
      panel.border = element_rect(color="black", fill=NA, size=.5),
      plot.background = element_rect(fill = "#E1EFFF", color = "black"),
      panel.background = element_rect(fill="#EFFFEF", color=NA),
      panel.grid.major = element_line(size = 0.1, linetype = 'twodash', colour = "seagreen"),
      panel.grid.minor = element_line(size = 0.1, linetype = 'twodash', colour = "seagreen"),
      axis.title = element_text(size=20),
      axis.text = element_text(size = 10),
      plot.title = element_text(hjust=.5, size=25)) +
    labs(title = "Cost of Produce and Obesity", x = "Relative Aggregate Cost of Produce", y = "Percent of Population Obese")

  if(product == "meat"){
    return(meat)
  }else if(product == "produce"){
    return(produce)
  }else if(product == "processed"){
    return(processed)
  }
}

page1Title <- "Exploring the Impacts of Socioeconomic Status on Obesity in America"
page1pGraphHeader2 <- "Our Approach:"
page1Intro1 <- "The United States is the most obese high income country in the world; more than 2 in 5 adults are obese, nearly 1 in 5 children are obese, and 19 states in America have obesity rates over 35 percent. Why is this the case?"
page1Intro2 <- "We chose to examine the impact of food availability: specifically the connection between household income and rates of obesity. Low income households will be more likely to purchase cheaper food, whereas wealthier people are more likely to apportion more money to their food budget. This poses an issue when the most affordable food options are often the most processed and the least healthy."
page1pGraph1 <- "Rates of obesity are consistently increasing over time in every region of the United States. Since 1999, the obesity rate in the US has increased over 10%. During the same period, the prevalence of severe obesity nearly doubled, increasing from 4.7% to 9.2%."
page1pGraphHeader1 <- "How are obesity rates changing over time?"
page1pGraphHeader3 <- "Why should you care?"
page1last1 <- "Obesity is destructive to virtually all aspects of one’s health. It’s a large contributor to diabetes and cardiovascular disease, leading to shorter lifespans while simultaneously decreasing quality of life and general wellbeing."
page1last2 <- "There is a common social notion that obesity is purely self-inflicted, that it is a result of personal decisions and can be easily fixed via exercise and a healthy diet. However, we wanted to challenge this perspective and ask: is there a systemic explanation to America’s obesity epidemic?"

page2Title <- "Is There Correlation Between Wealth and Obesity?"
page2Intro1 <- "To examine this connection, we wanted to see if wealthier states were more likely to have lower rates of obesity, and if poorer states were more likely to have higher rates of obesity."
page2Intro2 <- "We categorized states by their median household incomes: states categorized as high income belong to the top third of median household incomes, those in the middle third are categorized as medium income, and those designated as low income are part of the bottom third."
page2words1 <- "Key Takeaways:"
page2words2 <- "The data displays a clear skew, indicating wealthier states tend to have lower obesity rates, while poorer states tend to have higher obesity rates. In fact, the most obese high income state still has a lower obesity rate than the least obese of the low income states."
page2words3 <- "This distribution is indicative of some correlation between wealth and obesity. We hypothesize this correlation is a result of lack of financial access to healthy foods."

page3Intro <- "We divided the products in our dataset into three distinct categories: meats, produce, and processed goods. Here we compare the relative aggregate change in cost of these goods from 2011-2021:"
page3Intro2 <- "The purpose of these visualizations is to show how the prices of generally unhealthy, processed foods are changing at a different rate than healthier, produce and meats."
page3Words1 <- "Key Takeaways:"
page3Words2 <- "Over time, we see consistent increases in the prices of both produce and meats, while the relative price of processed foods is more variable*. A possible explanation is that while the input costs for produce and meats remain static, production costs of more processed foods can be adjusted to appeal to the market of low-income consumers."
page3Words3 <- "*Our data only contained 12 goods we considered as heavily processed, so our measure of aggregate price for these goods is more susceptible to abrupt change (like we see in 2019) than the models for produce and meats."

page4Title <- "Obesity Rates and Price of Food"
page4Words1 <- "Processed foods are becoming cheaper. Is this Impactful?"
page4Words2 <- "This visualization displays the relationship between the cost of food and obesity rates. For both produce and meat products, we see a positive correlation: meaning that as these products increase in price, obesity rises in tandem."
page4Words3 <- "However, the opposite is true for processed foods. As the price of these foods increase, obesity follows a downward trend."
page4Words4 <- "In other words, as availability of processed foods increases, there are measurable negative impacts on health."
page4last1 <- "What Does This Mean?"
page4last2 <- "The correlations shown above establish a link between wealth and health, and could point to a potential contribution to America’s obesity epidemic."

page5Title <- "Our Conclusions"
page5Words1 <- "What We Discovered:"
page5Words2 <- "First we looked into the prevalence of obesity over time, and found consistent increases across all regions in the United States within our time frame (2011-2021). Next we found an increased likelihood for wealthy states to have lower rates of obesity, and for poorer states to have higher rates."
page5Words3 <- "We then discovered that while the costs of meats and produce increase steadily over time, the costs of processed foods are more stable. From here we examined the price of foods directly against rates of obesity, and found that while a higher price of produce and meats correlates to higher levels of obesity, a higher price of processed foods correlates with lower levels of obesity. These figures suggest that a lack of access to healthy foods contributes to higher rates of obesity in poorer regions of the United States."

page5sol1 <- "Solutions and Implications:"
page5sol2 <- "As we identified the price of food to be correlated with rates of obesity, policy should be directed to incentivize healthy options. A possible solution to this issue would be to enact additional taxes on heavily processed or unhealthy foods, and to use these funds to directly subsidize healthier alternatives such as meats and produce. This policy would not only incentivize consumers to steer away from processed goods, but also increase access to healthy alternatives. "

page5resources1 <- "Data Sources:"
page5resources2 <- "The dataset of food prices was collected and organized by the Bureau of Labor and Statistics, and includes the prices of 116 unique food products from 1995-2022. There are 193,079 observations over this period, collected by the Bureau via surveys and consumer input."
page5resources3 <- "The dataset of obesity rates is collected by the CDC under the Behavioural Risk Factor Surveillance System (BRFSS) program, and shows the changes in obesity rates in the U.S. from 2011 to 2022."
page5resources4 <- "We obtained and compiled a third dataset from the Department of Justice which contained median household incomes for each U.S. state."

page5about1 <- "About us:"
page5about2 <- "Authors: Thomas Witty, David Wingard, Anthony Cabillaje"
page5about3 <- "Affiliation: Info 201 - Foundational Skills for Data Science"
page5about4 <- "University of Washington, Autumn 2023"

