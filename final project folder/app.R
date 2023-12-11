library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(shinyWidgets)
library(stringr)
library(RColorBrewer)
library(shinyjs)
library(shinydashboard)
library(rsconnect)

#setwd("C:/Users/lmwit/Desktop/Thomas/WITTYSCHOOL/AU' 23/INFO 201/FINAL PROJECT/PERSONAL R FILES")
df <- read.csv("df.12.4.4")
source("plots2.R")

df$income.level <- ifelse(df$median.income < 62190, "Low Income",
                          ifelse(df$median.income < 77751, "Medium Income",
                                 ifelse(df$median.income < 9999999, "High Income", NA)))

css.page.header <- "font-size: 30px; font-weight: bold;"
css.header <- "font-size: 18px; font-weight: bold;"
css.body <- "font-size: 18px;"
css.subheader <- "font-size: 22px;"

custom.theme <- bs_theme(
  version = 5,
  bg = "snow",
  fg = "black",
  primary = "#0199F8",
  secondary = "#FF374B",
  base_font = "Maven Pro"
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Obesity and Wealth: A Visual Analysis",
    titleWidth = 600,
    tags$li(
      class = "dropdown",
      tags$style(HTML('
        .main-header .logo {
          text-align: center;
          width: 100%;
        }
        .main-header .navbar {
          display: none;
        }
        .main-header .navbar-custom-menu {
          font-size: 18px;
        }
      '))
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "page1"),
      menuItem("Obesity and Wealth", tabName = "page2"),
      menuItem("Food Prices", tabName = "page3"),
      menuItem("Obesity Vs. Cost of Food", tabName = "page4"),
      menuItem("Conclusions", tabName = "page5")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
      
    .shiny-options-group {
      border: 1px solid black !important;
      border-radius: 5px;
      padding: 10px;
    }'
      ))
    ),
    tabItems(
      tabItem(tabName = "page1",
              h2(page1Title, style = css.page.header),
              h4(page1Intro1, style = css.subheader),
              br(),
              fluidRow(
                column(3,
                       div(
                          style = "padding: 10px;",
                          checkboxGroupInput("lineplotCheckbox", label = h3("Select Divisions:"),
                                             choices = unique(df$division),
                                             selected = unique(df$division))
                          ),
                      ),
                column(9,
                       plotOutput(outputId = "lineplot")
                )
              ),
              h4(page1pGraphHeader2, style = css.header),
              h4(page1Intro2, style = css.body),

              h4(page1pGraphHeader1, style = css.header),
              h4(page1pGraph1, style = css.body),
              
              h4(page1pGraphHeader3, style = css.header),
              h4(page1last1, style = css.body),
              h4(page1last2, style = css.body)
      ),
      
      tabItem(tabName = "page2",
              h2(page2Title, style = css.page.header),
              h4(page2Intro1, style = css.body),
              h4(page2Intro2, style = css.body),
                fluidRow(
                  column(3,
                    br(),
                    br(),
                    br(),
                    checkboxGroupInput("checkbox", label = h3("Select Income Groups:"),
                                      choices = c("Low Income", "Medium Income", "High Income"),
                                      selected = c("Low Income", "Medium Income", "High Income")),
                  ),
                  column(9,
                    plotOutput(outputId = "hist")
                  ),
                ),
              h4(page2words1, style = css.header),
              h4(page2words2, style = css.body),
              h4(page2words3, style = css.body),
      ),
      
      tabItem(tabName = "page3",
            h2("How Has the Cost of Food Changed Over Time?", style = css.page.header),
              fluidRow(
                column(8,
                  h4(page3Intro, style = css.body),
                  h4(page3Intro2, style = css.body)
                ),
                column(4,
                  sliderInput("yearSlider", h4("Select Starting Year:", style = css.header),
                              min = min(df$year),
                              max = max(df$year),
                              value = min(df$year),
                              step = 1)
                )
              ),
              
              fluidRow(
                column(4, plotOutput(outputId = "meats.scatter")),
                column(4, plotOutput(outputId = "produce.scatter")),
                column(4, plotOutput(outputId = "processed.scatter"))
              ),
            
            h4(page3Words1, style = css.header),
            h4(page3Words2, style = css.body),
            h4(page3Words3, style = "font-size: 14px;"),
      ),
      
      tabItem(tabName = "page4",
            h2(page4Title, style = css.page.header),
            h4(page4Words1, style = "font-size: 22px; font-weight: bold"),
            br(),
              
              fluidRow(
                column(4,
                  h4(page4Words2, style = css.body),
                  
                  selectInput(inputId = "food.type", label = h4("Select Food Category:", style = css.header), 
                              choices = c("Meats", "Produce", "Processed foods"),
                              selected = "Processed foods"),
                  
                  h4(page4Words3, style = css.body),
                  h4(page4Words4, style = css.body)
                ),
                column(8,
                  plotOutput(outputId = "foods.obesity")     
                )
              ),
            h4(page4last1, style = css.header),
            h4(page4last2, style = css.body)
      ),
      
      tabItem(tabName = "page5",
            fluidRow(
              column(8,
            
                h4(page5Title, style = css.page.header),
                h4(page5Words1, style = css.header),
                h4(page5Words2, style = css.body),
                h4(page5Words3, style = css.body),
                img(src = "obesity.stats.png", align = "right", width = 260, height = 260, style = "border: 2px solid black;"),
                br(),
                h4(page5sol1, style = css.header),
                h4(page5sol2, style = css.body)
            
          ),
            column(4,
                
               h4(page5resources1, style = css.header),
               h4(page5resources2, style = css.body),
               h4(page5resources3, style = css.body),
               h4(page5resources4, style = css.body),
               
               h4(page5about1, style = css.header),
               h4(page5about2, style = css.body),
               h4(page5about3, style = css.body),
               h4(page5about4, style = css.body)
            )  
          
          )  
      )
      
      
    )
  )
)

server <- function(input, output, session) {
  
  
  output$meats.scatter <- renderPlot({
    year.slider.df <- df[df$year >= input$yearSlider, ]
    meats.scatter(year.slider.df)
  })
  
  output$produce.scatter <- renderPlot({
    year.slider.df <- df[df$year >= input$yearSlider, ]
    produce.scatter(year.slider.df)
  })
  
  output$processed.scatter <- renderPlot({
    year.slider.df <- df[df$year >= input$yearSlider, ]
    processed.scatter(year.slider.df)
  })
  
  hist.data <- reactive({
    levels <- input$checkbox
    hist.data <- filter(df, year == 2021, income.level %in% levels)
    return(hist.data)
  })
  
  output$hist <- renderPlot({
    income.hist(hist.data())
  })
  
  obesity.lineplot.data <- reactive({
    divisions <- input$lineplotCheckbox
    obesity.lineplot.data <- filter(df, division %in% divisions)
    return(obesity.lineplot.data)
  })
  
  output$lineplot <- renderPlot({
    obesity.lineplot(obesity.lineplot.data())
  })
  
  foods.obesity.output <- reactive({
    plot.type <- input$food.type
    if (plot.type == "Meats") {
      return("meat")
    } else if (plot.type == "Produce") {
      return("produce")
    } else if (plot.type == "Processed foods") {
      return("processed")
    } else {
      return(NULL)
    }
  })
  
  observe({
    plot.type <- foods.obesity.output()
    
    output$foods.obesity <- renderPlot({
      if (!is.null(plot.type)) {
        obesity.food(plot.type)
      }
    })
  })
  
  observeEvent(input$sidebarTrigger, {
    shinyjs.toggleSidebar()
  })
  
}

shinyApp(ui, server)
