#Shiny App for investments
#Author: Yusheng Xia


library(shiny)
library(ggplot2) 
library(dplyr)
library(ggplot2) 
library(readr)
library(tibble)
library(tidyr)
library(utils)

# source toss() function
source('Functions.R')

#Inputs and Wigits portion
ui <- fluidPage(
  #title
  titlePanel("Saving Investment Simulations"),
  
  fluidRow(
    #Sliders
    #Initial Amount
    column(4, sliderInput("num",
                          "Initial Amount",
                          step = 500, min = 1, max = 100000, value = 1000, pre = "$")),
    #Return Rate (In %)
    column(4, sliderInput(inputId = "rate",
                          label = "Return Rate (In %)",
                          step = 1, min = 0, max = 20, value = 5)),
    #Years
    column(4, sliderInput(inputId = "year",
                          label = "Years",
                          step = 1, min = 0, max = 50, value = 20)),
    #Annual Contribution
    column(4, sliderInput(inputId = "annual",
                          label = "Annual Contribution",
                          step = 500, min = 0, max = 50000, value = 2000)), 
    #Growth Rate (In %)
    column(4, sliderInput(inputId = "growth",
                          label = "Growth Rate (In %)",
                          step = 1, min = 0, max = 20, value = 2)),
    #Facet
    column(4, selectInput("facet", "Facet?", c("No", "Yes"))),
    
    # Show a plot of the generated distribution
    
    fluidRow(
      column(12, plotOutput("investment_graph"))
    ),
    
    fluidRow(
      column(12,  dataTableOutput("investment_table"))
    )
  )
)

server <- function(input, output) {
  
    output$investment_graph <- renderPlot({
      
      #Future Value no contribution
      no_contrib <- rep(0, input$year + 1)
      for (i in 0:(input$year)) {
      no_contrib[i + 1] <- future_value(input$num, (input$rate * .01), i)
      }
      
      #Future Value annuity
      fixed_contrib <- rep(0, input$year + 1)
      for (i in 0:(input$year)) {
      fixed_contrib[i + 1] <- future_value(input$num, (input$rate * .01), i) + annuity(input$annual, (input$rate * .01), i)
      }
      
      #Future Value growing annuity
      growing_contrib <- rep(0, input$year + 1)
      for (i in 0:(input$year)) {
      growing_contrib[i + 1] <- future_value(input$num, (input$rate * .01), i) + growing_annuity(input$annual, (input$rate * .01), (input$growth * .01), i)
      }
    
      #Years and Graph
      years <- c(0:(input$year))
      modalities <- data.frame(years, no_contrib, fixed_contrib, growing_contrib)
      table_1 <- data.frame(years)
      table_2 <- data.frame(years)
      table_3 <- data.frame(years)
      table_1 <- mutate(table_1, type = rep("no_contrib", input$year + 1), value = no_contrib)
      table_2 <- mutate(table_2, type = rep("fixed_contrib", input$year + 1), value = fixed_contrib)
      table_3 <- mutate(table_3, type = rep("growing_contrib", input$year + 1), value = growing_contrib)
      table_4 <- rbind(table_1, table_2, table_3)
      table_4
      
      if (input$facet == "No") {
     return( 
      ggplot(data = modalities) + 
      geom_line(aes(y = no_contrib, x = years, color = 'blue')) + 
      geom_line(aes(y = fixed_contrib, x = years, color = 'red')) + 
      geom_line(aes(y = growing_contrib, x = years, color = 'green')) + 
      geom_point(aes(x = years, y = no_contrib, color = 'blue'))+ 
      geom_point(aes(x = years, y = fixed_contrib, color = 'red'))+ 
      geom_point(aes(x = years, y = growing_contrib, color = 'green')) +
      scale_color_manual(name = "Legend", labels = c("no_contrib",  "growing_contrib", "fixed_contrib"), values = c("blue", "green", "red")) +
      labs(x = "years", y = "value") + 
      ggtitle("Investment Comparison") +
      theme(plot.title = element_text(hjust = 0.5))
     )
      }
      if (input$facet == "Yes") {
        return(
          ggplot(data = table_4) + 
            geom_line(aes(y = value, x = years, color = type)) + 
            geom_point(aes(y = value, x = years, color = type)) + 
            geom_area(aes(y = value, x = years, fill = type, alpha = 0.3)) +
            facet_wrap(~type) +
            labs(x = "years", y = "value") + 
            ggtitle("Investment Comparison") +
            theme(plot.title = element_text(hjust = 0.5))
        )
      }
    })
   
    output$investment_table <- renderDataTable({
      #Future Value no contribution
      no_contrib <- rep(0, input$year + 1)
      for (i in 0:(input$year)) {
        no_contrib[i + 1] <- future_value(input$num, (input$rate * .01), i)
      }
      
      #Future Value annuity
      fixed_contrib <- rep(0, input$year + 1)
      for (i in 0:(input$year)) {
        fixed_contrib[i + 1] <- future_value(input$num, (input$rate * .01), i) + annuity(input$annual, (input$rate * .01), i)
      }
      
      #Future Value growing annuity
      growing_contrib <- rep(0, input$year + 1)
      for (i in 0:(input$year)) {
        growing_contrib[i + 1] <- future_value(input$num, (input$rate * .01), i) + growing_annuity(input$annual, (input$rate * .01), (input$growth * .01), i)
      }
      
      #Years and Graph
      years <- c(0:(input$year))
      
      #Table
      data.frame(years, no_contrib, fixed_contrib, growing_contrib)
    })
}

#Run the application 
shinyApp(ui = ui, server = server)