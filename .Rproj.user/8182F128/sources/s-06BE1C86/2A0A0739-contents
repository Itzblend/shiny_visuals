# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(data.table)

#source("readDb.R")

data <- read.csv('data/jira.csv')

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                    theme = "yeti",  # <--- To use a theme, uncomment this
                    "Magalorian",
                    tabPanel("Visualizations",
                             sidebarPanel(
                                 sliderInput('Time range',
                                             inputId = "trajectory",
                                             min = as.POSIXct(min(data$currentdate)),
                                             max = as.POSIXct(max(data$currentdate)),
                                             value = c(as.POSIXct(min(data$currentdate)), as.POSIXct(max(data$currentdate))),
                                             timeFormat="%Y-%m-%d", step = 1
                                 ),
                                 selectInput("Project", "Project:",
                                             data$project_key,
                                             selected = data$project_key["KAF"]),
                                 
                                 dateRangeInput("dateRange", "Date range:",
                                                start = Sys.Date() - 30)
                                 
                                 
                             ), # sidebarPanel
                             mainPanel(
                                 h1("Graphs"),
                                 h4("Jira daily statuses"),
                                 
                                 plotOutput(outputId = "stackPlot"),
                                 
                                 h4("Jira cumplot"),
                                 plotOutput(outputId = "cumPlot"),
                                 
                                 h4("Gantt"),
                                 plotOutput(outputId = "gantt"),
                                 
                                 h4("Jira data table"),
                                 tableOutput("dataTable"),
                                 
                                 
                             ) # mainPanel
                             
                    ), # Navbar 1, tabPanel
                    tabPanel("Info", "This panel is intentionally left blank"),
                    tabPanel("Contact", "This panel is intentionally left blank")
                    
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
    
    output$stackPlot <- renderPlot({
        data %>% 
            filter(currentdate >= input$dateRange[1] & currentdate <= input$dateRange[2]) %>%
            #filter(currentdate >= input$trajectory[1] & currentdate <= input$trajectory[2]) %>% 
            filter(project_key == input$Project) %>% 
        ggplot(aes(currentdate, fill = status))+
        geom_bar(aes(y = ..count.., text = paste('Date: ', as.Date(currentdate), '\n',
                                                 'Status: ', status)), position = "fill")
    })
    
    # Cumulative flow diagram
    output$cumPlot <- renderPlot({
      data %>% 
        filter(project_key == input$Project) %>% 
        filter(currentdate >= input$trajectory[1] & currentdate <= input$trajectory[2]) %>%
        filter(status != 'To Do') %>% 
        group_by(currentdate, status) %>% 
        summarise(count = length(status)) %>%
        ggplot(aes(x = currentdate, y = count, fill = status)) +
        geom_col(position = position_stack()) +
        ggtitle("Cumulative Flow Diagram")+
        theme(axis.title.x = element_blank())+
        ylab("Count of Tickets")+
        scale_fill_manual(values = c("#2eb9d1", "#7f1dcf", "#fc3903", 
                                     "#a6948f", "#fc9d03"))
    })
    
    # Gantt
    output$gantt <- renderPlot({
      data %>% 
        distinct(issue_key, .keep_all = TRUE) %>%
        filter(project_key == input$Project) %>%
        filter(status == 'Done') %>%  
        filter(currentdate >= input$trajectory[1] & currentdate <= input$trajectory[2]) %>%
        ggplot()+
        geom_segment(aes(x=as.POSIXct(as.character(created)),
                         xend=as.POSIXct(as.character(closeddate)),
                         y=issue_key,
                         yend=issue_key,
                         color=project_key,
                         text = paste("Ticket name: ", summary, "\n",
                                      "Closing lead time: ", closingleadtime, "days")), size=4)+
        xlab("Date")+
        ylab("Ticket name")+
        labs(color = "Project key")+
        theme(plot.title = element_text(hjust = 0.5))
    })
    
    # Jira table
    output$dataTable <- renderTable({
      data[, c("project_key", "issue_key", "status", "currentdate", "quick_age", input$Project), drop = FALSE] %>% 
      filter(project_key == input$Project)
    }, rownames = FALSE)
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server) -> portfolio

runApp(portfolio, host = "127.0.0.1")
