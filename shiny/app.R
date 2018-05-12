#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("CASI 2018",
             tabPanel("1 dimension",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("dist_1dim", "Distribution",
                                      choices = list("Uniform" = "unif",
                                                     "Normal" = "norm",
                                                     "Exponential" = "exp")),
                          sliderInput("n_data_1dim",
                                      "Number of samples:",
                                      min = 1,
                                      max = 1000,
                                      value = 50),
                          uiOutput("parameters_1dim")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("plot_1dim")
                        )
                      )
             ),  # end of 1dim panel
             tabPanel("2 dimensions",
                      selectInput("dist_2dim", "Distribution",
                                  choices = list("Uniform" = "unif",
                                                 "Normal" = "norm",
                                                 "Exponential" = "exp")),
                      sliderInput("n_data_2dim",
                                  "Number of samples:",
                                  min = 1,
                                  max = 1000,
                                  value = 50),
                      uiOutput("parameters_2dim"),
                      plotOutput("plot_2dim")
             )
  )  # end of navbarpage
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$parameters_1dim <- renderUI({
    switch (input$dist_1dim,
            unif = tagList(numericInput("unif_min_1dim", "Min", 0),
                           numericInput("unif_max_1dim", "Max", 1)),
            norm = tagList(numericInput("norm_mean_1dim", "Mean", 1),
                           numericInput("norm_sd_1dim", "SD", 1)),
            exp =  tagList(numericInput("exp_rate_1dim", "Rate", 1))
    )
  })
  
  
  output$plot_1dim <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- switch (input$dist_1dim,
                 unif = runif(input$n_data_1dim, 
                              min = ifelse(is.null(input$unif_min_1dim), 0, input$unif_min_1dim), 
                              max = ifelse(is.null(input$unif_max_1dim), 1, input$unif_max_1dim)),
                 norm = rnorm(input$n_data_1dim, 
                              mean = ifelse(is.null(input$norm_mean_1dim), 0, input$norm_mean_1dim), 
                              sd = ifelse(is.null(input$norm_sd_1dim), 1, input$norm_sd_1dim)),
                 exp =  rexp(input$n_data_1dim, 
                             rate = ifelse(is.null(input$exp_rate_1dim), 1, input$exp_rate_1dim))
    )
    
    hist(x, col = 'darkgray', border = 'white')
  })
  
  
  output$parameters_2dim <- renderUI({
    switch (input$dist_1dim,
            unif = tagList(numericInput("unif_min_2dim", "Min", 0),
                           numericInput("unif_max_2dim", "Max", 1)),
            norm = tagList(numericInput("norm_mean_2dim", "Mean", 1),
                           numericInput("norm_sd_2dim", "SD", 1)),
            exp =  tagList(numericInput("p_rate_2dim", "Rate", 1))
    )
  })
  
  output$plot_2dim <- renderPlot({
    x <- switch (input$dist_2dim,
                 unif = runif(input$n_data_2dim, 
                              min = ifelse(is.null(input$unif_min_2dim), 0, input$unif_min_2dim), 
                              max = ifelse(is.null(input$unif_max_2dim), 1, input$unif_max_2dim)),
                 norm = rnorm(input$n_data_2dim, 
                              mean = ifelse(is.null(input$norm_mean_2dim), 0, input$norm_mean_2dim), 
                              sd = ifelse(is.null(input$norm_sd_2dim), 1, input$norm_sd_2dim)),
                 exp =  rexp(input$n_data_2dim, 
                             rate = ifelse(is.null(input$exp_rate_2dim), 1, input$exp_rate_2dim))
    )
    y <- switch (input$dist_2dim,
                 unif = runif(input$n_data_2dim, 
                              min = ifelse(is.null(input$unif_min_2dim), 0, input$unif_min_2dim), 
                              max = ifelse(is.null(input$unif_max_2dim), 1, input$unif_max_2dim)),
                 norm = rnorm(input$n_data_2dim, 
                              mean = ifelse(is.null(input$norm_mean_2dim), 0, input$norm_mean_2dim), 
                              sd = ifelse(is.null(input$norm_sd_2dim), 1, input$norm_sd_2dim)),
                 exp =  rexp(input$n_data_2dim, 
                             rate = ifelse(is.null(input$exp_rate_2dim), 1, input$exp_rate_2dim))
    )
    
    plot(x, y)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

