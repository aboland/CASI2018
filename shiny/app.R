#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("CASI 2018",
             tabPanel("1 dimension",
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("n_data_1dim",
                                      "Number of samples:",
                                      min = 1,
                                      max = 1000,
                                      value = 50),
                          sliderInput("n_bins_1dim",
                                      "Number of bins:",
                                      min = 1,
                                      max = 100,
                                      value = 30),
                          selectInput("dist_1dim", "Distribution",
                                      choices = list("Uniform" = "unif",
                                                     "Normal" = "norm",
                                                     "Exponential" = "exp")),
                          uiOutput("parameters_1dim")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("plot_1dim")
                        )
                      )
             ),  # end of 1dim panel
             tabPanel("2 dimensions",
                      sidebarLayout(
                        sidebarPanel(
                      selectInput("dist_2dim", "Distribution",
                                  choices = list("Uniform" = "unif",
                                                 "Normal" = "norm",
                                                 "Exponential" = "exp")),
                      sliderInput("n_data_2dim",
                                  "Number of samples:",
                                  min = 1,
                                  max = 1000,
                                  value = 50),
                      uiOutput("parameters_2dim")
                        ),
                      mainPanel(
                      plotOutput("plot_2dim")
                      )
                      )
             )
  )  # end of navbarpage
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$parameters_1dim <- renderUI({
    switch (input$dist_1dim,
            unif = tagList(numericInput("unif_min_1dim", "Min", 0),
                           numericInput("unif_max_1dim", "Max", 1)),
            norm = tagList(numericInput("norm_mean_1dim", "Mean", 0),
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
    
    sample_dim1 <- data.frame(data = x)
    
    dim1_plot_title <- reactive(
      switch (input$dist_1dim,
              unif = paste0(input$n_data_1dim, " samples from uniform distribution"),
              norm = paste0(input$n_data_1dim, 
                            " samples from normal distribution with mean=", input$norm_mean_1dim, 
                            " and standard deviation=", input$norm_sd_1dim),
              exp =  paste0(input$n_data_1dim, " samples from exponential distribution with rate=",input$exp_rate_1dim)
      )
    )
    
    ggplot(data = sample_dim1, aes(data)) + geom_histogram(bins = input$n_bins_1dim) + 
      xlab("") + ylab("") + 
      ggtitle(dim1_plot_title())
  })
  
  
  output$parameters_2dim <- renderUI({
    switch (input$dist_2dim,
            unif = tagList(numericInput("unif_min_2dim", "Min", 0),
                           numericInput("unif_max_2dim", "Max", 1)),
            norm = tagList(numericInput("norm_mean_2dim", "Mean", 0),
                           numericInput("norm_sd_2dim", "SD", 1)),
            exp =  tagList(numericInput("exp_rate_2dim", "Rate", 1))
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
    sample_dim2 <- data.frame(xdata = x, ydata = y)
    
    dim2_plot_title <- reactive(
      switch (input$dist_2dim,
              unif = paste0(input$n_data_2dim, " samples from uniform distribution"),
              norm = paste0(input$n_data_2dim, 
                            " samples from normal distribution with mean=", input$norm_mean_2dim, 
                            " and standard deviation=", input$norm_sd_2dim),
              exp =  paste0(input$n_data_2dim, " samples from exponential distribution with rate=",input$exp_rate_2dim)
      )
    )
    
    ggplot(data = sample_dim2, aes(x = xdata, y = ydata)) + geom_point() + 
      xlab("") + ylab("") + 
      ggtitle(dim2_plot_title())
    # plot(x, y)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

