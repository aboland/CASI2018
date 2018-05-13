#
#      CASI 2018 Sample Shiny App
# 
# Sample application to display some simple capabilities of
# the shiny R package.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI 
ui <- fluidPage(
  # Create a navbar page
  navbarPage("CASI 2018",
             # The first tab which displays a simple histogram
             tabPanel("1 dimension",
                      
                      # Sidebar with various inputs
                      sidebarLayout(
                        sidebarPanel(
                          # Slider for number of samples
                          sliderInput("n_data_1dim",
                                      "Number of samples:",
                                      min = 1,
                                      max = 1000,
                                      value = 50),
                          # Slder for number of bins in the histogram
                          sliderInput("n_bins_1dim",
                                      "Number of bins:",
                                      min = 1,
                                      max = 100,
                                      value = 30),
                          # Select box for the type of distribution to sample from
                          selectInput("dist_1dim", "Distribution",
                                      choices = list("Uniform" = "unif",
                                                     "Normal" = "norm",
                                                     "Exponential" = "exp")),
                          # Reactive UI element which changes depending on the distribution
                          uiOutput("parameters_1dim")
                        ),
                        
                        mainPanel(
                          # Show a histogram of the samples from the selected distribution
                          plotOutput("plot_1dim")
                        )
                      )
             ),  # end of first tab
             
             # The second tab which displays a scatterplot of 2 samples
             tabPanel("2 dimensions",
                      sidebarLayout(
                        sidebarPanel(
                          # Slider for number of samples
                          sliderInput("n_data_2dim",
                                      "Number of samples:",
                                      min = 1,
                                      max = 1000,
                                      value = 50),
                          # Select box for the type of distribution to sample from
                          selectInput("dist_2dim", "Distribution",
                                      choices = list("Uniform" = "unif",
                                                     "Normal" = "norm",
                                                     "Exponential" = "exp")),
                          # Reactive UI element which changes depending on the distribution
                          uiOutput("parameters_2dim")
                        ),
                        mainPanel(
                          plotOutput("plot_2dim")
                        )
                      )
             )  # end of second tab
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
  
  # reactive element to create the histogram
  output$plot_1dim <- renderPlot({
    # generate random samples form selected distribution
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
    # title for the plot
    dim1_plot_title <- switch (input$dist_1dim,
                               unif = paste0(input$n_data_1dim, " samples from uniform distribution"),
                               norm = paste0(input$n_data_1dim, 
                                             " samples from normal distribution with mean=", input$norm_mean_1dim, 
                                             " and standard deviation=", input$norm_sd_1dim),
                               exp =  paste0(input$n_data_1dim, " samples from exponential distribution with rate=",input$exp_rate_1dim)
    )
    
    # create the plot
    ggplot(data = data.frame(data = x), aes(data)) + geom_histogram(bins = input$n_bins_1dim) + 
      xlab("") + ylab("") + 
      ggtitle(dim1_plot_title)
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
    x_sample <- switch (input$dist_2dim,
                 unif = runif(input$n_data_2dim, 
                              min = ifelse(is.null(input$unif_min_2dim), 0, input$unif_min_2dim), 
                              max = ifelse(is.null(input$unif_max_2dim), 1, input$unif_max_2dim)),
                 norm = rnorm(input$n_data_2dim, 
                              mean = ifelse(is.null(input$norm_mean_2dim), 0, input$norm_mean_2dim), 
                              sd = ifelse(is.null(input$norm_sd_2dim), 1, input$norm_sd_2dim)),
                 exp =  rexp(input$n_data_2dim, 
                             rate = ifelse(is.null(input$exp_rate_2dim), 1, input$exp_rate_2dim))
    )
    y_sample <- switch (input$dist_2dim,
                 unif = runif(input$n_data_2dim, 
                              min = ifelse(is.null(input$unif_min_2dim), 0, input$unif_min_2dim), 
                              max = ifelse(is.null(input$unif_max_2dim), 1, input$unif_max_2dim)),
                 norm = rnorm(input$n_data_2dim, 
                              mean = ifelse(is.null(input$norm_mean_2dim), 0, input$norm_mean_2dim), 
                              sd = ifelse(is.null(input$norm_sd_2dim), 1, input$norm_sd_2dim)),
                 exp =  rexp(input$n_data_2dim, 
                             rate = ifelse(is.null(input$exp_rate_2dim), 1, input$exp_rate_2dim))
    )
    # sample_dim2 <- data.frame(xdata = x, ydata = y)
    # title for the plot
    dim2_plot_title <- switch (input$dist_2dim,
                               unif = paste0(input$n_data_2dim, " samples from uniform distribution"),
                               norm = paste0(input$n_data_2dim, 
                                             " samples from normal distribution with mean=", input$norm_mean_2dim, 
                                             " and standard deviation=", input$norm_sd_2dim),
                               exp =  paste0(input$n_data_2dim, " samples from exponential distribution with rate=",input$exp_rate_2dim)
    )
    
    ggplot(data = data.frame(xdata = x_sample, ydata = y_sample), aes(x = xdata, y = ydata)) + geom_point() + 
      xlab("") + ylab("") + 
      ggtitle(dim2_plot_title)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

