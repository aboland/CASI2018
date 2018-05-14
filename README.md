# CASI 2018

Repository for my work for [CASI 2018](http://casi.ie/2018/).


## Running Examples Locally

The source code for the examples can be viewed and downloaded in the [examples](examples) folder.

### Shiny App

The [app.R](examples/app.R) file contains the code for the sample shiny app located at [cas18shiny.aboland.ie](casi18shiny.aboland.ie).

Opening the file in RStudio and clicking the *Run App* button at the top right of the window will run the application locally.

### Plumber Example

The plumber examples can be run locally using the [casi18_plumber.R](examples/casi18_plumber.R) script.  
Download the script and then run the following 3 lines in R from within the same directory as the downloaded file.

`library(plumber)`  
`r <- plumb("casi18_plumber.R")`  
`r$run(port=8000)`

This will start the API and it will then be available to call from within a local web browser using.  

http://localhost:8000/rnorm?n=40  
or  
http://localhost:8000/rnorm_plot?n=40