# library(plumber)
# r <- plumb("casi18_plumber.R")
# r$run(port=8000)

#* @get /test
function(){
  rnorm(10)
}

#* Plot a histogram of normals
#* @png
#* @get /rnorm_plot
function(n){
  hist(rnorm(n))
}

#* Return sample of normal variables
#* @param n The number of samples
#* @get /rnorm
function(n){
  rnorm(n)
}
