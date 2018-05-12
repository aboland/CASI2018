library(plumber)
r <- plumb("casi18_plumber.R")
r$run(host='0.0.0.0', port=8000)
