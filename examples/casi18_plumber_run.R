library(plumber)
r <- plumb("examples/casi18_plumber.R")
r$run(host='0.0.0.0', port=8000)
