### About writing functions

# 1. R packages and functions

# 2. Internal functions in R packages

################################
### R packages and functions ###
################################

# Using a function from another package inside function
#' Load pipe and do something
#' @importFrom magrittr %>%
pipe <- function(df) {
  df %>%
    summary()
}

# sample df created
df0 <- data.frame(price = rnorm(1000000,2,0.5),
                  qty = rnorm(1000000, 1,0.25))

pipe(df0) # complaints that it cannot find function %>%

# alt 1

pipe <- function(df) {
  suppressWarnings(suppressMessages(library(magrittr))) # load the function inside the package
  res <- df %>%
    summary()
  
  return(res)
}
system.time(
pipe(df0) # this returns the summary
)

# alt 2
detach(package:magrittr) # detach loaded lib first

pipe2 <- function(df) {
  suppressWarnings(suppressMessages(require(magrittr))) # require should be faster but is not?
  res <- df %>%
    summary()
  
  return(res)
}

system.time(
  pipe2(df0) # this returns the summary
)


##########################################
### 2. Internal function in R packages ###
##########################################

# source: https://www.r-bloggers.com/2019/12/internal-functions-in-r-packages/

# not exported funcionts, example
usethis:::base_and_recommended()

# Example of documenting and writing an internal function

# Documenting internal functions with roxygen2 tags

#' Check if x is even number
#' @param a is an integer
#' @keywords internal
#' @NoRd
is_even <- function(a) {

  x <- a %% 2
  return(x==0)
  
}

