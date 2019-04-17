#' @title Future value
#' @desciption Obtains future value of a present value investment given interest rate and time
#' @param a amount
#' @param r rate
#' @param y year
#' @return future value of investment

future_value <- function(a, r, y) {
  value <- a * (1 + r)^y
  return(value)
}

#' @title Future value of Annuity
#' @desciption Obtains future value of a constant yearly saving given interest rate and time
#' @param C contribution
#' @param r rate of return
#' @param t time in years
#' @return future value of investment

annuity <- function(C, r, t) {
  FVA <- C * (((1 + r)^t - 1)/r)
  return(FVA)
}

#' @title Growing_Annuity
#' @desciption Finds future value of growing annuity
#' @param C first contribution
#' @param r rate of return
#' @param g growth rate
#' @param t time in years
#' @return future value of growing investment

growing_annuity <- function(contrib, rate, growth, years) {
  if (rate - growth == 0) {
    FVGA <- c("Infinity")
  }
  if (rate - growth != 0) {  
    FVGA <- contrib * (((1 + rate)^years - (1 + growth)^years)/(rate - growth))
  }
  return(FVGA)
}

Obtain_savings_table <- function(a, c, r, g, t) {
  no_contrib01 <- rep(0, t + 1)
  fixed_contrib01 <- rep(0, t + 1)
  growing_contrib01 <- rep(0, t + 1)
  for (i in 0:t) {
    no_contrib01[i] <- future_value(a, r, i)
    fixed_contrib01[i] <- future_value(a, r, i) + annuity(c, r, i)
    growing_contrib01[i] <- future_value(a, r, i) + growing_annuity(c, r, g, i)
  }
  table <- data.frame(no_contrib01, fixed_contrib01, growing_contrib01)
  return(table)
}