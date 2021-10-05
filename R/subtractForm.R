#' subract linear fit to evaluate profile
#' 
#' @param travel 
#'
#' @param total 
#' @param doPlot 
#' @return list of corrected and uncorrected x and y
#' @export
subtractForm <- function(travel, total, doPlot = TRUE) {
  rawData <- cbind(travel, total)
  colnames(rawData) <- c("travel_mm", "total_um")
  x <- rawData
  if (doPlot) {
    par(mfrow = c(1, 2))
    plot(rawData[, 1], rawData[, 2], type = "l")
  }
  fit <- lm(x[, 2] ~ x[, 1])
  fit_data <- x[, 1] * fit$coefficients[[2]] + fit$coefficients[[1]]
  if (doPlot)
    lines(x[, 1], fit_data, type = "l", col = "red")
  x_corr <- x
  x_corr[, 2] <- x[, 2] - fit_data
  if (doPlot)
    plot(x_corr, type = "l")
  return(list(x = x_corr[, 1], y = x_corr[, 2],x_raw = x[,1],y_raw=x[,2]))
}