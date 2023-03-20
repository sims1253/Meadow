#' Convenience function that will generate one dataset for each configuration
#' row, print some summaries of the outcome and plot a histogram of the output.
#'
#' @param data_gen_conf A data.frame containing data generation configurations
#' @param data_gen_fun A function for dataset generation using the meadow
#'                     interface (TODO)
#'
#' @return Returns nothing but prints text and histograms.
#' @export
#'
#' @examples
data_gen_conf_y_analysis <- function(data_gen_conf, data_gen_fun) {
  datagen_result <- do.call(
    data_gen_fun,
    data_gen_conf
  )
  dataset <- datagen_result$dataset
  bad_samples <- datagen_result$data_gen_output$bad_samples
  print(paste(data_gen_conf$data_family, data_gen_conf$data_link, "data:"))
  print(paste(
    "y_intercept:", data_gen_conf$data_gen_output$y_intercept,
    "y_sigma:", data_gen_conf$data_gen_output$sigma_y
  ))
  print(paste("min:", min(dataset$y)))
  print(paste("max:", max(dataset$y)))
  print(paste("mean:", mean(dataset$y)))
  print(paste("median:", median(dataset$y)))
  print(paste("bad_samples:", bad_samples))
  hist(dataset$y,
    main = paste(
      data_gen_conf$data_family, data_gen_conf$data_link,
      data_gen_conf$shape,
      "x_y_coef: ",
      data_gen_conf$x_y_coef
    ),
    breaks = 20
  )
}
