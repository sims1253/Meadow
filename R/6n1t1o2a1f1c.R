#' Data generator from our TODO paper.
#'
#' The DAG consists of a treatment x, an outcome y, a fork z1, an ancestor of
#' y z2, an ancestor of y z3 and a collider z4.
#' All variables besides y are generated from normal distributions with the
#' respective parameters.
#'
#'
#' @source TODO
#'
#' @param data_N Number of samples to simulate.
#' @param z1_x_coef Mean effect size of z1 on x
#' @param z3_x_coef Mean effect size of z3 on x
#' @param x_y_coef Mean effect size of x on y
#' @param z1_y_coef Mean effect size of z1 on y
#' @param z2_y_coef Mean effect size of z2 on y
#' @param y_z4_coef Mean effect size of y on z4
#' @param x_z4_coef Mean effect size of x on z4
#' @param y_intercept Intercept of the linear predictor term of y.
#' @param sigma_z1 Standard deviation of z1
#' @param sigma_z2 Standard deviation of z2
#' @param sigma_z3 Standard deviation of z3
#' @param sigma_z4 Standard deviation of z4
#' @param sigma_x Standard deviation of x
#' @param y_aux_list A named list that contains all auxiliary parameters for the
#'                   rng function related to the data_family likelihood.
#' @param data_family RNG function or string identifier for the used likelihood
#'                    family to be looked up via \link{\code{rng_lookup}}.
#' @param data_link Link function or string identifier for the used link
#'                  function to be looked up via \link{\code{link_lookup}}.
#' @param lb Lower bound to truncate y values.
#' @param ub Upper bound to truncate y values.

#' @param oversample Ratio of oversampling to prevent multiple sampling rounds
#'                   when lb or ub are used.
#' @param testing_data True if a second dataset should be generated, eg. to be
#'                     used as testing data.
#' @param seed A random seed to make the generation deterministic.
#' @param ... Catch-all ellipses that is added to the output.
#'
#' @return A named list containing a dataset and collection of parameters and
#'         other outputs of interest in data_gen_output. Can also contain a
#'         testing_data dataset if testing_data is set to TRUE.
#' @export
#'
#' @import stats
#' @examples
#'
#' data <- basedag_data(data_N = 10,
#'              z1_x_coef = 1,
#'              z3_x_coef = 1,
#'              x_y_coef = 1,
#'              z1_y_coef = 1,
#'              z2_y_coef = 1,
#'              y_z4_coef = 1,
#'              x_z4_coef = 1,
#'              y_intercept = 1,
#'              sigma_z1 = 1,
#'              sigma_z2 = 1,
#'              sigma_z3 = 1,
#'              sigma_z4 = 1,
#'              sigma_x = 1,
#'              y_aux_list = list(sd = 2.5),
#'              "gaussian",
#'              "identity",
#'              oversample = 1.3,
#'              seed = NULL,
#'              testing_data = TRUE)
#' hist(data$dataset$y)
#' head(data$dataset)
#'
basedag_data <- function(data_N,
                         z1_x_coef,
                         z3_x_coef,
                         x_y_coef,
                         z1_y_coef,
                         z2_y_coef,
                         y_z4_coef,
                         x_z4_coef,
                         y_intercept,
                         sigma_z1,
                         sigma_z2,
                         sigma_z3,
                         sigma_z4,
                         sigma_x,
                         y_aux_list,
                         data_family,
                         data_link,
                         lb = -Inf,
                         ub = Inf,
                         oversample = 1.3,
                         seed = NULL,
                         testing_data = TRUE,
                         ...) {
  arguments <- as.list(c(as.list(environment()), list(...)))
  arguments$seed <- NULL

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (!is.function(data_link)){
    data_link <- bayesfam::link_lookup(data_link, inv = TRUE)
  }

  if (!is.function(data_family)){
    data_family <- bayesfam::rng_lookup(data_family)
  }

  dataset <- data.frame()
  iter <- 0
  if (testing_data) {
    data_gen_size <- data_N * 2
  } else {
    data_gen_size <- data_N
  }

  while (nrow(dataset) < data_gen_size) {
    z1 <- rnorm(n = oversample * data_gen_size, mean = 0, sd = sigma_z1)
    z2 <- rnorm(n = oversample * data_gen_size, mean = 0, sd = sigma_z2)
    z3 <- rnorm(n = oversample * data_gen_size, mean = 0, sd = sigma_z3)
    x <- rnorm(n = oversample * data_gen_size, mean = (z1_x_coef * z1 + z3_x_coef * z3), sd = sigma_x)

    mu <- do.call(
      data_link,
      list(
        y_intercept +
          x_y_coef * x +
          z1_y_coef * z1 +
          z2_y_coef * z2
      )
    )

    y <- do.call(
      data_family,
      c(
        list(
          n = length(mu),
          mu
      ),
        y_aux_list
      )
    )

    sanitized_index <- which(y < ub & y > lb)
    z1 <- z1[sanitized_index]
    z2 <- z2[sanitized_index]
    z3 <- z3[sanitized_index]
    x <- x[sanitized_index]
    y <- y[sanitized_index]

    z4 <- rnorm(n = length(y), mean = (y_z4_coef * y + x_z4_coef * x), sd = sigma_z4)

    dataset <- rbind(
      dataset,
      data.frame(
        z1 = z1[1:min((data_gen_size - nrow(dataset)), length(y))],
        z2 = z2[1:min((data_gen_size - nrow(dataset)), length(y))],
        z3 = z3[1:min((data_gen_size - nrow(dataset)), length(y))],
        z4 = z4[1:min((data_gen_size - nrow(dataset)), length(y))],
        x = x[1:min((data_gen_size - nrow(dataset)), length(y))],
        y = y[1:min((data_gen_size - nrow(dataset)), length(y))]
      )
    )
    iter <- iter + 1
    bad_samples <- (data_gen_size * oversample) - length(y)
  }

  data_gen_output <- list(
    sampling_loops = iter,
    bad_samples = bad_samples
  )
  data_gen_output <- c(data_gen_output, arguments)

  if (testing_data) {
    return(
      list(
        dataset = dataset[1:data_N, ],
        testing_data = dataset[(data_N + 1):data_gen_size, ],
        data_gen_output = data_gen_output
      )
    )
  } else {
    return(
      list(
        dataset = dataset,
        data_gen_output = data_gen_output
      )
    )
  }
}
