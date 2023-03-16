test_that("Test 6n1t1o2a1f1c Functions", {
  arguments <- list(
    data_N = 1000,
    z1_x_coef = 1,
    z3_x_coef = 1,
    x_y_coef = 1,
    z1_y_coef = 1,
    z2_y_coef = 1,
    y_z4_coef = 1,
    x_z4_coef = 1,
    y_intercept = 1,
    sigma_z1 = 1,
    sigma_z2 = 1,
    sigma_z3 = 1,
    sigma_z4 = 1,
    sigma_x = 1,
    y_aux_list = list(sd = 2.5),
    data_family = "gaussian",
    data_link = "identity",
    lb = -Inf,
    ub = Inf,
    oversample = 1.3,
    seed = 1234,
    testing_data = TRUE
  )

  data <- do.call(basedag_data, arguments)

  expect_equal(mean(data$dataset$y), 1.188203447)
  expect_equal(sd(data$dataset$y), 3.65360708)
  expect_equal(mean(data$testing_data$y), 0.9928523315)
  expect_equal(sd(data$testing_data$y), 3.551646468)
  expect_equal(nrow(data$dataset), arguments$data_N)
  expect_equal(nrow(data$testing_data), arguments$data_N)

  expect_equal(names(data), c("dataset", "testing_data", "data_gen_output"))
  expect_equal(data$data_gen_output[sort(names(data$data_gen_output))], c(arguments, list("bad_samples" = 0, "sampling_loops" = 1))[sort(names(data$data_gen_output))])

  arguments <- list(
    data_N = 1000,
    z1_x_coef = 1,
    z3_x_coef = 1,
    x_y_coef = 1,
    z1_y_coef = 1,
    z2_y_coef = 1,
    y_z4_coef = 1,
    x_z4_coef = 1,
    y_intercept = 1,
    sigma_z1 = 1,
    sigma_z2 = 1,
    sigma_z3 = 1,
    sigma_z4 = 1,
    sigma_x = 1,
    y_aux_list = list(sd = 2.5),
    data_family = rnorm,
    data_link = function(x) x,
    lb = -Inf,
    ub = Inf,
    oversample = 1.3,
    testing_data = FALSE
  )

  data <- do.call(basedag_data, arguments)
  expect_false("seed" %in% names(data$data_gen_output))
  expect_false("testing_data" %in% names(data))

  expect_equal(nrow(data$dataset), arguments$data_N)
})
