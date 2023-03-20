test_that("Test data_gen_conf_y_analysis", {
  data_gen_conf <- list(
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
    seed = 1234,
    oversample = 1.3
  )

  expect_no_error(data_gen_conf_y_analysis(data_gen_conf, basedag_data))
  # expect_output(data_gen_conf_y_analysis(data_gen_conf, basedag_data),
  #               paste("min:", min(dataset$y), "\n",
  #                     "max:", max(dataset$y), "\n",
  #                     "mean:", mean(dataset$y), "\n",
  #                     "median:", median(dataset$y), "\n",
#                     "bad_samples:", bad_samples))
})
