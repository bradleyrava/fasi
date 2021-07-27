test_that("fasi main function check", {
  z <- fasi::adult
  z <- z[sample(1:nrow(z), 1000),]
  ## Split data into observed and test
  obs_rows <- sort(sample(1:nrow(z), 0.5*nrow(z)))
  test_rows <- (1:nrow(z))[-obs_rows]
  observed_data <- z[obs_rows,]
  test_data <- z[test_rows, ]

  ## Split observed data into train and calibrate
  train_split_index <- sample(1:nrow(observed_data), 0.5*nrow(observed_data))
  calibrate_split_index <- (1:nrow(observed_data))[-train_split_index]
  train_data <- observed_data[train_split_index,]
  calibrate_data <- observed_data[calibrate_split_index,]

  ## FASI train and testing scores using NB
  calibrate_data$s <- sample(1:nrow(calibrate_data))
  test_data$s <- sample(1:nrow(test_data))

  fasi_fit <- fasi::fasi(calibrate_data, test_data,  0.1, 0.1, ptd_group_var="sex")

  expect_equivalent(c(0.1,0.1), fasi_fit$alpha)
  expect_equivalent("fasi", class(fasi_fit))
})
