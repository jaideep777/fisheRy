library(testthat)

params_file = here::here("params/cod_params.ini")

test_that("FishParams fields are writable and persistent", {
  fp <- new(FishParams)
  fp$c <- 123.45
  expect_equal(fp$c, 123.45)
  
  fp$beta1 <- 5.5
  expect_equal(fp$beta1, 5.5)
})

test_that("Fish fields are writable and persistent", {
  fish <- new(Fish, params_file)
  fish$age <- 10
  expect_equal(fish$age, 10)
  
  fish$length <- 25.0
  expect_equal(fish$length, 25.0)
  
  fish$par$gsi <- 2.2
  expect_equal(fish$par$gsi, 2.2)
})

test_that("SeaEnvironment fields are writable and persistent", {
  env1 <- new(SeaEnvironment)
  env1$temperature <- 14.5
  expect_equal(env1$temperature, 14.5)
})

test_that("PopulationParams fields are writable and persistent", {
  pp <- new(PopulationParams)
  pp$n <- 999
  expect_equal(pp$n, 999)
})

test_that("Population fields are writable and persistent", {
  fish <- new(Fish, params_file)
  pop <- new(Population, fish)
  
  pop$verbose <- TRUE
  expect_true(pop$verbose)
  
  pop$par$n <- 777
  expect_equal(pop$par$n, 777)
  
  pop$env$temperature <- 11.1
  expect_equal(pop$env$temperature, 11.1)
  
  # pop$init(100, 5.6)  # Functionality call
  # n_after = pop$nfish()
  # expect_equal(n_after, 100)
})

test_that("Fleet fields are writable and persistent", {
  fleet <- new(Fleet)
  fleet$chi <- 0.8
  expect_equal(fleet$chi, 0.8)
})

test_that("Fishery pop subfields are writable and persistent", {
  fish <- new(Fish, params_file)
  fishery <- new(Fishery, params_file, fish)
  
  fishery$init(100, 5.6)
  expect_equal(fishery$pop$nfish(), 100)
      
})

