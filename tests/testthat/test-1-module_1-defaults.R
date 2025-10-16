
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with defaults", {

  ## Run simInit and spades ----

  # Set up project
  projectName <- "1-defaults"

  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_defaults",
    paths   = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$modulePath,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
    )
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)

  expect_s4_class(simTest, "simList")


  ## Check outputs ----

  # CBMspecies
  expect_true(!is.null(simTest$CBMspecies))
  expect_true(inherits(simTest$CBMspecies, "data.table"))

  # disturbanceMatrix
  expect_true(!is.null(simTest$disturbanceMatrix))
  expect_true(inherits(simTest$disturbanceMatrix, "data.table"))

  # cTransfers
  expect_true(!is.null(simTest$cTransfers))
  expect_true(inherits(simTest$cTransfers, "data.table"))

  # spinupSQL
  expect_true(!is.null(simTest$spinupSQL))
  expect_true(inherits(simTest$spinupSQL, "data.table"))

  # pooldef
  expect_true(!is.null(simTest$pooldef))
  expect_true(inherits(simTest$pooldef, "character"))

})


