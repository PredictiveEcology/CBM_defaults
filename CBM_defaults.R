defineModule(sim, list(
  name = "CBM_defaults",
  description = "Provides CBM-CFS3 defaults for Canada",
  keywords = c("CBM-CFS3", "forest carbon", "Canada parameters"),
  authors = c(
    person("Céline",  "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb"))
  ),
  childModules = character(0),
  version = list(CBM_defaults = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CBM_defaults.Rmd"),
  reqdPkgs = list("reproducible", "RSQLite", "data.table"),
  parameters = bindrows(
    defineParameter(".useCache", "logical", FALSE, NA, NA, NA)
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "dbPath", objectClass = "character",
                 desc = "Path to the CBM defaults databse",
                 sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db"),
    expectsInput(objectName = "dbPathURL", objectClass = "character",
                 desc = "URL for dbPath"),
    expectsInput(objectName = "dMatrixAssociation", objectClass = "data.frame",
                 desc = "Disturbance table matching different disturbance IDs",
                 sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_exn/disturbance_matrix_association.csv"),
    expectsInput( objectName = "dMatrixAssociationURL", objectClass = "character",
      desc = "URL for dMatrixAssociation"),
    expectsInput(objectName = "dMatrixValue", objectClass = "data.frame",
                 desc = "Carbon transfer values table with associated disturbance IDs",
                 sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_exn/disturbance_matrix_value.csv"),
    expectsInput( objectName = "dMatrixValueURL", objectClass = "character",
                  desc = "URL for dMatrixValue")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "CBMspecies",        objectClass = "data.table",
                  desc = "Species table with names and associated species id"),
    createsOutput(objectName = "disturbanceMatrix", objectClass = "data.table",
                  desc = "Disturbance table with disturbance names associated with disturbance_matrix_id, disturbance_type_id"),
    createsOutput(objectName = "cTransfers",        objectClass = "data.table",
                  desc = "Carbon transfer values table with associated disturbance names and IDs"),
    createsOutput(objectName = "spinupSQL",         objectClass = "data.table",
                  desc = "Table containing many necesary spinup parameters used in CBM_core"),
    createsOutput(objectName = "pooldef",           objectClass = "character",
                  desc = "Vector of names for each of the carbon pools"),
    createsOutput(objectName = "cbmAdmin",        objectClass = "data.table",
                  desc = "Administrative boundaries with their associated ecozones and spatial unit IDs"),
  )
))

doEvent.CBM_defaults <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  ## Read the CBM-CFS3 Database ----

  # Connect to database
  archiveIndex <- dbConnect(dbDriver("SQLite"), sim$dbPath)
  on.exit(dbDisconnect(archiveIndex))

  # dbListTables(archiveIndex)
  # [1] "admin_boundary"                       "admin_boundary_tr"
  # [3] "afforestation_initial_pool"           "afforestation_pre_type"
  # [5] "afforestation_pre_type_tr"            "biomass_to_carbon_rate"
  # [7] "composite_flux_indicator"             "composite_flux_indicator_category"
  # [9] "composite_flux_indicator_category_tr" "composite_flux_indicator_tr"
  # [11] "composite_flux_indicator_value"       "decay_parameter"
  # [13] "disturbance_matrix"                   "disturbance_matrix_association"
  # [15] "disturbance_matrix_tr"                "disturbance_matrix_value"
  # [17] "disturbance_type"                     "disturbance_type_tr"
  # [19] "dom_pool"                             "eco_boundary"
  # [21] "eco_boundary_tr"                      "flux_indicator"
  # [23] "flux_indicator_sink"                  "flux_indicator_source"
  # [25] "flux_process"                         "forest_type"
  # [27] "forest_type_tr"                       "genus"
  # [29] "genus_tr"                             "growth_multiplier_series"
  # [31] "growth_multiplier_value"              "land_class"
  # [33] "land_class_tr"                        "land_type"
  # [35] "locale"                               "pool"
  # [37] "pool_tr"                              "random_return_interval"
  # [39] "root_parameter"                       "slow_mixing_rate"
  # [41] "spatial_unit"                         "species"
  # [43] "species_tr"                           "spinup_parameter"
  # [45] "stump_parameter"                      "turnover_parameter"
  # [47] "vol_to_bio_factor"                    "vol_to_bio_forest_type"
  # [49] "vol_to_bio_genus"                     "vol_to_bio_species"

  # This table, matrices6, has all the names associated with
  # disturbance_type_id. disturbance_type_id, along with spatial_unit_id and a
  # sw_hw flag is how libcbm connects (internally) to the proportions. In the
  # libcbm stepping function libcbmr::cbm_exn_step(), disturbance_type_id is in
  # cbm_vars$parameters$disturbance_type column and in the libcbm spinup
  # function libcbmr::cbm_exn_spinup(), the disturbance_type_id is passed in the
  # historical/lastpass values.
  matrices6 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_type_tr"))
  # matrices6 has French, Spanish and Russian disturbance translations, here we
  # only keep one copy in English.
  matrices6 <- matrices6[locale_id <= 1,]
  # only keep the colums we need, disturbance_type_id and its associated name.
  matrices6 <- matrices6[,.(disturbance_type_id, name, description)]
  # $disturbanceMatrix links together spatial_unit_id disturbance_type_id
  # disturbance_matrix_id, the disturbance names and descriptions.
  disturbanceMatrix <- sim$dMatrixAssociation[matrices6, on = .(disturbance_type_id = disturbance_type_id), allow.cartesian = TRUE]
  sim$disturbanceMatrix <- disturbanceMatrix

  # here we want to match sim$dMatrixValue with disturbanceMatrix so that sim$CTransfers has disturbance names.
  # CTransfers will have to be subset to the regions' spatial_unit_id.
  disturbanceNames <- unique(disturbanceMatrix[,.(disturbance_type_id, disturbance_matrix_id, name, description, spatial_unit_id)])
  cTransfers <- sim$dMatrixValue[disturbanceNames, on = .(disturbance_matrix_id = disturbance_matrix_id), allow.cartesian=TRUE]
  sim$cTransfers <- cTransfers

  # Get location and mean_annual_temperature for each spatial_unit, along with
  # other spinup parameters. These are needed to create sim$level3DT in
  # CBM_dataPrep_XX, which are in turn used in the CBM_core to create the
  # spinup_parameters in CBM_core.
  spatialUnitIds <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spatial_unit"))
  spinupParameter <-  as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spinup_parameter"))
  #create $spinupSQL for use in other modules
  sim$spinupSQL <- spatialUnitIds[spinupParameter, on = .(spinup_parameter_id = id)]

  #extract for pooldef
  pooldefURL <- "https://raw.githubusercontent.com/cat-cfs/libcbm_py/refs/heads/main/libcbm/resources/cbm_exn/pools.json"
  pooldef <- prepInputs(url = pooldefURL,
                        targetFile = "pools.json",
                        destinationPath = inputPath(sim),
                        fun = suppressWarnings(data.table::fread(targetFile)))
  sim$pooldef <- as.character(pooldef$V1)

  #extract species.csv
  CBMspeciesURL <- "https://raw.githubusercontent.com/cat-cfs/libcbm_py/refs/heads/main/libcbm/resources/cbm_exn/species.csv"
  sim$CBMspecies <- prepInputs(url = CBMspeciesURL,
                           targetFile = "species.csv",
                           destinationPath = inputPath(sim),
                           fun = fread)

  #build cbmAdmin
  spatialUnit <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spatial_unit"))
  spatialUnit <- spatialUnit[, .(id , admin_boundary_id , eco_boundary_id)]
  adminBoundary <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM admin_boundary_tr"))
  adminBoundary <- adminBoundary[locale_id <= 1,]
  adminBoundary[, locale_id := NULL]
  cbmAdmin <- spatialUnit[adminBoundary, on = .(admin_boundary_id = admin_boundary_id)]
  colNames <- c("SpatialUnitID", "AdminBoundaryID", "EcoBoundaryID", "stump_parameter_id", "adminName")
  setnames(cbmAdmin,names(cbmAdmin),
           colNames)
    cbmAdmin$abreviation <- c(
    "Newfoundland"              = "NL",
    "Labrador"                  = "NL",
    "Newfoundland and Labrador" = "NL",
    "Prince Edward Island"      = "PE",
    "Nova Scotia"               = "NS",
    "New Brunswick"             = "NB",
    "Quebec"                    = "QC",
    "Ontario"                   = "ON",
    "Manitoba"                  = "MB",
    "Alberta"                   = "AB",
    "Saskatchewan"              = "SK",
    "British Columbia"          = "BC",
    "Yukon"                     = "YT",
    "Yukon Territory"           = "YT",
    "Northwest Territories"     = "NT",
    "Nunavut"                   = "NU")[cbmAdmin$adminName]
    sim$cbmAdmin <- cbmAdmin


  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  # CBM-CFS3 Database
  if (!suppliedElsewhere("dbPath", sim)){
    if (suppliedElsewhere("dbPathURL", sim)){

      sim$dbPath <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$dbPathURL
      )

    }else{

      sim$dbPath <- file.path(inputPath(sim), "cbm_defaults_v1.2.8340.362.db")

      if (!file.exists(sim$dbPath)) prepInputs(
        destinationPath = inputPath(sim),
        url         = extractURL("dbPath"),
        targetFile  = basename(sim$dbPath),
        dlFun       = download.file(extractURL("dbPath"), sim$dbPath, mode = "wb", quiet = TRUE),
        fun         = NA
      )
    }
  }

  # Disturbance Matrix Association
  if (!suppliedElsewhere("dMatrixAssociation", sim)) {
    if (!suppliedElsewhere("dMatrixAssociationURL", sim)) {
      sim$dMatrixAssociationURL <- extractURL("dMatrixAssociation")
    }
    sim$dMatrixAssociation <- prepInputs(url = sim$dMatrixAssociationURL,
                               targetFile = "disturbance_matrix_association.csv",
                               destinationPath = inputPath(sim),
                               fun = fread
                               )
  }

  #Disturbance Matrix Value
  if (!suppliedElsewhere("dMatrixValue", sim)) {
    if (!suppliedElsewhere("dMatrixValueURL", sim)) {
      sim$dMatrixValueURL <- extractURL("dMatrixValue")
    }
    sim$dMatrixValue <- prepInputs(url = sim$dMatrixValueURL,
                                         targetFile = "disturbance_matrix_value.csv",
                                         destinationPath = inputPath(sim),
                                         fun = fread
    )
  }

  # Return sim
  return(invisible(sim))

}
