# This file update the modules into the package

# source template
source(here::here("data-raw", "shinyTemplate.R"))

# source modules
source(here::here("data-raw", "modules", "moduleBackground.R"))
source(here::here("data-raw", "modules", "moduleIncidence.R"))
source(here::here("data-raw", "modules", "moduleCharacteristics.R"))

# relation between modules and results_type
relation <- readr::read_csv(
  here::here("data-raw", "relation.csv"),
  col_types = readr::cols(.default = "c")
)

# internal datasets to add to the package
usethis::use_data(
  shinyTemplate,
  moduleBackground,
  moduleIncidence,
  moduleCharacteristics,
  relation,
  internal = TRUE, overwrite = TRUE
)

#' mock datasets
mockIncidence <- readr::read_csv(
  here::here("data-raw", "mockResults", "mockIncidence.csv"),
  col_types = readr::cols(.default = "c")
)
usethis::use_data(mockIncidence, overwrite = TRUE)

mockCharacteristics <- readr::read_csv(
  here::here("data-raw", "mockResults", "mockCharacteristics.csv"),
  col_types = readr::cols(.default = "c")
)
usethis::use_data(mockCharacteristics, overwrite = TRUE)
