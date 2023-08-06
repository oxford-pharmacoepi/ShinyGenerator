library(IncidencePrevalence)
library(dplyr)
library(tidyr)
library(readr)

cdm <- mockIncidencePrevalenceRef(
  sampleSize = 50000,
  outPre = 0.5
)

cdm <- generateDenominatorCohortSet(
  cdm = cdm, name = "denominator",
  cohortDateRange = c(as.Date("2008-01-01"), as.Date("2012-01-01")),
  ageGroup = list(c(0, 150)),
  sex = "Both",
  daysPriorHistory = 0,
  temporary = FALSE,
)

inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  interval = "years",
  outcomeWashout = 0,
  repeatedEvents = FALSE,
  temporary = FALSE
)

inc %>%
  mutate(result_type = "Incidence estimates") %>%
  relocate(c("cdm_name", "result_type")) %>%
  write_csv(here::here("data-raw", "mockResults", "mockIncidence.csv"))

library(PatientProfiles)
library(DrugUtilisation)
cdm <- mockDrugUtilisation(numberIndividuals = 1000)
attr(cdm, "cdm_name") <- "mock dus"
cdm$cohort1 %>%
  summariseCharacteristics() %>%
  write_csv(here::here("data-raw", "mockResults", "mockCharacteristics.csv"))
