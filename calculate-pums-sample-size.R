library(tidyverse)
library(ipumsr)

# get PUMS microdata from IPUMS in order to populate Table 1 (sample size)
# (the PUMS is a sub-sample, with sample size only about 2/3 of overall ACS...
# ... so this is a loose lower bound on number of people responding to the ACS)

fornpersons.description <- '50 states + DC + PR PUMS data for # perons in sample'
fornpersons.sample.names <- expand_grid(
    year=c(2010:2019, 2021:2023),
    acs.vs.prcs=letters[1:2] # 'a' has 50 states plus DC, 'b' has Puerto Rico
  ) %>%
  mutate(comb = paste0('us', year, acs.vs.prcs)) %>%
  pull(comb)
fornpersons.variables=c(
  "YEAR", "SAMPLE", "SERIAL", "CBSERIAL", "HHWT", "CLUSTER", "STATEICP", "STRATA", "GQ", "PERNUM", "PERWT",
  "SEX", "AGE", "MARST", "RACE", "RACED", "HISPAN", "HISPAND", "BPL", "BPLD", "CITIZEN", "SPEAKENG", "EDUC", "EDUCD",
  "DIFFREM", "DIFFPHYS", "DIFFMOB", "DIFFCARE", "DIFFEYE", "DIFFHEAR"
)
fornpersons.extract <- define_extract_usa(
  description=fornpersons.description,
  samples=fornpersons.sample.names,
  fornpersons.variables
)
fornpersons.submitted_extract <- submit_extract(fornpersons.extract)
fornpersons.downloadable_extract <- wait_for_extract(fornpersons.submitted_extract)
fornpersons.data_path <- download_extract(fornpersons.downloadable_extract)
fornpersons.ipums_data <- read_ipums_micro(fornpersons.data_path)

saveRDS(fornpersons.ipums_data, 'fornpersons.ipums_data.RDS')

noninstitutionalized_working_age <- fornpersons.ipums_data %>%
  dplyr::filter(18 <= AGE & AGE < 65 & GQ != 3) %>%
  mutate(
    across(
      starts_with('DIFF'),
      ~ na_if(.x, 0),
      .names='diff_na_applied_{.col}'
    )
  ) %>%
  mutate(
    disab = if_any(starts_with('diff_na_applied'), ~ .x == 2),
    disab = if_else(disab, 2L, 1L, 0L),
    disab = haven::labelled(
      disab,
      c(
        `N/A`=0,
        `No disability`=1,
        `Has disability`=2
      )
    )
  )

noninstitutionalized_working_age_tab <- noninstitutionalized_working_age %>%
  select(YEAR, disab, starts_with('DIFF', ignore.case=FALSE)) %>%
  pivot_longer(cols=!YEAR) %>%
  count(YEAR, name, value) %>%
  arrange(desc(YEAR), name, value)

# only need disab and cog disab because reviewer asked to narrow focus of paper

noninstitutionalized_working_age %>% count(YEAR) %>% arrange(desc(YEAR)) %>%
  left_join(
    noninstitutionalized_working_age_tab %>%
      pivot_wider(
        names_from=c(value, name),
        values_from=n
      ) %>%
      select(
        YEAR,
        ends_with('disab'),
        ends_with('DIFFREM')
      )
  )


  



  
